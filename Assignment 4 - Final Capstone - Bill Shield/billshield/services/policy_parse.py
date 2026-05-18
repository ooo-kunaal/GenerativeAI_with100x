"""Parse a health insurance policy PDF into a structured PolicySummary.

Public surface:
    parse_policy(pdf_path) -> PolicySummary
"""
from __future__ import annotations
from pathlib import Path
from typing import Optional, Callable
import re
import unicodedata
from . import ingest
from .llm import complete_json

_PROMPT_PATH = Path(__file__).resolve().parents[2] / "prompts" / "policy_parse.txt"
_PROMPT_TEMPLATE: str = _PROMPT_PATH.read_text(encoding="utf-8")

# Keywords that indicate policy-relevant paragraphs
_POLICY_KEYWORDS = [
    "room rent", "room-rent", "sub-limit", "sublimit", "sub limit",
    "icu", "iccu", "intensive care", "critical care",
    "co-pay", "copay", "co pay", "co-payment", "co payment",
    "deductible", "excess",
    "schedule of benefits", "benefit schedule", "benefits table",
    "maternity", "pre-existing", "pre existing", "waiting period",
    "cataract", "hernia", "knee", "hip replacement", "cabg", "bypass",
    "appendectomy", "hysterectomy", "tonsillectomy", "piles", "fistula",
    "exclusion", "not covered", "not payable", "not admissible",
    "sum insured", "coverage", "cap", "ceiling",
    "cashless", "reimbursement", "tpa",
    "employee base", "base policy", "family floater", "proportionate deduction",
    "ambulance", "day care", "daycare", "pre and post hospitalization",
]

# Phrases that strongly mark the primary employee/base policy block.
_BASE_POLICY_ANCHORS = (
    "employee base policy",
    "base policy",
    "coverage summary",
    "primary benefits",
    "family composition",
    "schedule of benefits",
)


def _sanitize(text: str) -> str:
    """Normalize unicode and replace pdfplumber's font-decode failures.

    Some PDFs (the GMC handbook in particular) emit U+FFFD replacement chars
    for en-dashes, em-dashes and bullets when their ToUnicode tables don't
    cover those glyphs. Replace them with a plain hyphen so the LLM doesn't
    see meaningless garbage.
    """
    if not text:
        return ""
    text = unicodedata.normalize("NFKC", text)
    text = text.replace("�", "-")
    # Squeeze long runs of identical punctuation that come from dot-leaders in TOCs.
    text = re.sub(r"\.{4,}", " ", text)
    return text


def _score_paragraph(para: str) -> int:
    p_low = para.lower()
    score = sum(1 for kw in _POLICY_KEYWORDS if kw in p_low)
    if re.search(r"(?:rs\.?|₹|inr)\s*[\d,]+", p_low):
        score += 2
    if re.search(r"\d+\s*%", p_low):
        score += 1
    if para.count("|") >= 2 or para.count("\t") >= 2:
        score += 1
    # Boost paragraphs that look like the primary base-policy block.
    if any(a in p_low for a in _BASE_POLICY_ANCHORS):
        score += 3
    return score


def _extract_relevant_sections(full_text: str, max_chars: int = 22_000) -> str:
    """Pick the most policy-relevant paragraphs, preserving document order.

    Why document order: the LLM extracts numeric parameters far more reliably
    when adjacent rows of a table stay adjacent. The old version sorted by
    descending score before joining, which scrambled tables and seeded the
    LLM with the cover page. This rewrite keeps the original sequence.
    """
    paragraphs = re.split(r"\n{2,}", full_text)
    if not paragraphs:
        return ""

    # Score every paragraph; remember its original index.
    scored: list[tuple[int, int, str]] = [
        (_score_paragraph(p), i, p) for i, p in enumerate(paragraphs)
    ]

    # If the whole doc fits, return it untouched (best signal for the LLM).
    total = sum(len(p) + 2 for p in paragraphs)
    if total <= max_chars:
        return "\n\n".join(paragraphs)

    # Otherwise, pick paragraphs in descending score order until we hit the
    # budget, then RESTORE document order before joining.
    picked: dict[int, str] = {}
    chars = 0
    for score, idx, para in sorted(scored, key=lambda x: (-x[0], x[1])):
        if score == 0:
            break
        cost = len(para) + 2
        if chars + cost > max_chars:
            continue
        picked[idx] = para
        chars += cost
        if chars >= max_chars:
            break

    if not picked:
        # Nothing scored — fall back to a truncated head so the LLM at least
        # sees the policy header.
        return full_text[:max_chars]

    return "\n\n".join(picked[i] for i in sorted(picked))


def parse_policy(
    pdf_path: str | Path,
    on_progress: Optional[Callable[[str], None]] = None,
) -> "PolicySummary":
    """Ingest the policy PDF via OCR/pdfplumber and extract structured parameters via LLM."""
    def _say(m: str):
        if on_progress: on_progress(m)

    pdf_path = Path(pdf_path)
    _say("Reading policy PDF…")
    ing = ingest.ingest(pdf_path, on_progress=_say)
    text = _sanitize(ing["text"])

    if len(text) > 22_000:
        _say(f"Policy is {len(text):,} chars - extracting relevant sections…")
        text = _extract_relevant_sections(text, max_chars=22_000)
        _say(f"Extracted {len(text):,} chars of policy-relevant content.")

    _say("Extracting policy parameters (LLM)…")
    prompt = _PROMPT_TEMPLATE.replace("{policy_text}", text)
    raw: dict = {}
    try:
        raw = complete_json(prompt, max_tokens=4000)
    except Exception as exc:
        _say(f"Policy LLM extraction failed: {exc}. Using minimal defaults.")
        from .models import PolicySummary
        return PolicySummary(confidence="low")

    # Coerce disease_sub_limits
    from .models import DiseaseSubLimit, PolicySummary
    disease_sub_limits = []
    for d in (raw.get("disease_sub_limits") or []):
        try:
            disease_sub_limits.append(DiseaseSubLimit(**d))
        except Exception:
            continue
    raw["disease_sub_limits"] = disease_sub_limits

    # Coerce None-like strings to actual None
    for key in (
        "room_rent_sub_limit_pct", "room_rent_sub_limit_abs", "icu_sub_limit_pct",
        "icu_sub_limit_abs", "co_pay_pct", "deductible_amount", "maternity_sub_limit",
        "opd_sub_limit", "pre_existing_waiting_days", "initial_waiting_days",
        "maternity_covered", "opd_covered", "room_rent_waiver", "cashless_available",
        "sum_insured", "ambulance_limit", "pre_hospitalization_days",
        "post_hospitalization_days", "room_type_description",
    ):
        v = raw.get(key)
        if isinstance(v, str) and v.lower() in ("null", "none", "n/a", ""):
            raw[key] = None

    try:
        return PolicySummary(**raw)
    except Exception as exc:
        _say(f"Policy model validation failed ({exc}). Using partial extraction.")
        safe_fields: dict = {}
        for field in (
            "insurer", "policy_name", "policy_type", "sum_insured",
            "room_rent_sub_limit_pct", "room_rent_sub_limit_abs", "room_rent_waiver",
            "room_type_description", "icu_sub_limit_pct", "icu_sub_limit_abs",
            "co_pay_pct", "deductible_amount", "maternity_covered", "opd_covered",
            "pre_existing_waiting_days", "initial_waiting_days",
            "pre_hospitalization_days", "post_hospitalization_days",
            "ambulance_limit", "exclusions_summary", "confidence",
        ):
            if field in raw:
                safe_fields[field] = raw[field]
        return PolicySummary(**safe_fields)
