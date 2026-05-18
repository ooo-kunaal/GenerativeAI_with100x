"""OCR-text -> structured bill JSON. Chunked extraction with garbage filtering."""
from __future__ import annotations
import re
from pathlib import Path
from .llm import complete_json
from .models import Hospital, Patient, Encounter, Totals, RawLineItem

_PROMPT_PATH = Path(__file__).resolve().parents[2] / "prompts" / "extract.txt"
_GARBAGE_RE = re.compile(r"[A-Za-z]{2,}")
_OCR_NOISE_TOKEN_RE = re.compile(r"[A-Z]{2,}\d|\d[A-Z]{2,}|[~`^]|\.{3,}")

def _is_garbage(desc: str) -> bool:
    """Reject OCR-noise descriptions."""
    if not desc: return True
    s = desc.strip()
    if len(s) < 4: return True
    # Need at least one 2+ letter run
    words = _GARBAGE_RE.findall(s)
    if not words: return True
    # Reject if > 30% non-alnum non-space chars
    alnum = sum(c.isalnum() or c.isspace() for c in s)
    if alnum / max(len(s), 1) < 0.70: return True
    # Reject if 2+ OCR-noise tokens (mixed letter-digit clusters, tildes, dots)
    if len(_OCR_NOISE_TOKEN_RE.findall(s)) >= 2: return True
    return False

_SECTION_RE = re.compile(r"^\s*\d+\s+[A-Z][A-Za-z &./()-]{2,40}\s*$", re.MULTILINE)

def _chunk_text(ocr_text: str, max_lines_per_chunk: int = 60) -> list[str]:
    # Small bills: always one chunk. Loses column headers when split.
    if len(ocr_text) < 8000: return [ocr_text]
    lines = ocr_text.splitlines()
    if len(lines) <= max_lines_per_chunk: return [ocr_text]
    boundaries = [i for i, ln in enumerate(lines) if _SECTION_RE.match(ln)]
    if not boundaries or len(boundaries) < 2:
        return ["\n".join(lines[i:i+max_lines_per_chunk]) for i in range(0, len(lines), max_lines_per_chunk)]
    chunks: list[str] = []
    buf: list[str] = []
    for i, ln in enumerate(lines):
        if i in boundaries and len(buf) > max_lines_per_chunk // 2:
            chunks.append("\n".join(buf)); buf = []
        buf.append(ln)
    if buf: chunks.append("\n".join(buf))
    return chunks

def extract(ocr_text: str) -> dict:
    import json
    tpl = _PROMPT_PATH.read_text(encoding="utf-8")
    chunks = _chunk_text(ocr_text)
    # print(f"\n===== EXTRACT DEBUG =====", flush=True)
    # print(f"Total OCR length: {len(ocr_text)} chars, split into {len(chunks)} chunks", flush=True)
    # for i, ch in enumerate(chunks):
    #     print(f"  chunk {i}: {len(ch)} chars", flush=True)

    first_prompt = tpl.replace("{ocr_text}", chunks[0])
    first_data = complete_json(first_prompt, max_tokens=16000)
    # print(f"\nCHUNK 0 line_items count: {len(first_data.get('line_items', []))}", flush=True)
    # print(f"CHUNK 0 line_items:", flush=True)
    # print(json.dumps(first_data.get("line_items", []), indent=2, ensure_ascii=False), flush=True)

    hospital = first_data.get("hospital", {})
    patient  = first_data.get("patient", {"name_redacted":"Unknown"})
    encounter = first_data.get("encounter", {})
    totals   = first_data.get("totals", {})
    all_line_items = list(first_data.get("line_items", []))
    all_unparsed   = list(first_data.get("unparsed_lines", []))

    if len(chunks) > 1:
        for i, ch in enumerate(chunks[1:], 2):
            try:
                sub = complete_json(tpl.replace("{ocr_text}", ch), max_tokens=12000)
                items = sub.get("line_items", [])
                print(f"CHUNK {i-1} line_items count: {len(items)}", flush=True)
                print(f"CHUNK {i-1} line_items: {json.dumps(items, indent=2, ensure_ascii=False)}", flush=True)
                base = len(all_line_items)
                for j, it in enumerate(items):
                    it["id"] = f"li_{base + j + 1}"
                all_line_items.extend(items)
                all_unparsed.extend(sub.get("unparsed_lines", []))
                # If subsequent chunk has better hospital info (i.e. first chunk failed), use it
                sub_hosp = sub.get("hospital", {})
                if not hospital.get("name") and sub_hosp.get("name"):
                    hospital = sub_hosp
            except Exception as exc:
                all_unparsed.append(f"[chunk {i} extract failed: {exc}]")

    cleaned: list[dict] = []
    for li in all_line_items:
        desc = li.get("raw_description") or ""
        if _is_garbage(desc):
            if desc: all_unparsed.append(desc)
            continue
        # Tier 4 extra: drop small "unspecified" items (under ₹100) entirely
        amt = float(li.get("amount") or 0)
        if amt < 100 and "unspecified" in (desc.lower() + (li.get("notes","") or "").lower()):
            all_unparsed.append(desc); continue
        cleaned.append(li)

    all_unparsed = [str(x) for x in all_unparsed if x is not None and str(x).strip()]
    # print(f"\nTOTAL line_items collected: {len(all_line_items)}", flush=True)
    # print(f"===== END EXTRACT DEBUG =====\n", flush=True)
    return {
        "hospital":  Hospital(**hospital).model_dump(),
        "patient":   Patient(**patient).model_dump(),
        "encounter": Encounter(**encounter).model_dump(),
        "totals":    Totals(**totals).model_dump(),
        "line_items": [RawLineItem(**li).model_dump() for li in cleaned],
        "unparsed_lines": all_unparsed,
    }