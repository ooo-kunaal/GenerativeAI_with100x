"""End-to-end pipeline: PDF path -> BillAnalysis."""
from __future__ import annotations
import re
from pathlib import Path
from typing import Callable, Optional
from . import ingest, extract, classify, kb_lookup, flag_rules
from .reasonableness import check_reasonableness
from .models import (
    BillAnalysis, Hospital, Patient, Encounter, Totals,
    RawLineItem, Classification, AnalysedLineItem, Flag,
)

_CLASSIFY_BATCH = 40

# Looser regex - matches "Fortis Hospital", "FMC Healthcare Private Limited",
# "Remedy Hospital", "Manipal Hospital Sarjapur Road", etc. Title-case OK, suffix
# words alone OK if preceded by 1+ capitalized word.
_HOSPITAL_KEYWORDS = r"(?:HOSPITAL|HOSPITALS|HEALTHCARE|MEDICAL(?:\s+CENTRE|\s+CENTER|\s+STORE)?|CLINIC|NETRALAYA|SPECIALITY|PHARMACY|Hospital|Hospitals|Healthcare|Medical(?:\s+Centre|\s+Center|\s+Store)?|Clinic|Netralaya|Speciality|Pharmacy)"
_HOSPITAL_HEADER_RE = re.compile(
    rf"\b([A-Z][A-Za-z&.'\- ]{{2,60}}?\s+{_HOSPITAL_KEYWORDS}(?:\s+(?:Private|Pvt|Limited|Ltd))*)\b",
)

def _batched(seq, n):
    for i in range(0, len(seq), n):
        yield seq[i:i+n]

def _detect_hospitals(ocr_text: str) -> list[str]:
    hits = []
    seen = set()
    for m in _HOSPITAL_HEADER_RE.finditer(ocr_text):
        name = re.sub(r"\s+", " ", m.group(1)).strip()
        key = name.upper()
        if key not in seen and len(key) >= 8:
            seen.add(key)
            hits.append(name)
    return hits

_GSTIN_RE = re.compile(r"\b(\d{2}[A-Z]{5}\d{4}[A-Z][1-9A-Z]Z[\dA-Z])\b")

def _detect_distinct_gstins(ocr_text: str) -> list[str]:
    """Distinct GSTINs in the OCR text — used to catch stacked-bill PDFs even when
    the regex finds only one hospital name."""
    seen = []
    for m in _GSTIN_RE.finditer(ocr_text):
        g = m.group(1)
        if g not in seen: seen.append(g)
    return seen

def analyse(pdf_path: str | Path, on_progress: Optional[Callable[[str], None]] = None) -> BillAnalysis:
    def _report(msg: str):
        if on_progress: on_progress(msg)

    _report("Reading PDF & running OCR…")
    ing = ingest.ingest(pdf_path, on_progress=_report)

    _report("Extracting line items from bill text (LLM)…")
    raw = extract.extract(ing["text"])
    hospital = Hospital(**raw["hospital"])
    patient  = Patient(**raw["patient"])
    encounter = Encounter(**raw["encounter"])
    totals   = Totals(**raw["totals"])
    line_items = [RawLineItem(**li) for li in raw["line_items"]]
    unparsed   = raw.get("unparsed_lines", [])

    detected = _detect_hospitals(ing["text"])
    distinct_gstins = _detect_distinct_gstins(ing["text"])
    name_missing = (not hospital.name) or hospital.name.lower() == "unknown"
    if len(detected) >= 2 or len(distinct_gstins) >= 2:
        label_parts = detected[:3] if detected else [f"GSTIN {g}" for g in distinct_gstins[:3]]
        unparsed.append(
            f"⚠ Multiple billing entities detected in this PDF: {', '.join(label_parts)}. "
            f"The analysis below has been combined. The grievance letter will address: {hospital.name or (detected[0] if detected else label_parts[0])}."
        )
    if name_missing and detected:
        hospital = Hospital(**{**hospital.model_dump(), "name": detected[0]})
    elif name_missing and hospital.gstin:
        hospital = Hospital(**{**hospital.model_dump(), "name": f"Hospital [GSTIN {hospital.gstin}]"})
        unparsed.append(f"⚠ Could not identify hospital name from bill text. GSTIN found: {hospital.gstin}.")
    elif name_missing:
        unparsed.append("⚠ Could not identify hospital name from bill. Letter will need manual addressing.")

    if not line_items:
        return BillAnalysis(hospital=hospital, patient=patient, encounter=encounter,
                            totals=totals, items=[], unparsed_lines=unparsed)

    _report(f"Classifying {len(line_items)} line items (LLM)…")
    classifications_raw: list[dict] = []
    items_dicts = [li.model_dump() for li in line_items]
    for batch_idx, batch in enumerate(_batched(items_dicts, _CLASSIFY_BATCH), 1):
        _report(f"Classifying batch {batch_idx} ({len(batch)} items)…")
        try:
            classifications_raw.extend(classify.classify(batch))
        except Exception as exc:
            for it in batch:
                classifications_raw.append({
                    "id": it["id"], "category": "misc",
                    "canonical_name": it["raw_description"][:60] or "Unknown item",
                    "needs_benchmark": False, "confidence": "low",
                    "notes": f"classifier batch failed: {exc}",
                })
    cls_by_id = {c["id"]: Classification(**c) for c in classifications_raw}

    analysed: list[AnalysedLineItem] = []
    total = len(line_items)
    for i, li in enumerate(line_items, 1):
        if i == 1 or i % 10 == 0 or i == total:
            _report(f"Benchmarking item {i}/{total}…")
        cls = cls_by_id.get(li.id) or Classification(id=li.id, category="misc",
                                                     canonical_name=li.raw_description,
                                                     needs_benchmark=False, confidence="low")
        bench = kb_lookup.lookup(cls.model_dump(), li.raw_description, hospital.city_tier)
        flags = flag_rules.evaluate(li.model_dump(), cls.model_dump(), bench.model_dump())
        analysed.append(AnalysedLineItem(raw=li, classification=cls, benchmark=bench, flags=flags))

    grey_items = [it for it in analysed if any(f.kind in ("unbenchmarkable","ambiguous_mapping") for f in it.flags)]
    if grey_items:
        _report(f"AI reasonableness check on {len(grey_items)} unbenchmarked items…")
        items_for_check = [{
            "id": it.raw.id, "description": it.raw.raw_description,
            "category": it.classification.category, "amount": it.raw.amount,
            "quantity": it.raw.quantity, "unit_rate": it.raw.unit_rate,
        } for it in grey_items]
        try:
            assessments = check_reasonableness(items_for_check,
                hospital_name=hospital.name, city=hospital.city or "",
                nabh_status=hospital.nabh_status, encounter_type=encounter.type)
        except Exception as exc:
            assessments = {}
            unparsed.append(f"⚠ AI reasonableness check failed ({exc.__class__.__name__}): {len(grey_items)} items remain unverified.")
        if not assessments and grey_items:
            unparsed.append(f"⚠ AI reasonableness check returned no assessments for {len(grey_items)} unbenchmarked items.")
        for it in grey_items:
            assessment = assessments.get(it.raw.id)
            if not assessment: continue
            verdict = assessment.get("verdict", "reasonable")
            reasoning = assessment.get("reasoning", "")
            est_low = assessment.get("estimated_fair_range_low")
            est_high = assessment.get("estimated_fair_range_high")
            it.flags = [f for f in it.flags if f.kind not in ("unbenchmarkable","ambiguous_mapping")]
            if verdict == "reasonable":
                rng = f" (estimated fair range: ₹{est_low:,.0f}–₹{est_high:,.0f})" if est_low and est_high else ""
                it.flags.append(Flag(kind="ok", severity="green",
                    message=f"No government benchmark, but amount appears reasonable.{rng} {reasoning}",
                    billed_amount=it.raw.amount,
                    citation="AI reasonableness check (not a cited benchmark)"))
            elif verdict == "possibly_high":
                rng = f" Estimated fair range: ₹{est_low:,.0f}–₹{est_high:,.0f}." if est_low and est_high else ""
                it.flags.append(Flag(kind="overcharge", severity="amber",
                    message=f"No government benchmark, but amount may be above typical.{rng} {reasoning}",
                    billed_amount=it.raw.amount,
                    citation="AI reasonableness check (not a cited benchmark)"))
            elif verdict == "likely_high":
                rng = f" Estimated fair range: ₹{est_low:,.0f}–₹{est_high:,.0f}." if est_low and est_high else ""
                it.flags.append(Flag(kind="overcharge", severity="red",
                    message=f"No government benchmark, but amount appears significantly above typical.{rng} {reasoning}",
                    billed_amount=it.raw.amount,
                    citation="AI reasonableness check (not a cited benchmark)"))
            else:
                it.flags.append(Flag(kind="unbenchmarkable", severity="grey",
                    message=f"We don't have a benchmark for this charge of ₹{it.raw.amount:,.0f}. Ask the hospital for a breakdown.",
                    billed_amount=it.raw.amount))

    _report("Checking for duplicate charges…")
    flag_rules.detect_duplicates(analysed)

    # CRITICAL: set worst_severity for EACH item BEFORE rolling up totals
    for it in analysed:
        it.worst_severity = flag_rules.worst_severity(it.flags)  # type: ignore

    _report("Calculating totals…")
    sum_items = sum(it.raw.amount or 0 for it in analysed)
    extracted_net = float(totals.net_amount or totals.gross_amount or 0)
    if sum_items > 0 and (extracted_net == 0 or sum_items > extracted_net * 1.10):
        if extracted_net > 0:
            unparsed.append(
                f"ℹ The bill total used for this review is ₹{sum_items:,.0f}, the sum of all "
                f"line items. A different figure (₹{extracted_net:,.0f}) was also detected in "
                f"the document — likely a section subtotal."
            )
        totals = Totals(
            gross_amount=sum_items, discount=totals.discount or 0,
            tax=totals.tax or 0, net_amount=sum_items, currency=totals.currency or "INR",
        )

    # Roll up AFTER worst_severity is set, so AI-reasonableness greens count as fair
    fair = sum((it.raw.amount or 0) for it in analysed if it.worst_severity == "green")
    flagged = sum((it.raw.amount or 0) for it in analysed if it.worst_severity in ("amber","red"))
    unverif = sum((it.raw.amount or 0) for it in analysed if it.worst_severity == "grey")

    return BillAnalysis(
        hospital=hospital, patient=patient, encounter=encounter, totals=totals,
        items=analysed, unparsed_lines=unparsed,
        rupees_verified_fair=fair, rupees_flagged=flagged, rupees_unverifiable=unverif,
    )