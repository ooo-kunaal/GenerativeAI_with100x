"""Apply the flag taxonomy to an analysed line item.

User-facing flags:
  - overcharge:           billed > benchmark * 1.10
  - drug_mrp_violation:   drug billed above NPPA ceiling OR > 5x Jan Aushadhi
  - duplicate:            same canonical_name in multiple line items
  - unbenchmarkable:      no benchmark exists for this item (plain-language message)
  - ambiguous_mapping:    fuzzy match below threshold
  - ok:                   green; within tolerance

Severity:
  green = ok (within +10% of benchmark)
  amber = 10-50% over benchmark
  red   = >50% over OR drug_mrp_violation OR duplicate
  grey  = unbenchmarkable / ambiguous_mapping

Note: stale_benchmark used to be a user-facing flag. It is no longer added.
The user does not need to see 'benchmark may be outdated' — it adds doubt
without giving them any actionable signal. Source date is still tracked
internally on the Benchmark object for citations."""
from __future__ import annotations
from collections import Counter
from .models import Flag, AnalysedLineItem, RawLineItem, Classification, Benchmark

AMBER_THRESHOLD = 1.10
RED_THRESHOLD   = 1.50
JAN_AUSHADHI_MULTIPLE = 5.0
ROOM_AMBER_THRESHOLD = 1.25  # rooms get more leeway (NABH/equipment/category variance)
ROOM_RED_THRESHOLD   = 1.75

# Substring fragments (lowercase) that indicate legitimately-per-day or per-event
# items. Substring matching collapses classifier variants ("Doctor Visit - IPD" vs
# "Doctor Visit Charge - IPD" vs "Glucose Test - Capillary Blood (CBG)" vs
# "Capillary Blood Glucose (CBG) Measurement").
DUPLICATABLE_FRAGMENTS = (
    "injection administration", "iv cannulation", "vitals checking",
    "blood sample collection", "random blood sugar", "rbs", "grbs",
    "sugar test by glucometer", "capillary blood glucose", "glucose test",
    "arterial blood gas", "abg sampling",
    "consultation - ipd", "doctor visit", "junior doctor",
    "patient linen", "laundry charges", "biomedical waste",
    "ward sundry", "diet package", "nursing charges",
    "dressing charges", "surgical dressing", "oxygen",
)

# Canonical names that are too generic to count for duplicates
_FALLBACK_CANONICALS = {"unclear item","misc","miscellaneous","unknown","item"}

def _delta(billed: float, bench: float) -> tuple[float, float]:
    delta = billed - bench
    pct = (delta / bench * 100.0) if bench else 0.0
    return delta, pct

def _severity_from_ratio(ratio: float) -> str:
    if ratio <= AMBER_THRESHOLD: return "green"
    if ratio <= RED_THRESHOLD:   return "amber"
    return "red"

def evaluate(raw: dict, classification: dict, benchmark: dict, unit_qty: float = 1.0) -> list[Flag]:
    """Return flags for one line item. Caller will set worst_severity from these."""
    flags: list[Flag] = []
    billed = float(raw.get("amount") or 0.0)
    unit_rate = raw.get("unit_rate")
    qty = float(raw.get("quantity") or 1.0)
    # Defensive: if unit_rate * qty wildly disagrees with billed, prefer the larger
    # (the "Amount" column is rarely under-stated; under-statement usually means
    # extract picked the unit_rate as billed).
    if unit_rate and qty > 0:
        implied = float(unit_rate) * qty
        if billed > 0 and implied > billed * 1.5:
            billed = implied
        # Recover qty when billed >> unit_rate * stated_qty (extract missed multiplicity)
        elif billed > float(unit_rate) * qty * 1.5 and float(unit_rate) > 0:
            inferred_qty = round(billed / float(unit_rate))
            if inferred_qty >= 2:
                qty = float(inferred_qty)
    bench_amt = benchmark.get("benchmark_amount")
    matched   = benchmark.get("matched", False)
    citation = None
    cit_om = benchmark.get("citation_om")
    cit_dt = benchmark.get("citation_date")
    if cit_om: citation = f"{cit_om}" + (f" dated {cit_dt}" if cit_dt else "")

    # Ambiguous mapping?
    if not matched and benchmark.get("match_method") == "none" and benchmark.get("candidates"):
        cands = benchmark.get("candidates") or []
        top = ", ".join(f"'{c.get('name')}'" for c in cands[:3])
        flags.append(Flag(
            kind="ambiguous_mapping", severity="grey",
            message=(f"We weren't sure which government benchmark to apply here. Closest matches considered: {top}." if top
                     else "We couldn't confidently match this line to a government benchmark."),
            billed_amount=billed,
        ))
        return flags

    # Unbenchmarkable (mapping known to be no-benchmark, OR no match at all)
    if not matched or bench_amt is None:
        canon = (classification.get("canonical_name") or "this item").strip()
        cat = classification.get("category") or "item"
        raw_notes = (benchmark.get("notes") or "").strip()
        # Only surface notes that read like a user-facing reason
        user_safe = raw_notes if raw_notes.lower().startswith(("no ","not ","pre-","non-","tbd","bundled","included")) else ""
        reason = user_safe or "we don't have a government benchmark for this kind of charge yet — ask the hospital for a rate-card breakdown."
        flags.append(Flag(
            kind="unbenchmarkable", severity="grey",
            message=f"This is a '{canon}' ({cat}) charge of ₹{billed:,.0f}. {reason.rstrip('.').capitalize()}.",
            billed_amount=billed,
        ))
        return flags

    # Drug MRP / Jan Aushadhi
    source_table = benchmark.get("source_table")
    if source_table == "nppa_drugs":
        per_unit_billed = (unit_rate if unit_rate else billed / max(qty,1.0))
        if per_unit_billed > bench_amt:
            d, p = _delta(per_unit_billed, bench_amt)
            flags.append(Flag(
                kind="drug_mrp_violation", severity="red",
                message=f"Per-unit billed ₹{per_unit_billed:.2f} exceeds NPPA ceiling ₹{bench_amt:.2f}.",
                billed_amount=billed, benchmark_amount=bench_amt * qty,
                delta_amount=d * qty, delta_pct=p, citation=citation,
            ))
            return flags

    if source_table == "jan_aushadhi":
        per_unit_billed = (unit_rate if unit_rate else billed / max(qty,1.0))
        if per_unit_billed > bench_amt * JAN_AUSHADHI_MULTIPLE:
            d, p = _delta(per_unit_billed, bench_amt)
            flags.append(Flag(
                kind="drug_mrp_violation", severity="red",
                message=f"Per-unit ₹{per_unit_billed:.2f} is >{JAN_AUSHADHI_MULTIPLE:.0f}× Jan Aushadhi generic ₹{bench_amt:.2f}.",
                billed_amount=billed, benchmark_amount=bench_amt * qty,
                delta_amount=d * qty, delta_pct=p, citation=citation,
            ))
            return flags

    # Standard procedure/diagnostic overcharge
    expected = bench_amt * qty
    ratio = billed / expected if expected else 0.0
    cat = classification.get("category")
    if cat == "room":
        if ratio <= ROOM_AMBER_THRESHOLD: sev = "green"
        elif ratio <= ROOM_RED_THRESHOLD: sev = "amber"
        else: sev = "red"
    else:
        sev = _severity_from_ratio(ratio)
    
    if sev == "green":
        flags.append(Flag(
            kind="ok", severity="green",
            message=f"Within tolerance of benchmark (₹{expected:.0f}).",
            billed_amount=billed, benchmark_amount=expected, citation=citation,
        ))
    else:
        d, p = _delta(billed, expected)
        flags.append(Flag(
            kind="overcharge", severity=sev,
            message=f"Billed ₹{billed:.0f} vs benchmark ₹{expected:.0f} ({p:+.0f}%).",
            billed_amount=billed, benchmark_amount=expected,
            delta_amount=d, delta_pct=p, citation=citation,
        ))
    return flags

def detect_duplicates(items: list[AnalysedLineItem]) -> None:
    """Mutates items in place: adds a 'duplicate' flag where the same canonical_name
    appears in 2+ line items and isn't a legitimately-per-day item."""
    counts: Counter[str] = Counter()
    for it in items:
        canon = it.classification.canonical_name
        if canon and canon.lower() not in _FALLBACK_CANONICALS:
            counts[canon] += 1
    for it in items:
        canon = it.classification.canonical_name
        if not canon or counts[canon] <= 1: continue
        canon_lower = canon.lower()
        if any(frag in canon_lower for frag in DUPLICATABLE_FRAGMENTS): continue
        it.flags.append(Flag(
            kind="duplicate", severity="red",
            message=f"'{canon}' appears in {counts[canon]} line items — confirm this isn't double-billing.",
            billed_amount=float(it.raw.amount or 0.0),
        ))

_SEVERITY_RANK = {"red":3, "amber":2, "grey":1, "green":0}

def worst_severity(flags: list[Flag]) -> str:
    if not flags: return "grey"
    return max(flags, key=lambda f: _SEVERITY_RANK.get(f.severity, 0)).severity