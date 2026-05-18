"""Insurance claim settlement estimation engine.

Given a BillAnalysis and a PolicySummary (plus user-declared SI / top-up),
applies Indian health insurance claim rules step-by-step and returns a
ClaimEstimate with a full deduction breakdown.

Estimation steps (in order):
  1. Identify IRDAI non-payable items → deduct them entirely
  2. Identify room-linked items; if room billed > room sub-limit, apply
     proportional cascade to all room-linked charges
  3. Apply ICU sub-limit
  4. Apply disease-specific sub-limits
  5. Apply fixed deductible (if any)
  6. Apply co-pay percentage
  7. Cap at sum insured; if top-up provided, check whether it kicks in
  8. Generate tips and warnings
"""
from __future__ import annotations
import re
from functools import lru_cache
from pathlib import Path
from typing import Optional
import pandas as pd
from rapidfuzz import fuzz, process as fuzz_process

from .models import (
    BillAnalysis, PolicySummary, ClaimEstimate, ClaimDeduction, AnalysedLineItem
)

_KB_DIR = Path(__file__).resolve().parents[2] / "kb"
_NON_PAYABLE_CSV = _KB_DIR / "irdai_non_payables.csv"

# Categories of bill items that are "room-linked" and subject to proportional
# reduction when room rent exceeds the sub-limit.
_ROOM_LINKED_CATEGORIES = {"room", "consultation", "anaesthesia"}
_ROOM_LINKED_KEYWORDS = (
    "room", "bed", "nursing", "ward", "doctor visit", "consultation",
    "attendant", "diet", "patient diet", "visiting charges",
)

_NON_PAYABLE_SCORE_THRESHOLD = 85  # rapidfuzz threshold — keep high to avoid false positives

# Categories that are NEVER non-payable (clinical/treatment items)
_PROTECTED_CATEGORIES = {"drug", "procedure", "diagnostic", "package", "surgery", "anaesthesia"}


@lru_cache(maxsize=1)
def _load_non_payables() -> list[str]:
    """Return list of non-payable item names from IRDAI KB."""
    df = pd.read_csv(_NON_PAYABLE_CSV)
    return df["item_name"].astype(str).str.strip().tolist()


def _is_non_payable(description: str, category: str) -> tuple[bool, str]:
    """Return (is_non_payable, matched_rule_name).
    Only matches on raw description (not canonical) to reduce false positives.
    Protected categories (drugs, procedures, diagnostics) are never flagged.
    """
    if (category or "").lower() in _PROTECTED_CATEGORIES:
        return False, ""
    non_payables = _load_non_payables()
    result = fuzz_process.extractOne(
        description, non_payables, scorer=fuzz.token_set_ratio
    )
    if result is not None and result[1] >= _NON_PAYABLE_SCORE_THRESHOLD:
        return True, result[0]
    return False, ""


def _is_icu_item(description: str, canon: str) -> bool:
    combined = (description + " " + canon).lower()
    return any(kw in combined for kw in ("icu", "iccu", "intensive care", "critical care"))


def _is_room_linked(item: AnalysedLineItem) -> bool:
    cat = (item.classification.category or "").lower()
    if cat in _ROOM_LINKED_CATEGORIES:
        return True
    desc = (item.raw.raw_description or "").lower()
    canon = (item.classification.canonical_name or "").lower()
    return any(kw in desc or kw in canon for kw in _ROOM_LINKED_KEYWORDS)


def _find_room_rent_item(items: list[AnalysedLineItem]) -> Optional[AnalysedLineItem]:
    """Find the primary room/bed charge item."""
    for it in items:
        cat = (it.classification.category or "").lower()
        desc = (it.raw.raw_description or "").lower()
        canon = (it.classification.canonical_name or "").lower()
        if cat == "room":
            return it
        if any(kw in desc or kw in canon for kw in ("room rent", "bed charge", "room charge", "ward charge", "bed rent")):
            return it
    return None


def _find_disease_sub_limit(
    item: AnalysedLineItem,
    policy: PolicySummary,
) -> Optional[float]:
    """Return the disease sub-limit cap (₹) if this item matches a disease sub-limit."""
    if not policy.disease_sub_limits:
        return None
    desc = (item.raw.raw_description or "").lower()
    canon = (item.classification.canonical_name or "").lower()
    query = f"{desc} {canon}"
    disease_names = [d.disease for d in policy.disease_sub_limits]
    result = fuzz_process.extractOne(query, disease_names, scorer=fuzz.token_set_ratio)
    if result and result[1] >= 65:
        matched = policy.disease_sub_limits[disease_names.index(result[0])]
        return matched.cap_amount
    return None


def _fmt(v: float) -> str:
    return f"₹{v:,.0f}"


def estimate_claim(
    analysis: BillAnalysis,
    policy: PolicySummary,
    sum_insured: float,
    top_up_limit: float = 0.0,
    policy_was_uploaded: bool = False,
) -> ClaimEstimate:
    """Run the full claim estimation pipeline and return a ClaimEstimate."""
    items = analysis.items
    # Use item-level sum as the authoritative gross_billed so the deduction
    # waterfall always adds up: gross - deductions = settlement.
    items_sum = sum(float(it.raw.amount or 0) for it in items)
    totals_amount = float(analysis.totals.net_amount or analysis.totals.gross_amount or 0)
    # Prefer items_sum when it is positive; fall back to totals header.
    gross_billed = items_sum if items_sum > 0 else totals_amount

    deductions: list[ClaimDeduction] = []
    # Working copy: {item_id -> admissible_amount} — starts at billed amount
    admissible: dict[str, float] = {
        it.raw.id: float(it.raw.amount or 0) for it in items
    }

    # ── Step 1: IRDAI Non-Payable Items ────────────────────────────────────
    non_payable_total = 0.0
    non_payable_names: list[str] = []
    for it in items:
        desc = (it.raw.raw_description or "").strip()
        cat = (it.classification.category or "").lower()
        flagged, matched_rule = _is_non_payable(desc, cat)
        if flagged:
            amt = admissible.get(it.raw.id, 0.0)
            if amt > 0:
                non_payable_total += amt
                label = f"{desc[:35]} → '{matched_rule}' ({_fmt(amt)})"
                non_payable_names.append(label)
                admissible[it.raw.id] = 0.0

    if non_payable_total > 0:
        deductions.append(ClaimDeduction(
            kind="non_payable",
            label="IRDAI Non-Payable Items",
            amount=non_payable_total,
            explanation=(
                "These items are on the IRDAI standard non-payable list (Circular "
                "IRDAI/HLT/CIR/MISC/253/11/2019) and must be excluded by all insurers."
            ),
            items_affected=non_payable_names[:10],
        ))

    # ── Step 2: Room Rent Proportional Cascade ─────────────────────────────
    room_rent_billed = 0.0
    room_rent_allowed = 0.0
    cascade_ratio = 1.0
    room_cascade_deduction = 0.0
    room_item = _find_room_rent_item(items)

    if room_item and not policy.room_rent_waiver:
        room_rent_billed = float(room_item.raw.amount or 0)
        los = float(analysis.encounter.length_of_stay_days or 1)

        # Compute the sub-limit ceiling per day
        daily_billed = room_rent_billed / los if los > 0 else room_rent_billed
        daily_allowed: Optional[float] = None

        if policy.room_rent_sub_limit_abs:
            daily_allowed = policy.room_rent_sub_limit_abs
        elif policy.room_rent_sub_limit_pct:
            daily_allowed = sum_insured * (policy.room_rent_sub_limit_pct / 100.0)

        if daily_allowed is not None and daily_billed > daily_allowed * 1.01:
            # Cascade applies
            total_allowed = daily_allowed * los
            room_rent_allowed = total_allowed
            cascade_ratio = total_allowed / room_rent_billed  # e.g. 0.4 if 40% allowed

            # Deduct the excess room rent itself
            room_excess = room_rent_billed - total_allowed
            admissible[room_item.raw.id] = total_allowed

            # Apply cascade to all room-linked items (except ICU, procedures, drugs)
            cascade_names: list[str] = [
                f"Room charge ({_fmt(room_rent_billed)} → {_fmt(total_allowed)})"
            ]
            cascade_deduct = room_excess

            for it in items:
                if it.raw.id == room_item.raw.id:
                    continue
                if not _is_room_linked(it):
                    continue
                cat = (it.classification.category or "").lower()
                # Never cascade on procedures, drugs, diagnostics
                if cat in ("procedure","drug","diagnostic","consumable","package","anaesthesia"):
                    continue
                billed_amt = admissible.get(it.raw.id, 0.0)
                if billed_amt <= 0:
                    continue
                proportional_allowed = billed_amt * cascade_ratio
                excess = billed_amt - proportional_allowed
                cascade_deduct += excess
                admissible[it.raw.id] = proportional_allowed
                name = (it.classification.canonical_name or it.raw.raw_description)[:40]
                cascade_names.append(f"{name} ({_fmt(billed_amt)} → {_fmt(proportional_allowed)})")

            room_cascade_deduction = cascade_deduct
            if cascade_deduct > 0:
                deductions.append(ClaimDeduction(
                    kind="room_rent_cascade",
                    label="Room Rent Sub-Limit Proportional Reduction",
                    amount=cascade_deduct,
                    explanation=(
                        f"Your policy allows a room rent of {_fmt(daily_allowed)}/day "
                        f"({_fmt(total_allowed)} for {int(los)} night(s)). "
                        f"You were billed {_fmt(room_rent_billed)}. "
                        f"Under IRDAI guidelines, all room-linked charges are reduced "
                        f"proportionally by the same ratio ({cascade_ratio*100:.0f}%). "
                        f"This cascade applies to doctor visits, nursing, and ward charges "
                        f"but NOT to procedures, drugs, or diagnostics."
                    ),
                    items_affected=cascade_names[:8],
                ))
        else:
            room_rent_allowed = room_rent_billed  # no cascade

    # ── Step 3: ICU Sub-Limit ──────────────────────────────────────────────
    icu_deduction = 0.0
    icu_names: list[str] = []
    icu_daily_allowed: Optional[float] = None
    los = float(analysis.encounter.length_of_stay_days or 1)

    if policy.icu_sub_limit_abs:
        icu_daily_allowed = policy.icu_sub_limit_abs
    elif policy.icu_sub_limit_pct:
        icu_daily_allowed = sum_insured * (policy.icu_sub_limit_pct / 100.0)

    if icu_daily_allowed is not None:
        for it in items:
            if not _is_icu_item(it.raw.raw_description or "", it.classification.canonical_name or ""):
                continue
            billed_amt = admissible.get(it.raw.id, 0.0)
            # ICU sub-limit applied per stay — cap total ICU charges
            allowed = icu_daily_allowed * los
            if billed_amt > allowed:
                excess = billed_amt - allowed
                icu_deduction += excess
                admissible[it.raw.id] = allowed
                name = (it.classification.canonical_name or it.raw.raw_description)[:40]
                icu_names.append(f"{name} ({_fmt(billed_amt)} → {_fmt(allowed)})")

    if icu_deduction > 0:
        deductions.append(ClaimDeduction(
            kind="icu_sub_limit",
            label="ICU / Critical Care Sub-Limit",
            amount=icu_deduction,
            explanation=(
                f"Your policy caps ICU charges at {_fmt(icu_daily_allowed)}/day "
                f"({_fmt(icu_daily_allowed * los)} for {int(los)} night(s))."
            ),
            items_affected=icu_names,
        ))

    # ── Step 4: Disease Sub-Limits ─────────────────────────────────────────
    disease_accum: dict[str, float] = {}  # disease -> total claimed so far
    disease_deduction = 0.0
    disease_names: list[str] = []

    for it in items:
        cap = _find_disease_sub_limit(it, policy)
        if cap is None:
            continue
        # Find which disease sub-limit this matched
        desc = (it.raw.raw_description or "").lower()
        canon = (it.classification.canonical_name or "").lower()
        disease_names_list = [d.disease for d in policy.disease_sub_limits]
        result = fuzz_process.extractOne(f"{desc} {canon}", disease_names_list, scorer=fuzz.token_set_ratio)
        if not result:
            continue
        disease_key = result[0]
        already_claimed = disease_accum.get(disease_key, 0.0)
        billed_amt = admissible.get(it.raw.id, 0.0)
        remaining_cap = max(0.0, cap - already_claimed)
        if billed_amt > remaining_cap:
            excess = billed_amt - remaining_cap
            disease_deduction += excess
            admissible[it.raw.id] = remaining_cap
            name = (it.classification.canonical_name or it.raw.raw_description)[:40]
            disease_names.append(
                f"{name} ({_fmt(billed_amt)} → {_fmt(remaining_cap)}, {disease_key} cap {_fmt(cap)})"
            )
            disease_accum[disease_key] = cap
        else:
            disease_accum[disease_key] = already_claimed + billed_amt

    if disease_deduction > 0:
        deductions.append(ClaimDeduction(
            kind="disease_sub_limit",
            label="Disease-Specific Sub-Limits",
            amount=disease_deduction,
            explanation="Your policy caps certain procedures/diseases at a specified amount per hospitalisation.",
            items_affected=disease_names,
        ))

    # ── Pre-deductible admissible ──────────────────────────────────────────
    pre_deductible = sum(admissible.values())
    total_deductions = sum(d.amount for d in deductions)

    # ── Step 5: Fixed Deductible ───────────────────────────────────────────
    deductible_applied = 0.0
    if policy.deductible_amount and policy.deductible_amount > 0:
        deductible_applied = min(policy.deductible_amount, pre_deductible)
        pre_deductible -= deductible_applied
        total_deductions += deductible_applied
        deductions.append(ClaimDeduction(
            kind="deductible",
            label="Fixed Deductible (Excess)",
            amount=deductible_applied,
            explanation=f"Your policy has a fixed deductible of {_fmt(policy.deductible_amount)} which the patient must bear first.",
            items_affected=[],
        ))

    # ── Step 6: Co-Pay ─────────────────────────────────────────────────────
    co_pay_amount = 0.0
    if policy.co_pay_pct and policy.co_pay_pct > 0:
        co_pay_amount = pre_deductible * (policy.co_pay_pct / 100.0)
        total_deductions += co_pay_amount
        deductions.append(ClaimDeduction(
            kind="co_pay",
            label=f"Co-Payment ({policy.co_pay_pct:.0f}%)",
            amount=co_pay_amount,
            explanation=(
                f"Your policy requires you to pay {policy.co_pay_pct:.0f}% of admissible charges. "
                f"This is {_fmt(co_pay_amount)} of the {_fmt(pre_deductible)} admissible amount."
            ),
            items_affected=[],
        ))

    # ── Step 7: Cap at Sum Insured + Top-Up ───────────────────────────────
    after_copay = pre_deductible - co_pay_amount
    estimated_settlement = after_copay
    top_up_triggered = False
    top_up_contribution = 0.0
    si_cap_deduction = 0.0

    if estimated_settlement > sum_insured:
        si_overflow = estimated_settlement - sum_insured
        if top_up_limit > 0 and si_overflow <= top_up_limit:
            top_up_triggered = True
            top_up_contribution = si_overflow
            # estimated_settlement stays as-is, funded by SI + top-up
        else:
            # Cap at SI + top-up (or just SI if no top-up)
            effective_cap = sum_insured + (top_up_limit if top_up_limit > 0 else 0)
            si_cap_deduction = estimated_settlement - effective_cap
            estimated_settlement = effective_cap
            if si_cap_deduction > 0:
                total_deductions += si_cap_deduction
                deductions.append(ClaimDeduction(
                    kind="sum_insured_cap",
                    label="Exceeds Sum Insured (+ Top-Up)",
                    amount=si_cap_deduction,
                    explanation=(
                        f"Admissible claim ({_fmt(after_copay)}) exceeds your total cover "
                        f"({_fmt(sum_insured + top_up_limit)}). The remaining {_fmt(si_cap_deduction)} "
                        f"is your responsibility."
                    ),
                    items_affected=[],
                ))

    estimated_settlement = max(0.0, estimated_settlement)
    patient_liability = gross_billed - estimated_settlement

    # ── Generate Tips & Warnings ───────────────────────────────────────────
    tips: list[str] = []
    warnings: list[str] = []

    if non_payable_total > 0:
        tips.append(
            f"Request a line-by-line bill and identify the {_fmt(non_payable_total)} in IRDAI "
            f"non-payable items. Insurers are legally required to reject these — pay them directly "
            f"after confirming each item is genuinely non-payable."
        )

    if room_cascade_deduction > 0:
        tips.append(
            f"The room rent cascade reduced your claim by {_fmt(room_cascade_deduction)}. "
            f"Consider upgrading your policy to a higher room rent limit or a 'room rent waiver' "
            f"add-on for future hospitalisations."
        )

    if policy.co_pay_pct and policy.co_pay_pct > 0:
        tips.append(
            f"Your {policy.co_pay_pct:.0f}% co-pay results in {_fmt(co_pay_amount)} out-of-pocket. "
            f"Senior citizen and some GMC policies carry mandatory co-pay — verify your policy schedule."
        )

    if top_up_triggered:
        tips.append(
            f"Your top-up policy is expected to cover {_fmt(top_up_contribution)} above the base SI. "
            f"File a separate reimbursement claim with your top-up insurer."
        )

    if disease_deduction > 0:
        tips.append(
            f"Disease sub-limits capped {_fmt(disease_deduction)} of your claim. Negotiate with "
            f"the hospital to restructure the invoice or escalate to the insurer's grievance desk "
            f"with CGHS benchmark evidence."
        )

    # Dispute the flagged overcharged items — these reduce patient liability
    red_amber_delta = sum(
        float(f.delta_amount or 0)
        for it in analysis.items
        for f in it.flags
        if f.kind in ("overcharge","drug_mrp_violation") and it.worst_severity in ("red","amber")
    )
    if red_amber_delta > 0:
        tips.append(
            f"Disputing the {_fmt(red_amber_delta)} in overcharged items identified in the bill review "
            f"above would directly reduce your gross bill — and therefore your non-payable deductions too."
        )

    if policy_was_uploaded and policy.confidence == "low":
        warnings.append(
            "The policy PDF was difficult to parse — some sub-limits may have been missed. "
            "Cross-check this estimate against your policy schedule of benefits."
        )
    if policy_was_uploaded and policy.confidence == "medium":
        warnings.append(
            "Some policy parameters were inferred rather than explicitly stated. "
            "Verify room rent and co-pay figures against your policy document."
        )
    if not policy.cashless_available:
        warnings.append(
            "This policy may require reimbursement claims (not cashless). "
            "Retain all original bills, prescriptions, and investigation reports."
        )

    return ClaimEstimate(
        gross_billed=gross_billed,
        deductions=deductions,
        total_deductions=total_deductions,
        pre_deductible_admissible=pre_deductible + deductible_applied,  # before deductible step
        co_pay_amount=co_pay_amount,
        deductible_applied=deductible_applied,
        estimated_settlement=estimated_settlement,
        patient_liability=patient_liability,
        sum_insured_used=min(estimated_settlement, sum_insured),
        top_up_triggered=top_up_triggered,
        top_up_contribution=top_up_contribution,
        room_rent_billed=room_rent_billed,
        room_rent_allowed=room_rent_allowed,
        room_rent_cascade_ratio=cascade_ratio,
        policy_confidence=policy.confidence,
        tips=tips,
        warnings=warnings,
    )
