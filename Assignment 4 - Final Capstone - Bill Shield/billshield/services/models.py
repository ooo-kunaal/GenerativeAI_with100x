"""Shared dataclasses / Pydantic models for the bill-analysis pipeline."""
from __future__ import annotations
import math
from typing import Literal, Optional
from pydantic import BaseModel, Field, model_validator

Category = Literal["consultation","diagnostic","procedure","drug","consumable","room","package","anaesthesia","registration","misc"]
FlagKind = Literal["overcharge","drug_mrp_violation","duplicate","unbenchmarkable","ambiguous_mapping","stale_benchmark","ok"]
Severity = Literal["green","amber","red","grey"]

class Hospital(BaseModel):
    name: str
    city: Optional[str] = None
    city_tier: Literal["X","Y","Z","unknown"] = "unknown"
    nabh_status: Literal["NABH","non_NABH","unknown"] = "unknown"
    gstin: Optional[str] = None

class Patient(BaseModel):
    name_redacted: str
    age: Optional[int] = None
    sex: Literal["M","F","other","unknown"] = "unknown"

class Encounter(BaseModel):
    type: Literal["OPD","Emergency","IPD","Daycare","unknown"] = "unknown"
    admission_date: Optional[str] = None
    discharge_date: Optional[str] = None
    ward_or_bed: Optional[str] = None
    length_of_stay_days: Optional[float] = None

class Totals(BaseModel):
    gross_amount: float = 0.0
    discount: float = 0.0
    tax: float = 0.0
    net_amount: float = 0.0
    currency: str = "INR"

class RawLineItem(BaseModel):
    id: str
    raw_description: str
    hospital_code: Optional[str] = None
    quantity: float = 1.0
    unit_rate: Optional[float] = None
    amount: float = 0.0
    section_header: Optional[str] = None

    @model_validator(mode="before")
    @classmethod
    def _coerce_nulls(cls, values):
        """LLM may return null for quantity/amount; coerce to defaults."""
        if isinstance(values, dict):
            if values.get("quantity") is None:
                values["quantity"] = 1.0
            if values.get("amount") is None:
                values["amount"] = 0.0
            if values.get("unit_rate") is not None:
                try:
                    values["unit_rate"] = float(values["unit_rate"])
                except (TypeError, ValueError):
                    values["unit_rate"] = None
        return values

class Classification(BaseModel):
    id: str
    category: Category
    canonical_name: str
    needs_benchmark: bool
    confidence: Literal["high","medium","low"] = "medium"
    notes: Optional[str] = None

class Benchmark(BaseModel):
    """A KB lookup result for one line item."""
    matched: bool
    source_table: Optional[str] = None
    canonical_name: Optional[str] = None
    benchmark_amount: Optional[float] = None
    citation_om: Optional[str] = None
    citation_date: Optional[str] = None
    match_method: Optional[Literal["alias","fuzzy","semantic","none"]] = None
    match_score: Optional[float] = None
    candidates: list[dict] = Field(default_factory=list)  # top-3 if ambiguous
    notes: Optional[str] = None

    @model_validator(mode="before")
    @classmethod
    def _scrub_nan(cls, values):
        """Pandas .to_dict() emits float('nan') for missing cells; coerce to None."""
        if isinstance(values, dict):
            for k, v in values.items():
                if isinstance(v, float) and math.isnan(v):
                    values[k] = None
        return values

class Flag(BaseModel):
    kind: FlagKind
    severity: Severity
    message: str
    benchmark_amount: Optional[float] = None
    billed_amount: float
    delta_amount: Optional[float] = None
    delta_pct: Optional[float] = None
    citation: Optional[str] = None  # human-readable citation string

class AnalysedLineItem(BaseModel):
    raw: RawLineItem
    classification: Classification
    benchmark: Benchmark
    flags: list[Flag] = Field(default_factory=list)
    worst_severity: Severity = "grey"

class BillAnalysis(BaseModel):
    hospital: Hospital
    patient: Patient
    encounter: Encounter
    totals: Totals
    items: list[AnalysedLineItem]
    unparsed_lines: list[str] = Field(default_factory=list)
    # roll-ups for the KPI band
    rupees_verified_fair: float = 0.0
    rupees_flagged: float = 0.0
    rupees_unverifiable: float = 0.0


# ─── Insurance / Claim estimation models ─────────────────────────────────────

class DiseaseSubLimit(BaseModel):
    disease: str
    cap_amount: float
    notes: Optional[str] = None

class PolicySummary(BaseModel):
    """Structured parameters extracted from a health insurance policy PDF."""
    policy_type: Literal["GMC","individual","family_floater","top_up","super_top_up","unknown"] = "unknown"
    insurer: str = "Unknown Insurer"
    policy_name: str = ""
    sum_insured: Optional[float] = None  # base SI in rupees, if stated in the policy
    # Room rent
    room_rent_sub_limit_pct: Optional[float] = None   # % of SI per day
    room_rent_sub_limit_abs: Optional[float] = None   # absolute ₹ per day
    room_rent_waiver: bool = False
    room_type_description: Optional[str] = None  # e.g. "Single Standard AC Room"
    # ICU
    icu_sub_limit_pct: Optional[float] = None
    icu_sub_limit_abs: Optional[float] = None
    # Co-pay / deductible
    co_pay_pct: Optional[float] = None
    deductible_amount: Optional[float] = None
    # Riders
    maternity_covered: bool = False
    maternity_sub_limit: Optional[float] = None
    opd_covered: bool = False
    opd_sub_limit: Optional[float] = None
    # Waiting periods
    pre_existing_waiting_days: Optional[int] = None
    initial_waiting_days: Optional[int] = None
    pre_hospitalization_days: Optional[int] = None
    post_hospitalization_days: Optional[int] = None
    ambulance_limit: Optional[float] = None
    # Sub-limits
    disease_sub_limits: list[DiseaseSubLimit] = Field(default_factory=list)
    # Misc
    exclusions_summary: list[str] = Field(default_factory=list)
    non_payable_note: Optional[str] = None
    cashless_available: bool = True
    network_hospitals_note: Optional[str] = None
    confidence: Literal["high","medium","low"] = "low"


DeductionKind = Literal[
    "non_payable",          # IRDAI non-payable item
    "room_rent_cascade",    # proportional deduction due to room over sub-limit
    "icu_sub_limit",        # ICU charge above sub-limit
    "disease_sub_limit",    # disease-specific cap reached
    "co_pay",               # patient co-payment
    "deductible",           # fixed excess/deductible
    "sum_insured_cap",      # amount exceeds SI
]

class ClaimDeduction(BaseModel):
    kind: DeductionKind
    label: str              # human-readable label, e.g. "IRDAI Non-Payable Items"
    amount: float           # rupee amount deducted
    explanation: str        # one-sentence explanation
    items_affected: list[str] = Field(default_factory=list)  # item names or descriptions

class ClaimEstimate(BaseModel):
    """Output of the claim estimation engine."""
    gross_billed: float
    # Step-by-step deductions
    deductions: list[ClaimDeduction]
    total_deductions: float
    # Intermediate
    pre_deductible_admissible: float        # after all deductions before co-pay/deductible
    co_pay_amount: float = 0.0
    deductible_applied: float = 0.0
    # Final numbers
    estimated_settlement: float             # what insurer will likely pay
    patient_liability: float                # what patient must bear
    sum_insured_used: float                 # SI consumed by this claim
    top_up_triggered: bool = False          # whether top-up kicks in
    top_up_contribution: float = 0.0
    # Room rent cascade details (for UI display)
    room_rent_billed: float = 0.0
    room_rent_allowed: float = 0.0
    room_rent_cascade_ratio: float = 1.0    # allowed/billed; 1.0 = no cascade
    # Metadata
    policy_confidence: Literal["high","medium","low"] = "low"
    tips: list[str] = Field(default_factory=list)   # actionable claim tips
    warnings: list[str] = Field(default_factory=list)
