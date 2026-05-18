## How it works

BillShield runs two independent pipelines: one that audits the hospital bill, and one that estimates the insurance claim. Both feed into a single dashboard.

### Bill analysis pipeline

PDF upload
│
▼

Ingest (pdfplumber → Tesseract OCR fallback)
│  → raw text + per-page word boxes
▼
Extract (Gemini LLM)
│  → hospital, patient, encounter, totals, line items
▼
Hospital sanity check (regex)
│  → detect stacked-bill PDFs, fill missing hospital name from GSTIN
▼
Classify (Gemini LLM, batched 40 at a time)
│  → each line item gets a category + canonical name
│    (consultation / diagnostic / drug / consumable / room /
│     procedure / package / anaesthesia / registration / misc)
▼
Benchmark each item (rule-based KB lookup)
│  → match against CGHS procedures, CGHS diagnostics, CGHS room rent,
│    NPPA drug ceilings, Jan Aushadhi generics, consumables KB
│  → produces a "fair price" or marks as unbenchmarkable
▼
Flag (rule engine)
│  → overcharge, drug_mrp_violation, unbenchmarkable,
│    ambiguous_mapping, ok — each with severity (green/amber/red/grey)
▼
AI reasonableness check (Gemini LLM)
│  → for items the KB couldn't benchmark, ask the LLM whether the
│    amount is reasonable / possibly_high / likely_high given the
│    description, hospital, and city tier
▼
Duplicate detection
│  → flag identical line items charged more than once
▼
Totals reconciliation
│  → sum line items, sanity-check against extracted bill total
▼
BillAnalysis → Dashboard


### Insurance claim pipeline (optional)
Policy PDF upload + Sum Insured / Top-up / Add-ons
│
▼

Ingest policy PDF (same OCR pipeline as bills)
│
▼
Score paragraphs by policy keywords
│  → "room rent", "ICU", "co-pay", "deductible", "waiting period",
│    "sub-limit", "sum insured", etc.
│  → pick the highest-scoring sections within a 22k-character budget,
│    preserving document order so tables stay intact
▼
Extract policy parameters (Gemini LLM)
│  → insurer, policy name, sum insured, room rent cap (% or absolute),
│    ICU sub-limit, co-pay %, deductible, waiting periods,
│    disease-specific sub-limits, pre/post hospitalisation days,
│    exclusions
▼
Apply user-declared add-on overrides
│  → room-rent waiver, no co-pay, OPD, maternity riders
▼
Claim engine — apply deductions in order:
│
│   a. IRDAI non-payable items (fuzzy-matched against curated list)
│   b. Room-rent cascade — if billed room > sub-limit, all
│      room-linked charges (consultation, anaesthesia) get the
│      same proportional reduction
│   c. ICU sub-limit
│   d. Disease-specific sub-limits
│   e. Fixed deductible
│   f. Co-pay percentage
│   g. Cap at sum insured; if top-up provided, check whether
│      it kicks in for the excess
│
▼
Generate tips and warnings
│  → "Disputing the ₹X in flagged overcharges would also reduce
│    your non-payable deductions"
│
▼
ClaimEstimate → Insurance panel
→ Gross billed · Total deductions · Estimated settlement · Patient liability


### Output: PDF reports

The dashboard can export four PDF documents:

- **Summary** — patient-facing overview of flags and totals
- **Hospital dispute letter** — a formal letter listing each flagged charge with the cited benchmark, addressed to the hospital
- **Claim summary** — insurance-facing breakdown of the claim estimate with deduction reasoning
- **Full pack** — all three combined

PDFs are rendered with WeasyPrint (HTML/CSS templates) for the patient-facing documents and ReportLab for the structured claim summary.

### Knowledge base

All benchmarks come from public Government of India sources, committed as CSVs in `billshield/knowledge_base/`:

- `cghs_procedures.csv` — CGHS 2025 procedure rates (X/Y/Z city tiers)
- `cghs_diagnostics.csv` — CGHS 2025 diagnostic test rates
- `cghs_room_rent.csv` — CGHS room rent ceilings (NABH / non-NABH)
- `nppa_drugs.csv` — NPPA ceiling prices for scheduled formulations
- `jan_aushadhi.csv` — Jan Aushadhi generic MRPs
- `consumables.csv` — indicative retail ranges for hospital consumables
- `irdai_non_payables.csv` — IRDAI list of items insurers will not reimburse
- `procedure_aliases.csv` — free-text-pattern → canonical-name mapping that lets the classifier match hospital-specific naming back to KB rows
