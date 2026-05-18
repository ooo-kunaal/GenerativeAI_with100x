# BillShield

Indian hospital bill review tool. Upload a hospital bill PDF and an insurance policy PDF; BillShield cross-checks each line item against CGHS 2025 rates, NPPA drug ceilings, and Jan Aushadhi generics, flags overcharges, and estimates your insurance claim settlement.

Built with Reflex (Python full-stack) and Google Gemini for extraction and classification.

Youtube Demo - https://youtu.be/0lTIQpVWdKQ 

## Features

- Line-item extraction from scanned hospital bills (pdfplumber + Tesseract OCR fallback)
- Classification against a curated knowledge base of CGHS procedures, diagnostics, room rents, drug price ceilings, and IRDAI non-payables
- Policy parsing — extract sum insured, room rent caps, ICU sub-limits, co-pay, waiting periods
- Claim settlement estimation with proportionate deduction cascade
- Exportable PDF reports (summary, hospital dispute letter, full pack, claim summary)
- Conversational chat over the analysed bill

## Tech stack

- Reflex 0.9.x (Python web framework)
- Google Gemini (`gemini-2.5-flash`) for LLM extraction
- pdfplumber, pytesseract, Pillow for ingestion
- pandas, rapidfuzz for KB lookups
- WeasyPrint + ReportLab for PDF export

## Running locally

### Prerequisites

- Python 3.11+
- Tesseract OCR installed and on PATH (https://github.com/UB-Mannheim/tesseract/wiki on Windows)
- On Windows, GTK3 runtime for WeasyPrint (https://github.com/tschoonj/GTK-for-Windows-Runtime-Environment-Installer)
- A Google Gemini API key (https://aistudio.google.com/apikey)

## Project structure

- `billshield/components/` — Reflex UI components
- `billshield/state/` — Reflex state (`BillState`, `ChatState`)
- `billshield/services/` — pipeline (ingest → extract → classify → flag → claim engine)
- `billshield/prompts/` — LLM prompt templates
- `billshield/knowledge_base/` — CGHS, NPPA, Jan Aushadhi, IRDAI reference CSVs
- `billshield/assets/` — static assets

## Notes

BillShield is a research/educational tool. Reference rates are drawn from public Government of India sources. This is not legal or financial advice.
