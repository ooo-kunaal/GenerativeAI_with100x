"""LLM-based reasonableness check for items that the KB can't benchmark.

Instead of marking everything as grey/unverifiable, we ask the LLM to judge
whether the billed amount seems reasonable for a given hospital category, city,
and item type. This is explicitly flagged as an AI opinion, not a cited benchmark.
"""
from __future__ import annotations
import json
from .llm import complete_json


_PROMPT = """You are a medical billing auditor for Indian hospitals. For each item below, assess whether the billed amount is reasonable for the given hospital context.

Hospital: {hospital_name} ({city}, {nabh_status})
Encounter: {encounter_type}

Items to assess (these have no government benchmark available):
{items_json}

For EACH item, return your judgement. Consider:
- Typical rates for this category in Indian hospitals of this tier
- Whether the amount per unit seems inflated
- Common consumable/misc charges that are often padded

Return JSON:
{{
  "assessments": [
    {{
      "id": "<item id>",
      "verdict": "reasonable" | "possibly_high" | "likely_high",
      "estimated_fair_range_low": <number or null>,
      "estimated_fair_range_high": <number or null>,
      "reasoning": "<1-2 sentence explanation>"
    }}
  ]
}}

Rules:
- Be conservative: if unsure, say "reasonable"
- Do NOT invent specific government benchmark numbers
- Base estimates on typical Indian hospital rates for the category
- "possibly_high" = 20-80% above what you'd expect
- "likely_high" = >80% above what you'd expect
"""

def check_reasonableness(items, hospital_name, city, nabh_status, encounter_type):
    if not items: return {}
    BATCH = 25
    out: dict[str, dict] = {}
    for i in range(0, len(items), BATCH):
        batch = items[i:i+BATCH]
        minimal = [{"id": it["id"], "description": it["description"], "category": it["category"],
                    "amount": it["amount"], "quantity": it.get("quantity",1), "unit_rate": it.get("unit_rate")}
                   for it in batch]
        prompt = _PROMPT.format(hospital_name=hospital_name, city=city or "unknown",
                                nabh_status=nabh_status, encounter_type=encounter_type,
                                items_json=json.dumps(minimal, ensure_ascii=False, indent=2))
        try:
            data = complete_json(prompt, max_tokens=8000)
            for a in data.get("assessments", []):
                if "id" in a: out[a["id"]] = a
        except Exception:
            continue
    return out