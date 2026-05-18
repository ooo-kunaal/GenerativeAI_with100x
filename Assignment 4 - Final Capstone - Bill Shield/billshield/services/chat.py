"""Bounded chat. The system prompt forbids un-cited numeric claims, medical
advice, and formal legal advice. Analysis context is injected once per turn so
the model always sees the latest findings."""
from __future__ import annotations
import json
from pathlib import Path
from .llm import complete_chat
from .models import BillAnalysis

_PROMPT_PATH = Path(__file__).resolve().parents[2] / "prompts" / "chat_system.txt"

def _context_from(analysis: BillAnalysis) -> str:
    """Render the analysis as compact JSON the LLM can reason over without bloating tokens."""
    payload = {
        "hospital": analysis.hospital.model_dump(),
        "encounter": analysis.encounter.model_dump(),
        "totals": analysis.totals.model_dump(),
        "summary": {
            "verified_fair": analysis.rupees_verified_fair,
            "flagged":       analysis.rupees_flagged,
            "unverifiable":  analysis.rupees_unverifiable,
        },
        "items": [
            {
                "description": it.raw.raw_description,
                "billed": it.raw.amount,
                "canonical_name": it.classification.canonical_name,
                "category": it.classification.category,
                "severity": it.worst_severity,
                "flags": [f.model_dump() for f in it.flags],
            } for it in analysis.items
        ],
    }
    return json.dumps(payload, ensure_ascii=False, indent=2)

def reply(analysis: BillAnalysis, history: list[dict], user_message: str) -> str:
    """history is the existing conversation; user_message is the new user turn."""
    system_tpl = _PROMPT_PATH.read_text(encoding="utf-8")
    system = system_tpl.replace("{analysis_context}", _context_from(analysis))
    new_history = history + [{"role": "user", "content": user_message}]
    return complete_chat(system, new_history)
