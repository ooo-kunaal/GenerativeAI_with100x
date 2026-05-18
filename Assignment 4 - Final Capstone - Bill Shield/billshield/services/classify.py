"""Line items -> category + canonical_name + needs_benchmark via Claude."""
from __future__ import annotations
import json
from pathlib import Path
from .llm import complete_json
from .models import Classification

_PROMPT_PATH = Path(__file__).resolve().parents[2] / "prompts" / "classify.txt"

def classify(line_items: list[dict]) -> list[dict]:
    tpl = _PROMPT_PATH.read_text(encoding="utf-8")
    minimal = [
        {"id": li["id"], "raw_description": li["raw_description"], "section_header": li.get("section_header")}
        for li in line_items
    ]
    prompt = tpl.replace("{items_json}", json.dumps(minimal, ensure_ascii=False, indent=2))
    data = complete_json(prompt, max_tokens=8000)
    items = data.get("items", [])
    return [Classification(**c).model_dump() for c in items]
