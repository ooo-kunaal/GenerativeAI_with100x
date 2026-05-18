"""Single place where we talk to the Gemini API. All other modules go through this.
Public surface (unchanged from the Anthropic version):
    complete_json(prompt) -> dict
    complete_chat(system, history, max_tokens) -> str
"""
from __future__ import annotations
import os
import json
from google import genai
from google.genai import types

import re as _re
def _safe_json_loads(text: str) -> dict:
    """Parse JSON tolerantly. Handles bad escapes and mid-response truncation."""
    try: return json.loads(text)
    except json.JSONDecodeError: pass
    # Bad-escape pass
    fixed = _re.sub(r'\\(?!["\\/bfnrtu])', r'\\\\', text)
    try: return json.loads(fixed)
    except json.JSONDecodeError: pass
    # Truncation recovery: close unterminated string + brackets
    repaired = _repair_truncated_json(fixed)
    try: return json.loads(repaired)
    except json.JSONDecodeError: pass
    # Last resort: nuke lone backslashes
    nuked = _re.sub(r'\\(?!["\\/bfnrtu])', ' ', text)
    repaired2 = _repair_truncated_json(nuked)
    return json.loads(repaired2)

def _repair_truncated_json(text: str) -> str:
    """If Gemini's output was cut off mid-string/array/object, close it cleanly
    so we recover whatever items DID parse. Drops the last incomplete item."""
    s = text.rstrip()
    # If the last char isn't a closer, we're probably truncated. Walk the string
    # tracking quote/bracket state, then drop the trailing partial item.
    in_str = False; esc = False; stack: list[str] = []
    last_safe = 0  # index just after the last successfully closed top-level item
    for i, ch in enumerate(s):
        if esc: esc = False; continue
        if ch == "\\" and in_str: esc = True; continue
        if ch == '"': in_str = not in_str; continue
        if in_str: continue
        if ch in "{[": stack.append(ch)
        elif ch in "}]":
            if stack: stack.pop()
            # Each time we close back to depth 2 (inside line_items array), it's a safe rewind point
            if len(stack) == 2: last_safe = i + 1
    if not in_str and not stack:
        return s  # Wasn't actually truncated
    # Rewind to last safe boundary, then close all open brackets
    truncated = s[:last_safe].rstrip().rstrip(",")
    # Re-walk the truncated prefix to know what remains open
    in_str = False; esc = False; stack = []
    for ch in truncated:
        if esc: esc = False; continue
        if ch == "\\" and in_str: esc = True; continue
        if ch == '"': in_str = not in_str; continue
        if in_str: continue
        if ch in "{[": stack.append(ch)
        elif ch in "}]" and stack: stack.pop()
    closers = "".join("}" if c == "{" else "]" for c in reversed(stack))
    return truncated + closers
    
_client: "genai.Client | None" = None

def _get_client() -> "genai.Client":
    global _client
    if _client is None:
        api_key = os.environ.get("GEMINI_API_KEY")
        if not api_key:
            raise RuntimeError("GEMINI_API_KEY not set. Copy .env.example to .env and fill it.")
        _client = genai.Client(api_key=api_key)
    return _client

def _model() -> str:
    return os.environ.get("GEMINI_MODEL", "gemini-2.5-pro")

def complete_json(prompt: str, max_tokens: int = 16000) -> dict:
    """Call Gemini expecting a JSON-only response."""
    resp = _get_client().models.generate_content(
        model=_model(),
        contents=prompt,
        config=types.GenerateContentConfig(
            max_output_tokens=max_tokens,
            response_mime_type="application/json",
        ),
    )
    text = (resp.text or "").strip()
    if text.startswith("```"):
        text = text.split("```", 2)[1]
        if text.startswith("json"): text = text[4:]
        text = text.strip("` \n")
    return _safe_json_loads(text)

def complete_chat(system: str, history: list[dict], max_tokens: int = 1500) -> str:
    """Call Gemini for a chat turn. history is a list of {role, content} dicts (role in {'user','assistant'})."""
    contents = []
    for msg in history:
        role = "user" if msg["role"] == "user" else "model"
        contents.append(types.Content(role=role, parts=[types.Part.from_text(text=msg["content"])]))
    resp = _get_client().models.generate_content(
        model=_model(),
        contents=contents,
        config=types.GenerateContentConfig(
            system_instruction=system,
            max_output_tokens=max_tokens,
        ),
    )
    return (resp.text or "").strip()
