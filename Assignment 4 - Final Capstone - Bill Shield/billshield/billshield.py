"""BillShield Reflex app entry. Loads .env, registers pages, sets theme."""
from __future__ import annotations
import os
from pathlib import Path
import reflex as rx
from dotenv import load_dotenv

# Load .env once at import time so service modules see ANTHROPIC_API_KEY.
load_dotenv(Path(__file__).resolve().parents[1] / ".env")

from .pages.landing import landing
from .pages.dashboard import dashboard

app = rx.App(
    theme=rx.theme(
        appearance="light",
        has_background=True,
        radius="medium",
        accent_color="indigo",
        gray_color="slate",
    ),
    stylesheets=[],
)

app.add_page(landing,   route="/",          title="BillShield · Upload")
app.add_page(dashboard, route="/dashboard", title="BillShield · Analysis")
