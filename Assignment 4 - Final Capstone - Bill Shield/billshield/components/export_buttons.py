"""Export modal. Clicking an option triggers an in-memory PDF render and a
direct browser download via rx.download(data=bytes, filename=...). No /_upload
URL involved — avoids serialization and frontend/backend URL mismatch issues."""
import reflex as rx
from ..state.bill_state import BillState

def _option(label: str, desc: str, kind: str, icon_tag: str) -> rx.Component:
    return rx.button(
        rx.hstack(
            rx.icon(tag=icon_tag, size=18),
            rx.vstack(
                rx.text(label, size="2", weight="medium"),
                rx.text(desc, size="1", color_scheme="gray"),
                spacing="0", align="start",
            ),
            spacing="3", align="center", width="100%",
        ),
        on_click=BillState.export_pdf(kind),
        loading=(BillState.is_exporting & (BillState.export_kind == kind)),
        variant="ghost",
        style={"width":"100%","justifyContent":"flex-start","padding":"12px 16px","height":"auto"},
    )

def export_buttons() -> rx.Component:
    return rx.box(
        rx.button(rx.icon(tag="download", size=14), "Export",
                  on_click=BillState.toggle_export_modal, variant="soft", size="2"),
        rx.dialog.root(
            rx.dialog.content(
                rx.dialog.title("Export Report"),
                rx.dialog.description("Choose a format. The file will download automatically.", size="2"),
                rx.vstack(
                    _option("One-Page Summary", "Quick overview of flagged items and totals",
                            "summary", "file-text"),
                    _option("Letter to Hospital", "Polite request-for-clarification letter",
                            "hospital_letter", "mail"),
                    _option("Full Details", "Complete evidence pack with all benchmarks and citations",
                            "full_pack", "file-stack"),
                    spacing="2", width="100%", padding_top="12px",
                ),
                rx.dialog.close(
                    rx.button("Close", variant="soft", color_scheme="gray", size="2",
                              style={"marginTop":"12px"}),
                ),
                style={"maxWidth":"480px"},
            ),
            open=BillState.export_modal_open,
            on_open_change=lambda _: BillState.toggle_export_modal(),
        ),
    )