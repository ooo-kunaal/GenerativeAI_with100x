"""Landing page: upload a single PDF and analyse."""
import reflex as rx
from ..state.bill_state import BillState

UPLOAD_ID = "billpdf"

def _header() -> rx.Component:
    return rx.hstack(
        rx.hstack(
            rx.icon(tag="shield-check", size=20),
            rx.text("BillShield", weight="bold", size="5"),
            spacing="2", align="center",
        ),
        rx.spacer(),
        rx.text("Indian hospital bill review", size="2", color_scheme="gray"),
        align="center",
        padding="12px 22px",
        border_bottom="1px solid var(--gray-a4)",
        width="100%",
    )


def _upload_area() -> rx.Component:
    """Upload zone that collapses to a compact row once a file is selected."""
    return rx.cond(
        BillState.uploaded_filename != "",
        # ── File selected → compact pill ──────────────────────────────
        rx.box(
            rx.hstack(
                rx.icon(tag="file-check-2", size=16, color="var(--green-9)"),
                rx.text(BillState.uploaded_filename, size="2", weight="medium", flex="1",
                        style={"overflow": "hidden", "textOverflow": "ellipsis", "whiteSpace": "nowrap"}),
                rx.upload(
                    rx.button("Change file", size="1", variant="soft", color_scheme="gray"),
                    id=UPLOAD_ID,
                    accept={"application/pdf": [".pdf"]},
                    multiple=False,
                    on_drop=BillState.handle_upload(rx.upload_files(upload_id=UPLOAD_ID)),
                    style={"display": "inline-flex"},
                ),
                spacing="3", align="center", width="100%",
            ),
            padding="12px 16px",
            border="1px solid var(--green-a6)",
            border_radius="10px",
            background="var(--green-a2)",
            width="100%",
            max_width="560px",
        ),
        # ── No file → compact drop zone ───────────────────────────────
        rx.upload(
            rx.vstack(
                rx.icon(tag="cloud-upload", size=32, color="var(--gray-8)"),
                rx.text("Drop hospital bill PDF here, or click to browse",
                        size="2", weight="medium", color_scheme="gray"),
                rx.text("Single PDF · max 20 MB", size="1", color_scheme="gray"),
                spacing="2", align="center", padding="28px 20px",
            ),
            id=UPLOAD_ID,
            accept={"application/pdf": [".pdf"]},
            multiple=False,
            max_files=1,
            border="2px dashed var(--gray-a6)",
            border_radius="12px",
            background="var(--gray-a2)",
            on_drop=BillState.handle_upload(rx.upload_files(upload_id=UPLOAD_ID)),
            width="100%",
            max_width="560px",
            _hover={"borderColor": "var(--accent-9)", "background": "var(--accent-2)"},
            cursor="pointer",
        ),
    )


def landing() -> rx.Component:
    return rx.vstack(
        _header(),
        # ── Main content (flex-fills remaining height, scrolls if needed) ──
        rx.box(
            rx.vstack(
                rx.heading(
                    "Understand your hospital bill in 60 seconds.",
                    size="6",
                    style={"textAlign": "center", "maxWidth": "640px", "lineHeight": "1.2"},
                ),
                rx.text(
                    "Upload the final bill before discharge. We cross-check each line item against "
                    "CGHS 2025 rates, NPPA drug ceilings, and Jan Aushadhi generics.",
                    size="2", color_scheme="gray",
                    style={"textAlign": "center", "maxWidth": "560px"},
                ),

                _upload_area(),

                # ── Analyse button ─────────────────────────────────────────
                rx.cond(
                    BillState.is_analysing,
                    rx.box(
                        rx.vstack(
                            rx.hstack(
                                rx.spinner(size="2"),
                                rx.text("Analysing your bill…", weight="bold", size="2"),
                                spacing="3", align="center",
                            ),
                            rx.text(BillState.progress_message, size="1", color_scheme="gray"),
                            rx.progress(duration="30s", width="100%"),
                            spacing="2", align="center",
                            padding="14px 20px",
                        ),
                        border="1px solid var(--accent-6)",
                        border_radius="10px",
                        background="var(--accent-2)",
                        width="100%",
                        max_width="480px",
                    ),
                    rx.button(
                        "Analyse Bill",
                        on_click=BillState.analyse,
                        disabled=(BillState.uploaded_filename == ""),
                        size="3", variant="solid",
                    ),
                ),

                rx.cond(
                    BillState.error_message != "",
                    rx.callout(BillState.error_message, icon="triangle-alert",
                               color_scheme="red", size="1"),
                    rx.fragment(),
                ),

                rx.text(
                    "BillShield is a research/educational tool. Reference rates are drawn from "
                    "public Government of India sources. This is not legal advice.",
                    size="1", color_scheme="gray",
                    style={"textAlign": "center", "maxWidth": "520px"},
                ),

                spacing="4", align="center", width="100%",
                padding="28px 24px 20px 24px",
            ),
            flex="1",
            overflow_y="auto",
            width="100%",
            display="flex",
            align_items="center",
            justify_content="center",
        ),
        align="stretch",
        spacing="0",
        height="100vh",
        overflow="hidden",
    )
