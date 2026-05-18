"""Dashboard page: shown after analysis completes."""
import reflex as rx
from ..state.bill_state import BillState
from ..components.kpi_band import verdict_band, kpi_cards
from ..components.flag_table import flag_table
from ..components.export_buttons import export_buttons
from ..components.chat_panel import chat_panel
from ..components.claim_panel import claim_panel
from ..components.claim_form import claim_form

_BLUE = "#3b6fd4"


def _header() -> rx.Component:
    return rx.hstack(
        rx.hstack(rx.icon(tag="shield-check", size=20),
                  rx.text("BillShield", weight="bold", size="5"),
                  spacing="2", align="center"),
        rx.spacer(),
        rx.button(rx.icon(tag="upload", size=14), "New bill",
                  on_click=rx.redirect("/"), variant="ghost", size="2"),
        align="center", padding="12px 24px",
        border_bottom="1px solid var(--gray-a3)", width="100%",
    )


def _empty_state() -> rx.Component:
    return rx.center(
        rx.vstack(
            rx.icon(tag="file-question", size=44, color="var(--gray-7)"),
            rx.text("No analysis yet. Upload a bill to begin.", color_scheme="gray"),
            rx.button("Go to upload", on_click=rx.redirect("/"), variant="soft"),
            spacing="3", align="center",
        ),
        flex="1", width="100%",
    )


def _claim_tab_content() -> rx.Component:
    """Content of the Claim Estimate tab."""
    return rx.cond(
        BillState.is_estimating_claim,
        # ── Estimating spinner ─────────────────────────────────────────
        rx.center(
            rx.vstack(
                rx.spinner(size="3"),
                rx.text("Parsing policy & estimating claim…",
                        size="2", color_scheme="gray", weight="medium"),
                rx.text("This may take 30–60 seconds for long policy documents.",
                        size="1", color_scheme="gray"),
                spacing="3", align="center",
            ),
            flex="1", width="100%", padding="60px",
        ),
        rx.cond(
            BillState.has_claim_estimate,
            # ── Results ───────────────────────────────────────────────
            rx.vstack(
                # Re-estimate strip at top of results
                rx.hstack(
                    rx.text(BillState.claim_policy_label, size="1", color_scheme="gray"),
                    rx.spacer(),
                    rx.button(
                        rx.icon(tag="refresh-cw", size=12),
                        "Re-estimate",
                        on_click=BillState.reset_claim_estimate,
                        size="1", variant="ghost", color_scheme="gray",
                    ),
                    width="100%", align="center",
                    padding="0 0 8px 0",
                ),
                claim_panel(),
                spacing="0", align="start", width="100%",
            ),
            # ── Input form ─────────────────────────────────────────────
            rx.vstack(
                claim_form(),
                spacing="3", align="start", width="100%",
                padding_top="8px",
            ),
        ),
    )


def dashboard() -> rx.Component:
    return rx.vstack(
        _header(),
        rx.cond(
            BillState.has_result,
            # ── Tabbed dashboard ──────────────────────────────────────────
            rx.tabs.root(
                # Tab triggers
                rx.tabs.list(
                    rx.tabs.trigger("Bill Review", value="bill"),
                    rx.tabs.trigger(
                        rx.hstack(
                            rx.icon(tag="shield-check", size=13),
                            rx.text("Claim Estimate", size="2"),
                            spacing="1", align="center",
                        ),
                        value="claim",
                    ),
                    padding="0 24px",
                ),

                # ── Bill Review tab ───────────────────────────────────────
                rx.tabs.content(
                    rx.vstack(
                        verdict_band(),
                        rx.hstack(rx.spacer(), export_buttons(),
                                  padding="12px 24px 0 24px", width="100%"),
                        kpi_cards(),
                        rx.box(flag_table(), padding="8px 24px 24px 24px", width="100%"),
                        spacing="0", align="stretch", width="100%",
                    ),
                    value="bill",
                    overflow_y="auto",
                    height="100%",
                ),

                # ── Claim Estimate tab ────────────────────────────────────
                rx.tabs.content(
                    rx.box(
                        _claim_tab_content(),
                        padding="16px 24px 32px 24px",
                        width="100%",
                        max_width="760px",
                        margin="0 auto",
                    ),
                    value="claim",
                    overflow_y="auto",
                    height="100%",
                ),

                value=BillState.dashboard_tab,
                on_change=BillState.set_dashboard_tab,
                flex="1",
                display="flex",
                flex_direction="column",
                overflow="hidden",
                width="100%",
            ),
            _empty_state(),
        ),
        chat_panel(),
        align="stretch",
        spacing="0",
        height="100vh",
        overflow="hidden",
        background="var(--gray-1)",
    )