"""Verdict band + 5 KPI cards. Replaces the old hero."""
import reflex as rx
from ..state.bill_state import BillState

_RED, _AMBER, _GREY, _GREEN, _ACCENT = "#c8553d", "#b86b2a", "#737373", "#3a8262", "#5b6dd1"

def _notices_popover() -> rx.Component:
    return rx.popover.root(
        rx.popover.trigger(
            rx.button(
                rx.icon(tag="info", size=14),
                BillState.notices_count.to_string() + " notice" + rx.cond(BillState.notices_count == 1, "", "s"),
                variant="ghost", size="1", color_scheme="gray",
            ),
        ),
        rx.popover.content(
            rx.vstack(
                rx.text("Notes from analysis", weight="bold", size="2"),
                rx.foreach(BillState.all_notices,
                    lambda n: rx.text(n, size="1", color_scheme="gray", style={"lineHeight":"1.5"})),
                spacing="2", align="start", padding="4px",
            ),
            style={"maxWidth":"380px"},
        ),
    )

def verdict_band() -> rx.Component:
    return rx.box(
        rx.vstack(
            rx.hstack(
                rx.vstack(
                    rx.text(BillState.hospital_name, size="2", weight="medium"),
                    rx.text(BillState.patient_name + " · " + BillState.encounter_summary,
                            size="1", color_scheme="gray"),
                    spacing="1", align="start",
                ),
                rx.spacer(),
                rx.cond(BillState.notices_count > 0, _notices_popover(), rx.fragment()),
                width="100%", align="center",
            ),
            rx.box(height="8px"),
            rx.text(BillState.headline_sentence, size="5", weight="bold",
                    style={"letterSpacing":"-0.01em","lineHeight":"1.3"}),
            rx.text("Confidence: " + BillState.confidence_label + " · " +
                    BillState.items_verified_count + " items verified against government rates",
                    size="1", color_scheme="gray"),
            spacing="2", align="start", width="100%",
        ),
        padding="20px 24px", width="100%",
        border_bottom="1px solid var(--gray-a3)",
    )

def _kpi_card(key: str, label: str, amount: rx.Var, count: rx.Var,
              accent: str, icon: str) -> rx.Component:
    is_selected = BillState.selected_kpi == key
    return rx.box(
        rx.vstack(
            rx.hstack(
                rx.icon(tag=icon, size=14, color=accent),
                rx.text(label, size="1", weight="medium", color_scheme="gray"),
                spacing="2", align="center",
            ),
            rx.text("₹ " + amount, size="5", weight="bold",
                    style={"fontVariantNumeric":"tabular-nums","letterSpacing":"-0.02em"}),
            rx.text(count + " item" + rx.cond(count == "1", "", "s"),
                    size="1", color_scheme="gray"),
            spacing="1", align="start",
        ),
        on_click=BillState.select_kpi(key),
        padding="16px",
        border=rx.cond(is_selected, f"2px solid {accent}", "1px solid var(--gray-a4)"),
        border_radius="12px",
        background=rx.cond(is_selected, "var(--color-panel-solid)", "var(--color-panel-solid)"),
        cursor="pointer",
        min_width="170px", flex="1",
        style={
            "transition":"all 180ms ease",
            "boxShadow": rx.cond(is_selected, f"0 4px 14px {accent}22", "none"),
        },
        _hover={"transform":"translateY(-1px)","borderColor":accent},
    )

def kpi_cards() -> rx.Component:
    return rx.box(
        rx.hstack(
            _kpi_card("top_disputes", "Top 3 disputes",
                      BillState.top_disputes_total_str, BillState.top_disputes_count_str,
                      _ACCENT, "target"),
            _kpi_card("red", "Likely overcharged",
                      BillState.red_total_str, BillState.red_total_count.to_string(),
                      _RED, "alert-triangle"),
            _kpi_card("amber", "Possibly high",
                      BillState.amber_total_str, BillState.amber_total_count.to_string(),
                      _AMBER, "alert-circle"),
            _kpi_card("grey", "Need breakdown",
                      BillState.grey_total_str, BillState.grey_count.to_string(),
                      _GREY, "help-circle"),
            _kpi_card("green", "Looks fair",
                      BillState.green_total_str, BillState.green_count.to_string(),
                      _GREEN, "check"),
            spacing="3", width="100%",
            style={"overflowX":"auto","scrollSnapType":"x mandatory"},
        ),
        padding="20px 24px 8px 24px", width="100%",
    )