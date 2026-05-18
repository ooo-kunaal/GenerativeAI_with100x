"""Claim settlement estimate panel — shown on dashboard when user opted into insurance mode."""
from __future__ import annotations
import reflex as rx
from ..state.bill_state import BillState

_BLUE  = "#3b6fd4"
_GREEN = "#3a8262"
_RED   = "#c8553d"
_AMBER = "#b86b2a"
_GREY  = "#737373"


def _section_header(title: str, icon: str) -> rx.Component:
    return rx.hstack(
        rx.icon(tag=icon, size=16, color=_BLUE),
        rx.text(title, weight="bold", size="3"),
        spacing="2", align="center",
        padding_bottom="12px",
    )


def _big_stat(label: str, amount_var, sub: str, color: str) -> rx.Component:
    return rx.vstack(
        rx.text(label, size="1", color_scheme="gray", weight="medium"),
        rx.text(
            rx.text.span("₹ "),
            rx.text.span(amount_var),
            size="6", weight="bold", color=color,
            style={"fontVariantNumeric": "tabular-nums", "letterSpacing": "-0.02em"},
        ),
        rx.text(sub, size="1", color_scheme="gray"),
        spacing="1", align="start",
    )


def _money_row_total(label: str, amount_var, color: str, size: str = "2") -> rx.Component:
    return rx.hstack(
        rx.text(label, size=size, weight="bold", flex="1"),
        rx.text(
            rx.text.span("₹ "),
            rx.text.span(amount_var),
            size=size, weight="bold", color=color,
            style={"fontVariantNumeric": "tabular-nums"},
        ),
        width="100%", align="center",
    )


def _deduction_row(ded: rx.Var) -> rx.Component:
    """Render one ClaimDeduction dict from BillState.claim_deductions inside rx.foreach."""
    return rx.box(
        rx.vstack(
            rx.hstack(
                rx.icon(tag="minus", size=12, color=_AMBER),
                rx.text(ded["label"], size="2", weight="medium", flex="1"),
                rx.text(
                    rx.text.span("₹ "),
                    rx.text.span(ded["amount_str"]),
                    size="2", weight="bold", color=_RED,
                    style={"fontVariantNumeric": "tabular-nums"},
                ),
                spacing="2", align="center", width="100%",
            ),
            rx.text(ded["explanation"], size="1", color_scheme="gray",
                    style={"lineHeight": "1.55"}),
            spacing="2", align="start",
        ),
        padding="10px 14px",
        border_left=f"3px solid {_AMBER}",
        border_radius="0 8px 8px 0",
        background="var(--amber-2)",
        width="100%",
    )


def _tip_row(tip: rx.Var) -> rx.Component:
    return rx.hstack(
        rx.icon(tag="lightbulb", size=14, color=_GREEN),
        rx.text(tip, size="2", color_scheme="gray", style={"lineHeight": "1.55"}, flex="1"),
        spacing="2", align="start", width="100%",
    )


def _warning_row(w: rx.Var) -> rx.Component:
    return rx.hstack(
        rx.icon(tag="triangle-alert", size=14, color=_AMBER),
        rx.text(w, size="2", color_scheme="gray", style={"lineHeight": "1.55"}, flex="1"),
        spacing="2", align="start", width="100%",
    )


def _policy_kv(label: str, value_var) -> rx.Component:
    """One row of the parsed-policy verification card."""
    return rx.hstack(
        rx.text(label, size="1", color_scheme="gray", weight="medium",
                style={"minWidth": "150px"}),
        rx.text(value_var, size="2", weight="medium", flex="1",
                style={"fontVariantNumeric": "tabular-nums"}),
        spacing="3", align="start", width="100%",
    )


def _disease_cap_row(d: rx.Var) -> rx.Component:
    return rx.hstack(
        rx.text(d["disease"], size="1", flex="1"),
        rx.text(d["cap_str"], size="1", weight="medium",
                style={"fontVariantNumeric": "tabular-nums"}),
        spacing="2", align="center", width="100%",
    )


def _exclusion_pill(e: rx.Var) -> rx.Component:
    return rx.badge(e, color_scheme="gray", variant="soft", size="1")


def _policy_summary_card() -> rx.Component:
    """Collapsible accordion that shows exactly what the parser extracted from the
    policy PDF. Lets the user verify the parse before trusting the estimate."""
    return rx.cond(
        BillState.policy_was_uploaded,
        rx.box(
            rx.accordion.root(
                rx.accordion.item(
                    header=rx.hstack(
                        rx.icon(tag="file-search-2", size=16, color=_BLUE),
                        rx.text("What we read from your policy", weight="bold", size="3"),
                        rx.spacer(),
                        rx.badge(
                            rx.text.span("Confidence: "),
                            rx.text.span(BillState.claim_policy_confidence),
                            color_scheme=rx.cond(
                                BillState.claim_policy_confidence == "high", "green",
                                rx.cond(BillState.claim_policy_confidence == "medium", "amber", "red"),
                            ),
                            variant="soft", size="1",
                        ),
                        width="100%", align="center", spacing="2",
                        padding_right="12px",
                    ),
                    content=rx.vstack(
                        rx.text(
                            "Verify these match your policy schedule of benefits before relying on the estimate.",
                            size="1", color_scheme="gray",
                        ),
                        rx.separator(width="100%"),

                        _policy_kv("Insurer", BillState.policy_insurer_str),
                        _policy_kv("Policy", BillState.policy_name_str),
                        _policy_kv("Type", BillState.policy_type_str),
                        _policy_kv("Sum Insured", BillState.policy_sum_insured_str),
                        _policy_kv("Room rent", BillState.policy_room_rent_str),
                        _policy_kv("ICU", BillState.policy_icu_str),
                        _policy_kv("Co-pay", BillState.policy_copay_str),
                        _policy_kv("Deductible", BillState.policy_deductible_str),
                        _policy_kv("Waiting period", BillState.policy_waiting_str),
                        _policy_kv("Pre/Post hosp.", BillState.policy_prepost_str),

                        rx.cond(
                            BillState.policy_has_extra_note,
                            rx.callout(
                                BillState.policy_extra_note,
                                icon="info", color_scheme="blue", size="1",
                            ),
                            rx.fragment(),
                        ),

                        rx.cond(
                            BillState.policy_has_disease_caps,
                            rx.vstack(
                                rx.text("Disease sub-limits", size="1", weight="bold",
                                        color_scheme="gray", padding_top="6px"),
                                rx.vstack(
                                    rx.foreach(BillState.policy_disease_caps, _disease_cap_row),
                                    spacing="1", width="100%",
                                ),
                                spacing="1", width="100%", align="start",
                            ),
                            rx.fragment(),
                        ),

                        rx.cond(
                            BillState.policy_has_exclusions,
                            rx.vstack(
                                rx.text("Key exclusions", size="1", weight="bold",
                                        color_scheme="gray", padding_top="6px"),
                                rx.hstack(
                                    rx.foreach(BillState.policy_exclusions, _exclusion_pill),
                                    spacing="2", style={"flexWrap": "wrap"},
                                ),
                                spacing="1", width="100%", align="start",
                            ),
                            rx.fragment(),
                        ),

                        spacing="2", align="start", width="100%",
                        padding="4px 4px 8px 4px",
                    ),
                    value="policy-summary",
                ),
                type="single",
                collapsible=True,
                variant="ghost",
                width="100%",
            ),
            padding="4px 16px",
            border="1px solid var(--gray-a4)",
            border_radius="12px",
            background="var(--gray-1)",
            width="100%",
        ),
        rx.fragment(),
    )


def claim_panel() -> rx.Component:
    """Full claim estimate section — only rendered when has_claim_estimate is True."""
    return rx.cond(
        BillState.has_claim_estimate,
        rx.box(
            rx.vstack(

                # ── Header ──────────────────────────────────────────────────
                rx.hstack(
                    rx.hstack(
                        rx.icon(tag="shield-check", size=18, color=_BLUE),
                        rx.vstack(
                            rx.text("Insurance Claim Estimate", weight="bold", size="4"),
                            rx.text(BillState.claim_policy_label, size="1", color_scheme="gray"),
                            spacing="0", align="start",
                        ),
                        spacing="3", align="center",
                    ),
                    rx.spacer(),
                    rx.badge(
                        rx.text.span("Policy confidence: "),
                        rx.text.span(BillState.claim_policy_confidence),
                        color_scheme=rx.cond(
                            BillState.claim_policy_confidence == "high", "green",
                            rx.cond(BillState.claim_policy_confidence == "medium", "amber", "gray")
                        ),
                        variant="soft", size="1",
                    ),
                    width="100%", align="center",
                ),

                rx.separator(width="100%"),

                # ── Big 3 Stats ─────────────────────────────────────────────
                rx.hstack(
                    _big_stat("Gross Bill", BillState.claim_gross_str, "Total billed amount", _GREY),
                    rx.separator(orientation="vertical", size="3"),
                    _big_stat("Est. Settlement", BillState.claim_settlement_str,
                              "Insurer likely to pay", _GREEN),
                    rx.separator(orientation="vertical", size="3"),
                    _big_stat("Patient Liability", BillState.claim_liability_str,
                              "Your out-of-pocket", _RED),
                    spacing="6", width="100%", align="start",
                    padding="16px 0",
                    style={"flexWrap": "wrap"},
                ),

                # ── Room Rent Cascade note ───────────────────────────────────
                rx.cond(
                    BillState.claim_has_cascade,
                    rx.callout(
                        rx.text(
                            rx.text.span("Room Rent Cascade Applied: Billed ₹ "),
                            rx.text.span(BillState.claim_room_billed_str),
                            rx.text.span(" → Policy allows ₹ "),
                            rx.text.span(BillState.claim_room_allowed_str),
                            rx.text.span(" · Associated charges reduced to "),
                            rx.text.span(BillState.claim_cascade_pct),
                            rx.text.span(" of billed. Does NOT apply to procedures, drugs, or diagnostics."),
                            size="2",
                        ),
                        icon="bed",
                        color_scheme="amber",
                        size="1",
                    ),
                    rx.fragment(),
                ),

                rx.separator(width="100%"),

                # ── Deduction Waterfall ──────────────────────────────────────
                rx.vstack(
                    _section_header("Deduction Breakdown", "list-minus"),
                    rx.vstack(
                        # Gross row
                        rx.hstack(
                            rx.text("Gross Billed Amount", size="2", color_scheme="gray", flex="1"),
                            rx.text(
                                rx.text.span("₹ "),
                                rx.text.span(BillState.claim_gross_str),
                                size="2", weight="medium",
                                style={"fontVariantNumeric": "tabular-nums"},
                            ),
                            width="100%", align="center",
                        ),
                        # Each deduction
                        rx.foreach(BillState.claim_deductions, _deduction_row),
                        rx.separator(width="100%"),
                        # Total deductions
                        _money_row_total("Total Deductions", BillState.claim_total_deductions_str, _RED, "2"),
                        # Final settlement
                        _money_row_total("Estimated Settlement", BillState.claim_settlement_str, _GREEN, "3"),
                        spacing="3", width="100%",
                    ),
                    spacing="0", width="100%",
                ),

                # ── What we read from your policy (collapsed accordion) ─────
                _policy_summary_card(),

                # ── Top-Up note ──────────────────────────────────────────────
                rx.cond(
                    BillState.claim_top_up_triggered,
                    rx.callout(
                        rx.text(
                            rx.text.span("Top-Up Triggered: Base policy (₹ "),
                            rx.text.span(BillState.claim_si_str),
                            rx.text.span(") exhausted. Top-up contributes ₹ "),
                            rx.text.span(BillState.claim_top_up_str),
                            rx.text.span(". File a separate reimbursement claim with your top-up insurer."),
                            size="2",
                        ),
                        icon="shield-plus",
                        color_scheme="blue",
                        size="1",
                    ),
                    rx.fragment(),
                ),

                rx.separator(width="100%"),

                # ── Tips ─────────────────────────────────────────────────────
                rx.cond(
                    BillState.claim_has_tips,
                    rx.vstack(
                        _section_header("Claim Tips", "lightbulb"),
                        rx.vstack(
                            rx.foreach(BillState.claim_tips, _tip_row),
                            spacing="3", width="100%",
                        ),
                        spacing="0", width="100%",
                    ),
                    rx.fragment(),
                ),

                # ── Warnings ─────────────────────────────────────────────────
                rx.cond(
                    BillState.claim_has_warnings,
                    rx.vstack(
                        rx.foreach(BillState.claim_warnings, _warning_row),
                        spacing="2", width="100%",
                    ),
                    rx.fragment(),
                ),

                # ── Export button ─────────────────────────────────────────────
                rx.hstack(
                    rx.spacer(),
                    rx.button(
                        rx.icon(tag="download", size=14),
                        "Download Claim Summary PDF",
                        on_click=BillState.export_pdf("claim_summary"),
                        variant="soft",
                        color_scheme="blue",
                        size="2",
                    ),
                    width="100%",
                    padding_top="8px",
                ),

                spacing="4", align="start", width="100%",
            ),
            padding="24px",
            border="1px solid var(--blue-a6)",
            border_radius="16px",
            background="var(--blue-1)",
            width="100%",
        ),
        rx.fragment(),
    )
