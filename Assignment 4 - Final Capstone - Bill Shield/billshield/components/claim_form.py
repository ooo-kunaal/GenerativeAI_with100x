"""Claim estimation input form — lives in the dashboard Claim Estimate tab."""
from __future__ import annotations
import reflex as rx
from ..state.bill_state import BillState

POLICY_UPLOAD_ID = "policypdf_tab"

_BLUE = "#3b6fd4"
_GREEN = "#3a8262"
_GREY = "#737373"


def _addon_pill(label: str, checked_var: rx.Var, on_click) -> rx.Component:
    return rx.button(
        rx.cond(checked_var, rx.icon(tag="check", size=12), rx.icon(tag="plus", size=12)),
        label,
        on_click=on_click,
        variant=rx.cond(checked_var, "solid", "outline"),
        color_scheme=rx.cond(checked_var, "blue", "gray"),
        size="1",
        style={"fontWeight": "500"},
    )


def _policy_file_picker() -> rx.Component:
    return rx.cond(
        BillState.policy_filename != "",
        rx.box(
            rx.hstack(
                rx.icon(tag="file-check-2", size=14, color="var(--blue-9)"),
                rx.text(BillState.policy_filename, size="2", weight="medium", flex="1",
                        style={"overflow": "hidden", "textOverflow": "ellipsis", "whiteSpace": "nowrap"}),
                rx.upload(
                    rx.button("Change", size="1", variant="ghost", color_scheme="gray"),
                    id=POLICY_UPLOAD_ID,
                    accept={"application/pdf": [".pdf"]},
                    multiple=False,
                    on_drop=BillState.handle_policy_upload(
                        rx.upload_files(upload_id=POLICY_UPLOAD_ID)
                    ),
                    style={"display": "inline-flex"},
                ),
                spacing="2", align="center", width="100%",
            ),
            padding="8px 12px",
            border="1px solid var(--blue-a6)",
            border_radius="8px",
            background="var(--blue-a2)",
            width="100%",
        ),
        rx.upload(
            rx.hstack(
                rx.icon(tag="upload", size=14, color="var(--blue-8)"),
                rx.text("Upload policy wording PDF", size="2", color_scheme="blue"),
                rx.badge("optional", size="1", variant="soft", color_scheme="gray"),
                spacing="2", align="center", padding="10px 16px",
            ),
            id=POLICY_UPLOAD_ID,
            accept={"application/pdf": [".pdf"]},
            multiple=False,
            on_drop=BillState.handle_policy_upload(
                rx.upload_files(upload_id=POLICY_UPLOAD_ID)
            ),
            border="1px dashed var(--blue-a6)",
            border_radius="8px",
            background="var(--blue-a2)",
            width="100%",
            _hover={"borderColor": "var(--blue-9)", "background": "var(--blue-a3)"},
            cursor="pointer",
        ),
    )


def claim_form() -> rx.Component:
    """Input form shown before estimation — all fields optional."""
    return rx.vstack(

        # Header
        rx.vstack(
            rx.text("Estimate Your Claim Settlement", weight="bold", size="4"),
            rx.text(
                "Without any inputs we apply IRDAI non-payable deductions as a baseline. "
                "Add policy wording and sum insured for a full settlement estimate.",
                size="2", color_scheme="gray",
            ),
            spacing="1", align="start", width="100%",
        ),

        rx.separator(width="100%"),

        # Baseline notice
        rx.hstack(
            rx.icon(tag="info", size=14, color=_BLUE),
            rx.text(
                "Without policy: we deduct IRDAI-listed non-payables and show the admissible baseline.",
                size="2", color_scheme="gray",
            ),
            spacing="2", align="start", width="100%",
        ),

        # Policy PDF
        rx.vstack(
            rx.hstack(
                rx.text("Policy Wording PDF", size="2", weight="medium"),
                rx.text("— optional but recommended", size="1", color_scheme="gray"),
                spacing="1", align="center",
            ),
            _policy_file_picker(),
            rx.text(
                "Enables: room rent cascade, sub-limits, co-pay, disease caps, exclusions.",
                size="1", color_scheme="gray",
            ),
            spacing="1", width="100%", align="start",
        ),

        # Sum insured + Top-up
        rx.hstack(
            rx.vstack(
                rx.hstack(
                    rx.text("Sum Insured (₹)", size="2", weight="medium"),
                    rx.text("optional", size="1", color_scheme="gray"),
                    spacing="1", align="center",
                ),
                rx.input(
                    placeholder="e.g. 500000  (leave blank to auto-detect from policy)",
                    value=BillState.sum_insured_input,
                    on_change=BillState.set_sum_insured,
                    type="text",
                    size="2",
                    width="100%",
                    id="sum-insured-input",
                ),
                spacing="1", flex="1",
            ),
            rx.vstack(
                rx.hstack(
                    rx.text("Top-Up / Super Top-Up (₹)", size="2", weight="medium"),
                    rx.text("optional", size="1", color_scheme="gray"),
                    spacing="1", align="center",
                ),
                rx.input(
                    placeholder="e.g. 2000000",
                    value=BillState.top_up_input,
                    on_change=BillState.set_top_up,
                    type="text",
                    size="2",
                    width="100%",
                ),
                spacing="1", flex="1",
            ),
            spacing="4", width="100%",
            style={"flexWrap": "wrap"},
        ),

        # Add-ons
        rx.vstack(
            rx.text("Add-ons / Riders", size="2", weight="medium"),
            rx.hstack(
                _addon_pill("OPD Rider", BillState.addon_opd, BillState.toggle_addon_opd),
                _addon_pill("Maternity", BillState.addon_maternity, BillState.toggle_addon_maternity),
                _addon_pill("Room Rent Waiver", BillState.addon_room_waiver, BillState.toggle_addon_room_waiver),
                _addon_pill("No Co-Pay", BillState.addon_no_copay, BillState.toggle_addon_no_copay),
                spacing="2", style={"flexWrap": "wrap"},
            ),
            spacing="2", width="100%", align="start",
        ),

        # Error
        rx.cond(
            BillState.claim_error != "",
            rx.callout(BillState.claim_error, icon="triangle-alert", color_scheme="red", size="1"),
            rx.fragment(),
        ),

        # Submit
        rx.hstack(
            rx.button(
                rx.icon(tag="shield-check", size=14),
                "Run Estimate",
                on_click=BillState.estimate_claim,
                size="2",
                color_scheme="blue",
                variant="solid",
                id="estimate-btn",
            ),
            rx.text(
                "Even without any inputs, we'll apply IRDAI baseline rules.",
                size="1", color_scheme="gray",
            ),
            spacing="3", align="center",
        ),

        spacing="4", align="start", width="100%",
        padding="20px",
        border="1px solid var(--gray-a4)",
        border_radius="14px",
        background="var(--gray-1)",
        max_width="700px",
    )
