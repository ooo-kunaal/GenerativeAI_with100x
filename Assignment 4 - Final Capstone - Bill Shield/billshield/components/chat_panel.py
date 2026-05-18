"""Bounded chat panel — collapsible. Wired to BillState (chat fields merged in)."""
import reflex as rx
from ..state.bill_state import BillState

def _bubble(msg: rx.Var) -> rx.Component:
    is_user = msg["role"] == "user"
    return rx.box(
        rx.text(msg["content"], size="2", style={"whiteSpace":"pre-wrap"}),
        padding="8px 12px",
        border_radius="10px",
        background=rx.cond(is_user, "var(--accent-3)", "var(--gray-a3)"),
        align_self=rx.cond(is_user, "flex-end", "flex-start"),
        max_width="88%",
    )

def _closed_button() -> rx.Component:
    return rx.box(
        rx.hstack(
            rx.icon(tag="message-circle", size=16, color="var(--accent-10)"),
            rx.text("Ask about this bill or any line item",
                    size="2", color_scheme="gray", style={"flex":"1"}),
            rx.icon(tag="chevron-up", size=14, color="var(--gray-9)"),
            spacing="3", align="center",
            padding="14px 20px",
        ),
        on_click=BillState.chat_toggle_open, cursor="pointer",
        style={
            "position":"fixed","bottom":"0","left":"0","right":"0","zIndex":"20",
            "background":"var(--color-panel-solid)",
            "borderTop":"1px solid var(--gray-a4)",
            "boxShadow":"0 -4px 14px rgba(0,0,0,0.04)",
            "transition":"all 180ms ease",
        },
        _hover={"background":"var(--gray-a2)"},
    )

def _open_panel() -> rx.Component:
    return rx.box(
        rx.vstack(
            rx.hstack(
                rx.text("Clarification chat", weight="bold", size="3"),
                rx.spacer(),
                rx.button(rx.icon(tag="x", size=16), variant="ghost", on_click=BillState.chat_toggle_open, size="1"),
                width="100%", align="center",
            ),
            rx.divider(),
            rx.box(
                rx.cond(
                    BillState.chat_history.length() == 0,
                    rx.text("Ask why a line item was flagged, what to do next, or to draft a follow-up email. I can only refer to facts from the analysis on the left.",
                            size="2", color_scheme="gray"),
                    rx.vstack(
                        rx.foreach(BillState.chat_history, _bubble),
                        rx.cond(BillState.chat_is_thinking, rx.text("…thinking", size="1", color_scheme="gray"), rx.fragment()),
                        spacing="2", align="stretch", width="100%",
                    ),
                ),
                flex="1", overflow_y="auto", padding="8px 2px", width="100%",
            ),
            rx.hstack(
                rx.input(
                    placeholder="Type a question…",
                    value=BillState.chat_input,
                    on_change=BillState.chat_set_input,
                    on_key_down=BillState.chat_handle_key_down,
                    flex="1", size="2",
                ),
                rx.button("Send", on_click=BillState.chat_send, loading=BillState.chat_is_thinking, size="2"),
                width="100%", spacing="2",
            ),
            spacing="3", align="stretch",
            height="100%",
        ),
        style={
            "position":"fixed","bottom":"20px","right":"20px","zIndex":"30",
            "width":"380px","height":"540px",
            "background":"var(--color-panel-solid)",
            "border":"1px solid var(--gray-a5)","borderRadius":"12px",
            "padding":"14px 14px 12px 14px","boxShadow":"0 10px 30px rgba(0,0,0,0.18)",
        },
    )

def chat_panel() -> rx.Component:
    return rx.cond(BillState.chat_is_open, _open_panel(), _closed_button())