"""Single-section line-items view, gated by BillState.selected_kpi."""
import reflex as rx
from ..state.bill_state import BillState

def _evidence_block(flag: rx.Var) -> rx.Component:
    return rx.cond(
        flag.kind == "stale_benchmark", rx.fragment(),
        rx.box(
            rx.vstack(
                rx.text(flag.message, size="2"),
                rx.cond(flag.citation,
                    rx.text("Source: ", rx.text(flag.citation, as_="span", style={"fontStyle":"italic"}),
                            size="1", color_scheme="gray"),
                    rx.fragment()),
                spacing="2", align="start",
            ),
            padding="10px 12px", border_left="3px solid var(--gray-a6)",
            background="var(--gray-a2)", border_radius="0 6px 6px 0", width="100%",
        ),
    )

def _drilldown(item: rx.Var) -> rx.Component:
    return rx.box(
        rx.vstack(rx.foreach(item.flags, _evidence_block),
                  spacing="3", align="start", width="100%"),
        padding="12px 18px 16px 24px", background="var(--gray-a1)", width="100%",
        style={"animation":"slideDown 180ms ease-out"},
    )

def _kebab(item_id: rx.Var) -> rx.Component:
    return rx.menu.root(
        rx.menu.trigger(
            rx.icon_button(rx.icon(tag="ellipsis", size=15),
                           variant="ghost", size="1", color_scheme="gray",
                           on_click=rx.stop_propagation),
        ),
        rx.menu.content(
            rx.menu.item(rx.hstack(rx.icon(tag="check", size=14), rx.text("Mark resolved", size="2"), spacing="2"),
                         on_click=BillState.resolve_item(item_id)),
            rx.menu.item(rx.hstack(rx.icon(tag="pencil", size=14), rx.text("Resolve with note…", size="2"), spacing="2"),
                         on_click=BillState.open_note_modal(item_id)),
        ),
    )

def _delta_chip(item: rx.Var) -> rx.Component:
    primary = item.flags[0]
    return rx.cond(
        primary.delta_amount > 0,
        rx.text("+₹" + primary.delta_amount.to(int).to_string(),
                size="1", weight="bold",
                style={"color":"#c8553d","background":"#fbeae5","padding":"2px 8px",
                       "borderRadius":"999px","fontVariantNumeric":"tabular-nums","whiteSpace":"nowrap"}),
        rx.fragment(),
    )

def _row(item: rx.Var) -> rx.Component:
    item_id = item.raw.id
    is_expanded = BillState.expanded_item_id == item_id
    primary = item.flags[0]
    has_bench = primary.benchmark_amount > 0
    return rx.vstack(
        rx.hstack(
            rx.text(item.classification.canonical_name, size="2",
                    style={"flex":"1","minWidth":"0","overflow":"hidden","textOverflow":"ellipsis"}),
            rx.hstack(
                rx.text("₹" + item.raw.amount.to(int).to_string(), size="2", weight="medium",
                        style={"fontVariantNumeric":"tabular-nums"}),
                rx.cond(has_bench,
                    rx.hstack(
                        rx.icon(tag="arrow-right", size=12, color="var(--gray-9)"),
                        rx.text("₹" + primary.benchmark_amount.to(int).to_string(),
                                size="2", color_scheme="gray",
                                style={"fontVariantNumeric":"tabular-nums"}),
                        spacing="1", align="center",
                    ),
                    rx.fragment(),
                ),
                _delta_chip(item),
                spacing="2", align="center",
            ),
            _kebab(item_id),
            rx.icon(tag=rx.cond(is_expanded, "chevron-up", "chevron-down"),
                    size=14, color="var(--gray-9)"),
            spacing="3", align="center", padding="12px 18px",
            cursor="pointer", on_click=BillState.toggle_expand(item_id),
            _hover={"background":"var(--gray-a2)"}, width="100%",
            style={"transition":"background 120ms ease"},
        ),
        rx.cond(is_expanded, _drilldown(item), rx.fragment()),
        spacing="0", border_bottom="1px solid var(--gray-a3)", width="100%",
    )

def _list_for(items: rx.Var) -> rx.Component:
    return rx.cond(
        items.length() == 0,
        rx.box(rx.text("No items in this category.", size="2", color_scheme="gray"),
               padding="32px", style={"textAlign":"center"}),
        rx.foreach(items, _row),
    )

def _top_disputes_view() -> rx.Component:
    return rx.box(
        rx.vstack(
            rx.text("Your three biggest wins to dispute first.", size="1", color_scheme="gray",
                    padding="14px 18px 6px 18px"),
            _list_for(BillState.top_dispute_items),
            spacing="0", width="100%",
        ),
        width="100%",
    )

def flag_table() -> rx.Component:
    return rx.box(
        rx.cond(BillState.selected_kpi == "top_disputes", _top_disputes_view(),
        rx.cond(BillState.selected_kpi == "red", _list_for(BillState.red_items_all),
        rx.cond(BillState.selected_kpi == "amber", _list_for(BillState.amber_items_all),
        rx.cond(BillState.selected_kpi == "grey", _list_for(BillState.grey_items),
        _list_for(BillState.green_items_all))))),
        border="1px solid var(--gray-a4)", border_radius="12px",
        overflow="hidden", background="var(--color-panel-solid)",
        width="100%",
        style={"animation":"fadeIn 220ms ease-out"},
    )