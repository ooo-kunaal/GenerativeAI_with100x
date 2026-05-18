"""Chat state: holds the message history and dispatches a turn to the bounded
chat service. Reads the analysis from BillState."""
from __future__ import annotations
import asyncio
import reflex as rx
from ..services import chat as chat_service
from ..services.models import BillAnalysis
from .bill_state import BillState

class ChatState(rx.State):
    history: list[dict[str, str]] = []
    input_value: str = ""
    is_thinking: bool = False
    is_open: bool = False

    @rx.event
    def toggle_open(self):
        self.is_open = not self.is_open

    @rx.event
    def set_input(self, val: str):
        self.input_value = val

    @rx.event
    def handle_key_down(self, key: str):
        if key == "Enter":
            return ChatState.send()

    @rx.event(background=True)
    async def send(self):
        async with self:
            msg = self.input_value.strip()
            if not msg or self.is_thinking:
                return
            self.history = self.history + [{"role": "user", "content": msg}]
            self.input_value = ""
            self.is_thinking = True
            # capture snapshots while we hold the lock
            _msg = msg
            _prior = list(self.history[:-1])

        # we need the analysis from BillState
        bill = await self.get_state(BillState)
        _has_result = bool(bill.has_result)
        _raw_analysis = bill.analysis
        _analysis_dict = {k: v for k, v in _raw_analysis.items()} if _raw_analysis else {}
        if not _has_result:
            async with self:
                self.history = self.history + [{"role": "assistant", "content": "Please analyse a bill first, then I can help with questions about it."}]
                self.is_thinking = False
            return

        try:
            analysis = BillAnalysis(**_analysis_dict)
            reply_text = await asyncio.to_thread(chat_service.reply, analysis, _prior, _msg)
            async with self:
                self.history = self.history + [{"role": "assistant", "content": reply_text}]
                self.is_thinking = False
        except Exception as exc:
            import traceback
            traceback.print_exc()
            async with self:
                self.history = self.history + [{"role": "assistant", "content": f"(Error contacting model: {exc})"}]
                self.is_thinking = False

    @rx.event
    def clear(self):
        self.history = []
