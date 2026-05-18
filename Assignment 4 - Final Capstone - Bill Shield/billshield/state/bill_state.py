"""Reflex state for upload + analysis + chat + resolve."""
from __future__ import annotations
import asyncio, json, os, tempfile, traceback
from pathlib import Path
from typing import Any
import reflex as rx
from ..services import pipeline, export, chat as chat_service, policy_parse, claim_engine
from ..services.models import BillAnalysis, AnalysedLineItem, PolicySummary, ClaimEstimate

def _detach(v):
    return json.loads(json.dumps(v)) if v else v

_CAT_LABEL = {
    "consultation":"Doctor visits","diagnostic":"Tests & lab work","drug":"Medicines","consumable":"Supplies",
    "room":"Room & procedures","procedure":"Room & procedures","package":"Room & procedures",
    "anaesthesia":"Room & procedures","registration":"Other charges","misc":"Other charges",
}
def _cat_key(c: str) -> str:
    return {"consultation":"doctors","diagnostic":"tests","drug":"meds","consumable":"supplies",
            "room":"room","procedure":"room","package":"room","anaesthesia":"room",
            "registration":"other","misc":"other"}.get(c, "other")

class BillState(rx.State):
    uploaded_filename: str = ""
    is_analysing: bool = False
    progress_message: str = ""
    error_message: str = ""
    has_result: bool = False
    analysis: dict[str, Any] = {}
    expanded_item_id: str = ""
    is_exporting: bool = False
    export_kind: str = ""
    export_modal_open: bool = False
    resolved: dict[str, dict[str, Any]] = {}
    note_modal_open: bool = False
    note_modal_item_id: str = ""
    note_modal_input: str = ""
    last_export_filename: str = ""
    chat_history: list[dict[str, str]] = []
    chat_input: str = ""
    chat_is_thinking: bool = False
    chat_is_open: bool = False

    # ── Insurance / Claim estimation inputs ──────────────────────────────
    insurance_enabled: bool = False
    policy_filename: str = ""
    sum_insured_input: str = ""        # kept as string for input binding
    top_up_input: str = ""             # top-up / super top-up
    addon_opd: bool = False
    addon_maternity: bool = False
    addon_room_waiver: bool = False
    addon_no_copay: bool = False

    # ── Insurance / Claim estimation results ─────────────────────────────
    policy_data: dict[str, Any] = {}   # PolicySummary.model_dump()
    claim_data: dict[str, Any] = {}    # ClaimEstimate.model_dump()
    has_claim_estimate: bool = False
    is_estimating_claim: bool = False
    claim_error: str = ""

    # ── Dashboard tab ─────────────────────────────────────────────────
    dashboard_tab: str = "bill"

    @rx.event
    def set_dashboard_tab(self, tab: str):
        self.dashboard_tab = tab

    @rx.event
    def reset_claim_estimate(self):
        """Clear results so the user can re-enter inputs."""
        self.has_claim_estimate = False
        self.claim_data = {}
        self.claim_error = ""

    @rx.event
    async def handle_upload(self, files: list[rx.UploadFile]):
        if not files: self.error_message = "No file selected."; return
        f = files[0]
        if not f.name or not f.name.lower().endswith(".pdf"):
            self.error_message = "Please upload a PDF."; return
        data = await f.read()
        upload_dir = rx.get_upload_dir(); upload_dir.mkdir(parents=True, exist_ok=True)
        path = upload_dir / f.name; path.write_bytes(data)
        self.uploaded_filename = f.name; self.error_message = ""

    @rx.event
    async def handle_policy_upload(self, files: list[rx.UploadFile]):
        if not files: return
        f = files[0]
        if not f.name or not f.name.lower().endswith(".pdf"):
            self.error_message = "Policy must be a PDF."; return
        data = await f.read()
        upload_dir = rx.get_upload_dir(); upload_dir.mkdir(parents=True, exist_ok=True)
        path = upload_dir / f.name; path.write_bytes(data)
        self.policy_filename = f.name; self.error_message = ""

    @rx.event
    def toggle_insurance(self):
        self.insurance_enabled = not self.insurance_enabled
        if not self.insurance_enabled:
            self.policy_filename = ""
            self.has_claim_estimate = False
            self.claim_data = {}

    @rx.event
    def set_insurance_enabled(self, val: bool):
        """Called by the Switch on_change with the new boolean value."""
        self.insurance_enabled = val
        if not val:
            self.policy_filename = ""
            self.has_claim_estimate = False
            self.claim_data = {}

    @rx.event
    def set_sum_insured(self, val: str): self.sum_insured_input = val
    @rx.event
    def set_top_up(self, val: str): self.top_up_input = val
    @rx.event
    def toggle_addon_opd(self): self.addon_opd = not self.addon_opd
    @rx.event
    def toggle_addon_maternity(self): self.addon_maternity = not self.addon_maternity
    @rx.event
    def toggle_addon_room_waiver(self):
        self.addon_room_waiver = not self.addon_room_waiver
    @rx.event
    def toggle_addon_no_copay(self): self.addon_no_copay = not self.addon_no_copay

    @rx.event(background=True)
    async def analyse(self):
        async with self:
            if not self.uploaded_filename: self.error_message = "Upload a PDF first."; return
            self.is_analysing = True; self.has_result = False; self.resolved = {}
            self.error_message = ""; self.progress_message = "Starting analysis…"
        pdf_path = rx.get_upload_dir() / self.uploaded_filename
        import queue
        progress_q: queue.Queue[str] = queue.Queue()
        def _on_progress(msg: str): progress_q.put(msg)
        import concurrent.futures
        with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool:
            future = pool.submit(pipeline.analyse, pdf_path, _on_progress)
            import time
            start = time.time()
            TIMEOUT_SEC = 600
            while not future.done():
                if time.time() - start > TIMEOUT_SEC:
                    future.cancel()
                    async with self:
                        self.error_message = "Analysis took too long (over 10 min). Try a smaller bill or split multi-page bills."
                        self.is_analysing = False
                    return
                await asyncio.sleep(0.3)
                while not progress_q.empty():
                    msg = progress_q.get_nowait()
                    async with self: self.progress_message = msg
            while not progress_q.empty():
                msg = progress_q.get_nowait()
                async with self: self.progress_message = msg
        try:
            result: BillAnalysis = future.result()
            async with self:
                self.analysis = result.model_dump()
                self.has_result = True
                self.progress_message = "Done — redirecting…"
                self.is_analysing = False
                return rx.redirect("/dashboard")
        except Exception as exc:
            traceback.print_exc()
            async with self:
                self.error_message = f"Analysis failed: {exc}"; self.is_analysing = False

    @rx.event
    def toggle_expand(self, item_id: str):
        self.expanded_item_id = "" if self.expanded_item_id == item_id else item_id

    @rx.event
    def resolve_item(self, item_id: str):
        self.resolved = {**self.resolved, item_id: {"resolved": True, "note": ""}}

    @rx.event
    def unresolve_item(self, item_id: str):
        new = dict(self.resolved); new.pop(item_id, None); self.resolved = new

    @rx.event
    def open_note_modal(self, item_id: str):
        self.note_modal_item_id = item_id
        self.note_modal_input = self.resolved.get(item_id, {}).get("note", "")
        self.note_modal_open = True

    @rx.event
    def close_note_modal(self):
        self.note_modal_open = False; self.note_modal_item_id = ""; self.note_modal_input = ""

    @rx.event
    def set_note_input(self, val: str):
        self.note_modal_input = val

    @rx.event
    def save_note_and_resolve(self):
        if self.note_modal_item_id:
            self.resolved = {**self.resolved, self.note_modal_item_id: {"resolved": True, "note": self.note_modal_input}}
        self.close_note_modal()

    @rx.event
    def toggle_export_modal(self):
        self.export_modal_open = not self.export_modal_open

    @rx.event(background=True)
    async def export_pdf(self, kind: str):
        """Render PDF to bytes in-memory and stream directly to browser.
        Avoids /_upload URL serving issues entirely."""
        if kind not in ("summary","hospital_letter","full_pack","claim_summary"): return
        async with self:
            self.is_exporting = True; self.export_kind = kind
            _analysis_dict = _detach(self.analysis)
            _resolved_ids = set(k for k,v in self.resolved.items() if v.get("resolved"))
            _claim_dict = _detach(self.claim_data)
            _policy_dict = _detach(self.policy_data)
        try:
            if kind == "claim_summary":
                from ..services.models import ClaimEstimate, PolicySummary
                claim = ClaimEstimate(**_claim_dict) if _claim_dict else None
                policy = PolicySummary(**_policy_dict) if _policy_dict else None
                _analysis_dict["items"] = [
                    it for it in _analysis_dict.get("items", [])
                    if (it.get("raw",{}) or {}).get("id") not in _resolved_ids
                ]
                analysis = BillAnalysis(**_analysis_dict)
                pdf_bytes = await asyncio.to_thread(
                    export.render_pdf_bytes, "claim_summary", analysis,
                    claim=claim, policy=policy,
                )
            else:
                _analysis_dict["items"] = [
                    it for it in _analysis_dict.get("items", [])
                    if (it.get("raw",{}) or {}).get("id") not in _resolved_ids
                ]
                analysis = BillAnalysis(**_analysis_dict)
                pdf_bytes = await asyncio.to_thread(export.render_pdf_bytes, kind, analysis)
            filename = f"billshield_{kind}.pdf"
            async with self:
                self.is_exporting = False
                self.last_export_filename = filename
                self.export_modal_open = False
            return rx.download(data=pdf_bytes, filename=filename)
        except Exception as exc:
            traceback.print_exc()
            async with self:
                self.error_message = f"Export failed: {exc}"; self.is_exporting = False
                self.export_modal_open = False

    @rx.event
    def chat_toggle_open(self): self.chat_is_open = not self.chat_is_open
    @rx.event
    def chat_set_input(self, val: str): self.chat_input = val
    @rx.event
    def chat_handle_key_down(self, key: str):
        if key == "Enter": return BillState.chat_send()

    @rx.event(background=True)
    async def chat_send(self):
        async with self:
            msg = self.chat_input.strip()
            if not msg or self.chat_is_thinking: return
            if not self.has_result:
                self.chat_history = self.chat_history + [{"role":"assistant","content":"Please analyse a bill first."}]
                self.chat_input = ""; return
            self.chat_history = self.chat_history + [{"role":"user","content":msg}]
            self.chat_input = ""; self.chat_is_thinking = True
            _msg = str(msg)
            _prior = _detach(list(self.chat_history[:-1]))
            _analysis_dict = _detach(self.analysis)
        try:
            analysis = BillAnalysis(**_analysis_dict)
            reply_text = await asyncio.to_thread(chat_service.reply, analysis, _prior, _msg)
            async with self:
                self.chat_history = self.chat_history + [{"role":"assistant","content":reply_text}]
                self.chat_is_thinking = False
        except Exception as exc:
            traceback.print_exc()
            async with self:
                self.chat_history = self.chat_history + [{"role":"assistant","content":f"(Error contacting model: {exc})"}]
                self.chat_is_thinking = False

    @rx.event
    def chat_clear(self): self.chat_history = []

    def _items_unresolved(self) -> list[dict]:
        if not self.analysis: return []
        rid = set(k for k,v in (self.resolved or {}).items() if v.get("resolved"))
        return [it for it in self.analysis.get("items", []) if (it.get("raw",{}) or {}).get("id") not in rid]

    def _items_resolved(self) -> list[dict]:
        if not self.analysis: return []
        rid = set(k for k,v in (self.resolved or {}).items() if v.get("resolved"))
        return [it for it in self.analysis.get("items", []) if (it.get("raw",{}) or {}).get("id") in rid]

    @rx.var
    def total_billed(self) -> str:
        if not self.analysis: return "₹ 0"
        v = float(self.analysis.get("totals",{}).get("net_amount") or self.analysis.get("totals",{}).get("gross_amount") or 0.0)
        return f"₹ {v:,.0f}"

    @rx.var
    def total_billed_raw(self) -> float:
        if not self.analysis: return 0.0
        return float(self.analysis.get("totals",{}).get("net_amount") or self.analysis.get("totals",{}).get("gross_amount") or 0.0)

    @rx.var
    def _fair_raw(self) -> float:
        return sum(float(it.get("raw",{}).get("amount") or 0) for it in self._items_unresolved() if it.get("worst_severity")=="green")
    @rx.var
    def _flagged_raw(self) -> float:
        return sum(float(it.get("raw",{}).get("amount") or 0) for it in self._items_unresolved() if it.get("worst_severity") in ("amber","red"))
    @rx.var
    def _grey_raw(self) -> float:
        return sum(float(it.get("raw",{}).get("amount") or 0) for it in self._items_unresolved() if it.get("worst_severity")=="grey")

    @rx.var
    def fair_amount(self) -> str: return f"₹ {self._fair_raw:,.0f}"
    @rx.var
    def flagged_amount(self) -> str: return f"₹ {self._flagged_raw:,.0f}"
    @rx.var
    def unverifiable_amount(self) -> str: return f"₹ {self._grey_raw:,.0f}"

    @rx.var
    def disputable_amount_raw(self) -> float:
        total = 0.0
        for it in self._items_unresolved():
            if it.get("worst_severity") not in ("red","amber"): continue
            for f in it.get("flags",[]):
                if f.get("kind") in ("overcharge","drug_mrp_violation"):
                    d = f.get("delta_amount")
                    if d: total += float(d)
                    break
        return total

    selected_kpi: str = "top_disputes"  # "top_disputes" | "red" | "amber" | "grey" | "green"

    @rx.event
    def select_kpi(self, key: str):
        self.selected_kpi = key

    @rx.var
    def all_notices(self) -> list[str]:
        if not self.analysis: return []
        return [l for l in (self.analysis.get("unparsed_lines") or [])
                if l.startswith(("⚠","ℹ"))]

    @rx.var
    def notices_count(self) -> int:
        return len(self.all_notices)

    @rx.var
    def top_disputes(self) -> list[dict]:
        """Top 3 line items by delta_amount — the user's highest-leverage fixes."""
        scored = []
        for it in self._items_unresolved():
            if it.get("worst_severity") not in ("red","amber"): continue
            delta = 0.0
            for f in it.get("flags",[]):
                if f.get("kind") in ("overcharge","drug_mrp_violation"):
                    delta = float(f.get("delta_amount") or 0.0); break
            if delta <= 0: continue
            cls = it.get("classification") or {}
            scored.append({
                "name": cls.get("canonical_name") or (it.get("raw",{}).get("raw_description") or "")[:60],
                "delta_str": f"+₹{delta:,.0f}",
                "delta_raw": delta,
            })
        scored.sort(key=lambda x: -x["delta_raw"])
        return scored[:3]

    @rx.var
    def has_top_disputes(self) -> bool:
        return len(self.top_disputes) > 0

    @rx.var
    def disputable_amount(self) -> str: return f"₹ {self.disputable_amount_raw:,.0f}"

    @rx.var
    def disputable_pct(self) -> str:
        billed = self.total_billed_raw
        if billed <= 0: return "0%"
        return f"{(self.disputable_amount_raw / billed * 100):.0f}%"

    @rx.var
    def confidence_label(self) -> str:
        if not self.analysis: return "Low"
        items = self._items_unresolved()
        verified_ru = sum(float(it.get("raw",{}).get("amount") or 0) for it in items if it.get("worst_severity") in ("green","amber","red"))
        billed = self.total_billed_raw
        if billed <= 0: return "Low"
        pct = verified_ru / billed
        if pct >= 0.70: return "High"
        if pct >= 0.40: return "Medium"
        return "Low"

    @rx.var
    def confidence_sentence(self) -> str:
        """Tier 8: hard-cited vs AI-estimated vs unverified breakdown."""
        if not self.analysis: return ""
        billed = self.total_billed_raw
        if billed <= 0: return ""
        hard_cited = 0.0; ai_estimated = 0.0; unverified = 0.0
        for it in self._items_unresolved():
            amt = float(it.get("raw",{}).get("amount") or 0)
            sev = it.get("worst_severity","grey")
            has_ai = any((f.get("citation") or "").startswith("AI reasonableness") for f in it.get("flags",[]))
            if sev == "grey": unverified += amt
            elif has_ai:      ai_estimated += amt
            else:             hard_cited += amt
        pct_hard = hard_cited / billed * 100
        pct_ai = ai_estimated / billed * 100
        pct_un = unverified / billed * 100
        return (f"Of your ₹{billed:,.0f} bill, we cited government rates for ₹{hard_cited:,.0f} ({pct_hard:.0f}%), "
                f"used AI estimation for ₹{ai_estimated:,.0f} ({pct_ai:.0f}%), and ₹{unverified:,.0f} ({pct_un:.0f}%) "
                f"remains unverifiable without more information.")

    @rx.var
    def items_verified_count(self) -> str:
        items = self._items_unresolved()
        total = len(items)
        v = sum(1 for it in items if it.get("worst_severity") in ("green","amber","red"))
        return f"{v} of {total}"

    @rx.var
    def hospital_name(self) -> str:
        return self.analysis.get("hospital", {}).get("name", "") if self.analysis else ""
    @rx.var
    def patient_name(self) -> str:
        return self.analysis.get("patient", {}).get("name_redacted", "") if self.analysis else ""
    @rx.var
    def encounter_summary(self) -> str:
        if not self.analysis: return ""
        e = self.analysis.get("encounter", {})
        bits = [e.get("type") or "Encounter"]
        if e.get("admission_date"): bits.append(f"admitted {e['admission_date']}")
        if e.get("discharge_date"): bits.append(f"discharged {e['discharge_date']}")
        return " · ".join(bits)

    @rx.var
    def multi_bill_warning(self) -> str:
        if not self.analysis: return ""
        for line in (self.analysis.get("unparsed_lines") or []):
            if line.startswith("⚠ Multiple hospital bills") or line.startswith("⚠ Multiple billing entities"):
                return line
        return ""

    @rx.var
    def totals_mismatch_warning(self) -> str:
        if not self.analysis: return ""
        for line in (self.analysis.get("unparsed_lines") or []):
            if line.startswith("ℹ The bill total used") or line.startswith("⚠ Extracted total"):
                return line
        return ""

    @rx.var
    def headline_sentence(self) -> str:
        name = self.patient_name.split()[0] if self.patient_name else "Your"
        return f"{name}, your ₹{self.total_billed_raw:,.0f} bill has ₹{self.disputable_amount_raw:,.0f} that looks disputable ({self.disputable_pct})."

    def _bucket(self, sev: str, cat_key: str) -> list[dict]:
        out = []
        for it in self._items_unresolved():
            if it.get("worst_severity") != sev: continue
            raw_cat = (it.get("classification",{}) or {}).get("category","misc")
            if _cat_key(raw_cat) != cat_key: continue
            out.append(it)
        def _amt(it):
            for f in it.get("flags",[]):
                if f.get("kind") in ("overcharge","drug_mrp_violation"):
                    return float(f.get("delta_amount") or 0.0)
            return float(it.get("raw",{}).get("amount") or 0.0)
        out.sort(key=lambda x: -_amt(x))
        return out

    @rx.var
    def top_disputes_total_str(self) -> str:
        t = sum(d.get("delta_raw", 0) for d in self.top_disputes)
        return f"{t:,.0f}"

    @rx.var
    def top_disputes_count_str(self) -> str:
        return str(len(self.top_disputes))

    @rx.var
    def red_total_str(self) -> str:
        t = sum(float(it.get("raw",{}).get("amount") or 0)
                for it in self._items_unresolved() if it.get("worst_severity") == "red")
        return f"{t:,.0f}"

    @rx.var
    def amber_total_str(self) -> str:
        t = sum(float(it.get("raw",{}).get("amount") or 0)
                for it in self._items_unresolved() if it.get("worst_severity") == "amber")
        return f"{t:,.0f}"

    @rx.var
    def grey_total_str(self) -> str:
        t = sum(float(it.get("raw",{}).get("amount") or 0)
                for it in self._items_unresolved() if it.get("worst_severity") == "grey")
        return f"{t:,.0f}"

    @rx.var
    def green_total_str(self) -> str:
        t = sum(float(it.get("raw",{}).get("amount") or 0)
                for it in self._items_unresolved() if it.get("worst_severity") == "green")
        return f"{t:,.0f}"

    @rx.var
    def red_items_all(self) -> list[AnalysedLineItem]:
        out = [it for it in self._items_unresolved() if it.get("worst_severity") == "red"]
        return [AnalysedLineItem(**it) for it in out]

    @rx.var
    def amber_items_all(self) -> list[AnalysedLineItem]:
        out = [it for it in self._items_unresolved() if it.get("worst_severity") == "amber"]
        return [AnalysedLineItem(**it) for it in out]

    @rx.var
    def green_items_all(self) -> list[AnalysedLineItem]:
        out = [it for it in self._items_unresolved() if it.get("worst_severity") == "green"]
        return [AnalysedLineItem(**it) for it in out]

    @rx.var
    def top_dispute_items(self) -> list[AnalysedLineItem]:
        scored = []
        for it in self._items_unresolved():
            if it.get("worst_severity") not in ("red","amber"): continue
            delta = 0.0
            for f in it.get("flags",[]):
                if f.get("kind") in ("overcharge","drug_mrp_violation"):
                    delta = float(f.get("delta_amount") or 0); break
            if delta > 0: scored.append((delta, it))
        scored.sort(key=lambda x: -x[0])
        return [AnalysedLineItem(**it) for _, it in scored[:3]]

    @rx.var
    def red_doctors(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("red","doctors")]
    @rx.var
    def red_tests(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("red","tests")]
    @rx.var
    def red_meds(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("red","meds")]
    @rx.var
    def red_supplies(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("red","supplies")]
    @rx.var
    def red_room(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("red","room")]
    @rx.var
    def red_other(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("red","other")]
    @rx.var
    def red_total_count(self) -> int:
        return sum(1 for it in self._items_unresolved() if it.get("worst_severity")=="red")

    @rx.var
    def amber_doctors(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("amber","doctors")]
    @rx.var
    def amber_tests(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("amber","tests")]
    @rx.var
    def amber_meds(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("amber","meds")]
    @rx.var
    def amber_supplies(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("amber","supplies")]
    @rx.var
    def amber_room(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("amber","room")]
    @rx.var
    def amber_other(self) -> list[AnalysedLineItem]: return [AnalysedLineItem(**it) for it in self._bucket("amber","other")]
    @rx.var
    def amber_total_count(self) -> int:
        return sum(1 for it in self._items_unresolved() if it.get("worst_severity")=="amber")

    @rx.var
    def grey_items(self) -> list[AnalysedLineItem]:
        out = [it for it in self._items_unresolved() if it.get("worst_severity")=="grey"]
        out.sort(key=lambda x: -float(x.get("raw",{}).get("amount") or 0))
        return [AnalysedLineItem(**it) for it in out]
    @rx.var
    def grey_count(self) -> int:
        return sum(1 for it in self._items_unresolved() if it.get("worst_severity")=="grey")

    @rx.var
    def green_count(self) -> int:
        return sum(1 for it in self._items_unresolved() if it.get("worst_severity")=="green")
    @rx.var
    def green_categories(self) -> str:
        cats = set()
        for it in self._items_unresolved():
            if it.get("worst_severity") != "green": continue
            c = (it.get("classification",{}) or {}).get("category","misc")
            cats.add(_CAT_LABEL.get(c, c))
        order = ["Doctor visits","Tests & lab work","Medicines","Supplies","Room & procedures","Other charges"]
        return ", ".join(c for c in order if c in cats)

    @rx.var
    def resolved_items(self) -> list[AnalysedLineItem]:
        return [AnalysedLineItem(**it) for it in self._items_resolved()]
    @rx.var
    def resolved_count(self) -> int:
        return len(self._items_resolved())
    @rx.var
    def resolved_notes(self) -> dict[str, str]:
        return {k: (v.get("note") or "") for k,v in (self.resolved or {}).items() if v.get("resolved")}

    @rx.var
    def problematic_items(self) -> list[AnalysedLineItem]:
        out = []
        for it in self._items_unresolved():
            sev = it.get("worst_severity","grey")
            amount = float(it.get("raw",{}).get("amount") or 0.0)
            if sev in ("red","amber") or (sev=="grey" and amount >= 100):
                out.append(it)
        order = {"red":0,"amber":1,"grey":2,"green":3}
        out.sort(key=lambda i: (order.get(i.get("worst_severity","grey"),9), -float(i.get("raw",{}).get("amount") or 0)))
        return [AnalysedLineItem(**it) for it in out]

    # ── Claim Estimation ────────────────────────────────────────────────────

    @rx.event(background=True)
    async def estimate_claim(self):
        """Parse the policy PDF and run the claim estimation engine.

        Sum-insured policy: prefer the user's input. If blank, fall back to
        the SI extracted from the uploaded policy. Only error if neither
        source provides a number.
        """
        async with self:
            if not self.has_result:
                self.claim_error = "Analyse the bill first."; return
            si_str = self.sum_insured_input.strip().replace(",","").replace("₹","")
            si_user = None
            if si_str and si_str.replace(".","").isdigit():
                si_user = float(si_str)
            elif not self.policy_filename:
                # No policy to fall back on.
                self.claim_error = "Enter a sum insured (or upload your policy PDF and we'll read it)."
                return
            self.is_estimating_claim = True
            self.has_claim_estimate = False
            self.claim_error = ""
            _analysis_dict = _detach(self.analysis)
            _policy_fn = str(self.policy_filename)
            _si_user = si_user
            _topup_str = self.top_up_input.strip().replace(",","").replace("₹","")
            _topup = float(_topup_str) if _topup_str and _topup_str.replace(".","").isdigit() else 0.0
            _room_waiver = bool(self.addon_room_waiver)
            _no_copay = bool(self.addon_no_copay)
            _maternity = bool(self.addon_maternity)
            _opd = bool(self.addon_opd)

        def _run():
            import queue
            q: queue.Queue[str] = queue.Queue()
            # Parse policy if provided
            if _policy_fn:
                pdf_path = rx.get_upload_dir() / _policy_fn
                policy = policy_parse.parse_policy(pdf_path, on_progress=q.put)
            else:
                from ..services.models import PolicySummary
                policy = PolicySummary()
            # Apply user-declared add-on overrides
            if _room_waiver:
                policy = policy.model_copy(update={"room_rent_waiver": True,
                                                   "room_rent_sub_limit_pct": None,
                                                   "room_rent_sub_limit_abs": None})
            if _no_copay:
                policy = policy.model_copy(update={"co_pay_pct": None})
            if _maternity:
                policy = policy.model_copy(update={"maternity_covered": True})
            if _opd:
                policy = policy.model_copy(update={"opd_covered": True})
            analysis = BillAnalysis(**_analysis_dict)
            # Resolve effective SI: user value wins; otherwise use parsed policy SI.
            si_effective = _si_user if _si_user is not None else (policy.sum_insured or 0.0)
            if not si_effective or si_effective <= 0:
                raise ValueError(
                    "We couldn't find a Sum Insured in your policy. Please enter it manually."
                )
            estimate = claim_engine.estimate_claim(analysis, policy, si_effective, _topup,
                                                   policy_was_uploaded=bool(_policy_fn))
            return policy, estimate, si_effective

        import concurrent.futures
        try:
            with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool:
                future = pool.submit(_run)
                import time; start = time.time()
                while not future.done():
                    if time.time() - start > 300: future.cancel(); break
                    await asyncio.sleep(0.4)
            policy_obj, estimate_obj, si_effective = future.result()
            async with self:
                self.policy_data = policy_obj.model_dump()
                self.claim_data = estimate_obj.model_dump()
                # Reflect the effective SI into the input box so the user
                # sees what we used (especially when auto-detected).
                if not self.sum_insured_input.strip():
                    self.sum_insured_input = f"{si_effective:,.0f}"
                self.has_claim_estimate = True
                self.is_estimating_claim = False
        except Exception as exc:
            traceback.print_exc()
            async with self:
                self.claim_error = f"Claim estimation failed: {exc}"
                self.is_estimating_claim = False

    # ── Claim panel computed vars ────────────────────────────────────────────

    def _cd(self) -> dict:
        return self.claim_data or {}

    @rx.var
    def claim_gross_str(self) -> str:
        return f"{float(self._cd().get('gross_billed', 0)):,.0f}"

    @rx.var
    def claim_settlement_str(self) -> str:
        return f"{float(self._cd().get('estimated_settlement', 0)):,.0f}"

    @rx.var
    def claim_liability_str(self) -> str:
        return f"{float(self._cd().get('patient_liability', 0)):,.0f}"

    @rx.var
    def claim_total_deductions_str(self) -> str:
        return f"{float(self._cd().get('total_deductions', 0)):,.0f}"

    @rx.var
    def claim_si_str(self) -> str:
        si_str = self.sum_insured_input.strip().replace(",","").replace("₹","")
        try: return f"{float(si_str):,.0f}"
        except: return "0"

    @rx.var
    def claim_top_up_str(self) -> str:
        return f"{float(self._cd().get('top_up_contribution', 0)):,.0f}"

    @rx.var
    def claim_top_up_triggered(self) -> bool:
        return bool(self._cd().get("top_up_triggered", False))

    @rx.var
    def claim_has_cascade(self) -> bool:
        return float(self._cd().get("room_rent_cascade_ratio", 1.0)) < 0.99

    @rx.var
    def claim_room_billed_str(self) -> str:
        return f"{float(self._cd().get('room_rent_billed', 0)):,.0f}"

    @rx.var
    def claim_room_allowed_str(self) -> str:
        return f"{float(self._cd().get('room_rent_allowed', 0)):,.0f}"

    @rx.var
    def claim_cascade_pct(self) -> str:
        r = float(self._cd().get("room_rent_cascade_ratio", 1.0))
        return f"{r*100:.0f}%"

    @rx.var
    def claim_policy_confidence(self) -> str:
        return self._cd().get("policy_confidence", "low")

    @rx.var
    def claim_policy_label(self) -> str:
        pd_ = self.policy_data or {}
        insurer = pd_.get("insurer") or ""
        name = pd_.get("policy_name") or ""
        if insurer and name: return f"{insurer} — {name}"
        if insurer: return insurer
        if self.policy_filename: return self.policy_filename
        return "No policy uploaded — sum insured only"

    @rx.var
    def claim_deductions(self) -> list[dict]:
        """Return deductions as display-ready dicts with amount_str."""
        out = []
        for d in (self._cd().get("deductions") or []):
            out.append({
                "label": d.get("label",""),
                "amount_str": f"{float(d.get('amount',0)):,.0f}",
                "explanation": d.get("explanation",""),
                "kind": d.get("kind",""),
            })
        return out

    @rx.var
    def claim_tips(self) -> list[str]:
        return list(self._cd().get("tips") or [])

    @rx.var
    def claim_warnings(self) -> list[str]:
        return list(self._cd().get("warnings") or [])

    @rx.var
    def claim_has_tips(self) -> bool:
        return len(self.claim_tips) > 0

    @rx.var
    def claim_has_warnings(self) -> bool:
        return len(self.claim_warnings) > 0

    # ── "What we read from your policy" view vars ─────────────────────────
    # These let the user verify the parser pulled the right values rather
    # than trusting a single confidence badge.

    def _pd(self) -> dict:
        return self.policy_data or {}

    @rx.var
    def policy_was_uploaded(self) -> bool:
        return bool(self.policy_filename)

    @rx.var
    def policy_insurer_str(self) -> str:
        return str(self._pd().get("insurer") or "Unknown")

    @rx.var
    def policy_name_str(self) -> str:
        return str(self._pd().get("policy_name") or "")

    @rx.var
    def policy_type_str(self) -> str:
        t = str(self._pd().get("policy_type") or "unknown")
        return {"GMC": "Group Mediclaim (GMC)", "individual": "Individual",
                "family_floater": "Family Floater", "top_up": "Top-Up",
                "super_top_up": "Super Top-Up", "unknown": "Unknown"}.get(t, t)

    @rx.var
    def policy_sum_insured_str(self) -> str:
        v = self._pd().get("sum_insured")
        if v is None: return "Not stated"
        try: return f"₹ {float(v):,.0f}"
        except: return "Not stated"

    @rx.var
    def policy_room_rent_str(self) -> str:
        p = self._pd()
        if p.get("room_rent_waiver"):
            return "Waived (any room covered)"
        if p.get("room_rent_sub_limit_abs"):
            return f"₹ {float(p['room_rent_sub_limit_abs']):,.0f}/day"
        if p.get("room_rent_sub_limit_pct"):
            return f"{float(p['room_rent_sub_limit_pct']):.2f}% of SI/day"
        if p.get("room_type_description"):
            return f"By type: {p['room_type_description']} (proportionate deduction if upgraded)"
        return "Not stated"

    @rx.var
    def policy_icu_str(self) -> str:
        p = self._pd()
        if p.get("icu_sub_limit_abs"):
            return f"₹ {float(p['icu_sub_limit_abs']):,.0f}/day"
        if p.get("icu_sub_limit_pct"):
            return f"{float(p['icu_sub_limit_pct']):.2f}% of SI/day"
        return "No ICU sub-limit"

    @rx.var
    def policy_copay_str(self) -> str:
        v = self._pd().get("co_pay_pct")
        if v is None or float(v) == 0: return "None"
        return f"{float(v):.0f}%"

    @rx.var
    def policy_deductible_str(self) -> str:
        v = self._pd().get("deductible_amount")
        if v is None or float(v) == 0: return "None"
        return f"₹ {float(v):,.0f}"

    @rx.var
    def policy_waiting_str(self) -> str:
        p = self._pd()
        ped = p.get("pre_existing_waiting_days")
        init = p.get("initial_waiting_days")
        parts = []
        if ped is not None:
            parts.append("Pre-existing: waived" if int(ped) == 0 else f"Pre-existing: {int(ped)} days")
        if init is not None:
            parts.append("Initial: waived" if int(init) == 0 else f"Initial: {int(init)} days")
        return " · ".join(parts) if parts else "Not stated"

    @rx.var
    def policy_prepost_str(self) -> str:
        p = self._pd()
        pre = p.get("pre_hospitalization_days")
        post = p.get("post_hospitalization_days")
        if pre is None and post is None: return "Not stated"
        return f"{int(pre) if pre else 0} days pre / {int(post) if post else 0} days post"

    @rx.var
    def policy_disease_caps(self) -> list[dict]:
        out = []
        for d in (self._pd().get("disease_sub_limits") or []):
            try:
                out.append({
                    "disease": str(d.get("disease","")),
                    "cap_str": f"₹ {float(d.get('cap_amount') or 0):,.0f}",
                    "notes": str(d.get("notes") or ""),
                })
            except Exception:
                continue
        return out

    @rx.var
    def policy_has_disease_caps(self) -> bool:
        return len(self.policy_disease_caps) > 0

    @rx.var
    def policy_exclusions(self) -> list[str]:
        return [str(x) for x in (self._pd().get("exclusions_summary") or [])]

    @rx.var
    def policy_has_exclusions(self) -> bool:
        return len(self.policy_exclusions) > 0

    @rx.var
    def policy_extra_note(self) -> str:
        return str(self._pd().get("non_payable_note") or "")

    @rx.var
    def policy_has_extra_note(self) -> bool:
        return len(self.policy_extra_note) > 0