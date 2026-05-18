"""WeasyPrint-based PDF export. Renders to bytes (preferred) or to disk."""
from __future__ import annotations
import re
from pathlib import Path
from datetime import date
from collections import OrderedDict
from jinja2 import Environment, FileSystemLoader, select_autoescape
try:
    from weasyprint import HTML
except (ImportError, OSError):
    HTML = None
from .models import BillAnalysis

_TPL_DIR = Path(__file__).resolve().parents[2] / "templates"
_env = Environment(loader=FileSystemLoader(str(_TPL_DIR)), autoescape=select_autoescape(["html"]))

_GARBAGE_RE = re.compile(r"[A-Za-z]{3,}")
def _is_garbage(desc: str) -> bool:
    if not desc or len(desc.strip()) < 4: return True
    if not _GARBAGE_RE.findall(desc): return True
    alnum = sum(c.isalnum() or c.isspace() for c in desc)
    return (alnum / max(len(desc), 1)) < 0.70

_GREY_MIN_RUPEES = 50.0

def _clean_desc(desc: str) -> str:
    if not desc: return ""
    s = re.sub(r"^\[[A-Z0-9.]+\]\s*", "", desc.strip())
    s = re.sub(r"\s*\([0-9/.\-]+\)\s*$", "", s)
    return s.strip()

def _dedup_by_canonical(items: list) -> list[dict]:
    groups: "OrderedDict[str, dict]" = OrderedDict()
    for it in items:
        key = (it.classification.canonical_name or it.raw.raw_description[:60]).strip()
        delta = 0.0; bench = 0.0
        for f in it.flags:
            if f.kind in ("overcharge","drug_mrp_violation"):
                delta = float(f.delta_amount or 0); bench = float(f.benchmark_amount or 0); break
        billed = float(it.raw.amount or 0)
        if key not in groups:
            groups[key] = {"canonical_name": key, "total_billed": billed, "total_delta": delta,
                           "total_benchmark": bench, "count": 1, "sample": it}
        else:
            g = groups[key]
            g["total_billed"] += billed; g["total_delta"] += delta
            g["total_benchmark"] += bench; g["count"] += 1
    out = list(groups.values())
    out.sort(key=lambda g: -g["total_delta"])
    return out

def _ctx(analysis: BillAnalysis) -> dict:
    clean_items = [it for it in analysis.items if not _is_garbage(it.raw.raw_description)]
    flagged = [it for it in clean_items if it.worst_severity in ("amber","red")]
    grey    = [it for it in clean_items if it.worst_severity == "grey" and (it.raw.amount or 0) >= _GREY_MIN_RUPEES]
    fair    = [it for it in clean_items if it.worst_severity == "green"]
    fair_total = sum(float(it.raw.amount or 0) for it in fair)
    def _delta(it):
        for f in it.flags:
            if f.kind in ("overcharge","drug_mrp_violation"):
                return float(f.delta_amount or 0)
        return 0.0
    flagged.sort(key=_delta, reverse=True)
    grey.sort(key=lambda it: -float(it.raw.amount or 0))
    fair.sort(key=lambda it: -float(it.raw.amount or 0))
    flagged_grouped = _dedup_by_canonical(flagged)
    return {
        "analysis": analysis,
        "flagged": flagged, "flagged_grouped": flagged_grouped,
        "grey": grey, "fair": fair, "fair_total": fair_total,
        "today": date.today().strftime("%d %B %Y"),
        "clean_desc": _clean_desc,
        "multi_hospital_warning": next(
            (line for line in (analysis.unparsed_lines or [])
             if line.startswith("⚠ Multiple hospital bills") or line.startswith("⚠ Multiple billing entities")),
            None,
        ),
    }

def render_pdf_bytes(kind: str, analysis: BillAnalysis, claim=None, policy=None) -> bytes:
    """Render PDF in-memory; preferred path. Avoids /_upload URL issues.

    Claim summary uses a ReportLab generator (pure-Python, Windows-friendly)
    so it never depends on GTK/Pango being installed. Other kinds still go
    through the WeasyPrint HTML pipeline.
    """
    if kind == "claim_summary":
        from . import claim_pdf
        return claim_pdf.render_claim_summary_pdf(analysis, claim, policy)

    tpl_name = f"{kind}.html"
    ctx = _ctx(analysis)
    html_str = _env.get_template(tpl_name).render(**ctx)
    if HTML is None:
        raise RuntimeError(
            "WeasyPrint could not load its native dependencies (GTK/Pango). "
            "Install the GTK3 runtime on Windows, or use the Claim Summary export "
            "which does not require it."
        )
    return HTML(string=html_str, base_url=str(_TPL_DIR)).write_pdf()

def render_pdf(kind: str, analysis: BillAnalysis, out_path: str | Path,
               claim=None, policy=None) -> Path:
    """Disk-write variant for back-compat / CLI / tests."""
    out_path = Path(out_path)
    out_path.write_bytes(render_pdf_bytes(kind, analysis, claim=claim, policy=policy))
    return out_path