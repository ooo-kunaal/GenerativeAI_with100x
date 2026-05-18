"""ReportLab-based PDF generator for the Claim Settlement Estimate.

Used instead of the WeasyPrint HTML pipeline so the export works on Windows
without GTK/Pango. Renders a clean, structured PDF directly from the
ClaimEstimate / PolicySummary models.
"""
from __future__ import annotations
import os
from datetime import date
from io import BytesIO
from typing import Optional

from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER, TA_LEFT, TA_RIGHT
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.units import mm
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle,
    KeepTogether, PageBreak,
)

from .models import BillAnalysis, ClaimEstimate, PolicySummary


# ── Unicode-capable font registration ────────────────────────────────────
# ReportLab's default Helvetica is WinAnsi-encoded and doesn't have a glyph
# for ₹ (U+20B9). Register a system TTF that supports the rupee sign so the
# PDF renders correctly. If nothing is found, _money() falls back to "Rs.".
_FONT_FAMILY = "Helvetica"
_FONT_BOLD = "Helvetica-Bold"
_FONT_ITALIC = "Helvetica-Oblique"
_FONT_SUPPORTS_RUPEE = False

def _try_register_unicode_font() -> None:
    global _FONT_FAMILY, _FONT_BOLD, _FONT_ITALIC, _FONT_SUPPORTS_RUPEE
    candidates = [
        # name, regular, bold, italic
        ("UIFont", "C:/Windows/Fonts/arial.ttf",  "C:/Windows/Fonts/arialbd.ttf",
                   "C:/Windows/Fonts/ariali.ttf"),
        ("UIFont", "C:/Windows/Fonts/segoeui.ttf", "C:/Windows/Fonts/segoeuib.ttf",
                   "C:/Windows/Fonts/segoeuii.ttf"),
        ("UIFont", "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
                   "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
                   "/usr/share/fonts/truetype/dejavu/DejaVuSans-Oblique.ttf"),
        ("UIFont", "/Library/Fonts/Arial.ttf", "/Library/Fonts/Arial Bold.ttf",
                   "/Library/Fonts/Arial Italic.ttf"),
    ]
    for name, regular, bold, italic in candidates:
        if not os.path.exists(regular):
            continue
        try:
            pdfmetrics.registerFont(TTFont(name, regular))
            bold_name = name + "-Bold" if os.path.exists(bold) else name
            italic_name = name + "-Italic" if os.path.exists(italic) else name
            if os.path.exists(bold):
                pdfmetrics.registerFont(TTFont(bold_name, bold))
            if os.path.exists(italic):
                pdfmetrics.registerFont(TTFont(italic_name, italic))
            _FONT_FAMILY = name
            _FONT_BOLD = bold_name
            _FONT_ITALIC = italic_name
            _FONT_SUPPORTS_RUPEE = True
            return
        except Exception:
            continue

_try_register_unicode_font()


# ── Style palette (matches the on-screen claim panel) ────────────────────
_BLUE   = colors.HexColor("#3b6fd4")
_GREEN  = colors.HexColor("#1f6f33")
_GREEN_BG = colors.HexColor("#e3f6e6")
_RED    = colors.HexColor("#a02020")
_RED_BG = colors.HexColor("#fde7e7")
_AMBER  = colors.HexColor("#b86b2a")
_AMBER_BG = colors.HexColor("#fff8e1")
_GREY   = colors.HexColor("#666666")
_BORDER = colors.HexColor("#dddddd")
_LIGHT_BG = colors.HexColor("#f6f6f6")
_TIP_BG = colors.HexColor("#f0f9f3")
_CASCADE_BG = colors.HexColor("#fffbf0")


def _styles():
    ss = getSampleStyleSheet()
    base = ParagraphStyle("base", parent=ss["Normal"], fontName=_FONT_FAMILY)
    return {
        "title": ParagraphStyle("title", parent=base, fontSize=18,
                                fontName=_FONT_BOLD,
                                textColor=colors.black, alignment=TA_LEFT,
                                spaceAfter=2),
        "subtitle": ParagraphStyle("subtitle", parent=base, fontSize=9,
                                   textColor=_GREY, alignment=TA_LEFT,
                                   spaceAfter=2),
        "subtitle_right": ParagraphStyle("subtitle_right", parent=base,
                                         fontSize=9, textColor=_GREY,
                                         alignment=TA_RIGHT),
        "h2": ParagraphStyle("h2", parent=base, fontSize=13,
                             fontName=_FONT_BOLD,
                             textColor=colors.black, spaceBefore=14, spaceAfter=6),
        "h3": ParagraphStyle("h3", parent=base, fontSize=11,
                             fontName=_FONT_BOLD,
                             textColor=colors.HexColor("#333333"),
                             spaceBefore=8, spaceAfter=4),
        "body": ParagraphStyle("body", parent=base, fontSize=10, leading=14),
        "small": ParagraphStyle("small", parent=base, fontSize=9,
                                textColor=_GREY, leading=12),
        "tip": ParagraphStyle("tip", parent=base, fontSize=9.5,
                              leading=13, leftIndent=8,
                              textColor=colors.HexColor("#1f4a35")),
        "warn": ParagraphStyle("warn", parent=base, fontSize=9.5,
                               leading=13, leftIndent=8,
                               textColor=colors.HexColor("#8a5a00")),
        "stat_label": ParagraphStyle("stat_label", parent=base, fontSize=8,
                                     textColor=_GREY, alignment=TA_CENTER,
                                     spaceAfter=2),
        "stat_val": ParagraphStyle("stat_val", parent=base, fontSize=15,
                                   fontName=_FONT_BOLD,
                                   alignment=TA_CENTER, leading=18),
        "stat_val_green": ParagraphStyle("stat_val_green", parent=base,
                                         fontSize=15, fontName=_FONT_BOLD,
                                         alignment=TA_CENTER,
                                         leading=18, textColor=_GREEN),
        "stat_val_red": ParagraphStyle("stat_val_red", parent=base,
                                       fontSize=15, fontName=_FONT_BOLD,
                                       alignment=TA_CENTER,
                                       leading=18, textColor=_RED),
        "deduct_label": ParagraphStyle("deduct_label", parent=base, fontSize=10),
        "deduct_amt": ParagraphStyle("deduct_amt", parent=base, fontSize=10,
                                     alignment=TA_RIGHT, textColor=_RED),
        "deduct_detail": ParagraphStyle("deduct_detail", parent=base,
                                        fontSize=9, textColor=_GREY, leading=12,
                                        leftIndent=10, fontName=_FONT_ITALIC),
    }


_RUPEE = "₹" if _FONT_SUPPORTS_RUPEE else "Rs. "

def _money(v: float) -> str:
    try:
        return f"{_RUPEE}{float(v):,.0f}"
    except Exception:
        return f"{_RUPEE}0"


def _header_table(analysis: BillAnalysis, policy: Optional[PolicySummary], st: dict) -> Table:
    today = date.today().strftime("%d %B %Y")
    hospital = (analysis.hospital.name or "").strip() or "Hospital"
    patient = (analysis.patient.name_redacted or "").strip() or ""
    policy_line = ""
    if policy:
        bits = []
        if policy.insurer: bits.append(policy.insurer)
        if policy.policy_name: bits.append(policy.policy_name)
        policy_line = " — ".join(bits)

    meta_lines = [hospital]
    if patient: meta_lines.append(f"{patient} · {today}")
    else: meta_lines.append(today)
    if policy_line: meta_lines.append(policy_line)
    meta_html = "<br/>".join(meta_lines)

    left = Paragraph("<b>BillShield — Claim Settlement Estimate</b>", st["title"])
    right = Paragraph(meta_html, st["subtitle_right"])
    tbl = Table([[left, right]], colWidths=[110*mm, 70*mm])
    tbl.setStyle(TableStyle([
        ("VALIGN", (0,0), (-1,-1), "BOTTOM"),
        ("LINEBELOW", (0,0), (-1,-1), 1.5, colors.black),
        ("BOTTOMPADDING", (0,0), (-1,-1), 6),
    ]))
    return tbl


def _big_stats(claim: ClaimEstimate, st: dict) -> Table:
    def _cell(label: str, val: str, style):
        return [
            Paragraph(label.upper(), st["stat_label"]),
            Paragraph(f"<b>{val}</b>", style),
        ]
    cells = [[
        _cell("Gross Bill", _money(claim.gross_billed), st["stat_val"]),
        _cell("Estimated Settlement", _money(claim.estimated_settlement), st["stat_val_green"]),
        _cell("Patient Liability", _money(claim.patient_liability), st["stat_val_red"]),
    ]]
    tbl = Table(cells, colWidths=[60*mm, 60*mm, 60*mm])
    tbl.setStyle(TableStyle([
        ("BOX", (0,0), (0,0), 0.6, _BORDER),
        ("BOX", (1,0), (1,0), 0.6, _BORDER),
        ("BOX", (2,0), (2,0), 0.6, _BORDER),
        ("BACKGROUND", (1,0), (1,0), colors.HexColor("#f4fbf5")),
        ("BACKGROUND", (2,0), (2,0), colors.HexColor("#fdf4f4")),
        ("VALIGN", (0,0), (-1,-1), "MIDDLE"),
        ("TOPPADDING", (0,0), (-1,-1), 8),
        ("BOTTOMPADDING", (0,0), (-1,-1), 8),
        ("LEFTPADDING", (0,0), (-1,-1), 8),
        ("RIGHTPADDING", (0,0), (-1,-1), 8),
    ]))
    return tbl


def _confidence_banner(claim: ClaimEstimate, st: dict):
    pc = (claim.policy_confidence or "low").lower()
    if pc == "low":
        text = ("⚠ Policy confidence: <b>Low</b> — some sub-limits may have been "
                "missed. Cross-check this estimate against your policy schedule of benefits.")
        bg = _AMBER_BG
    elif pc == "medium":
        text = ("ℹ Policy confidence: <b>Medium</b> — some parameters were inferred. "
                "Verify room rent and co-pay figures against your policy document.")
        bg = _AMBER_BG
    else:
        return None
    p = Paragraph(text, st["warn"])
    tbl = Table([[p]], colWidths=[180*mm])
    tbl.setStyle(TableStyle([
        ("BACKGROUND", (0,0), (-1,-1), bg),
        ("LEFTPADDING", (0,0), (-1,-1), 8),
        ("RIGHTPADDING", (0,0), (-1,-1), 8),
        ("TOPPADDING", (0,0), (-1,-1), 6),
        ("BOTTOMPADDING", (0,0), (-1,-1), 6),
        ("LINEBEFORE", (0,0), (0,-1), 3, _AMBER),
    ]))
    return tbl


def _cascade_box(claim: ClaimEstimate, st: dict):
    if not claim or claim.room_rent_cascade_ratio >= 0.99:
        return None
    ratio_pct = claim.room_rent_cascade_ratio * 100
    text = (
        f"<b>Room Rent Proportional Cascade Applied</b><br/>"
        f"You were billed {_money(claim.room_rent_billed)} for room rent, but your policy "
        f"allows {_money(claim.room_rent_allowed)}. Under IRDAI guidelines, all room-linked "
        f"charges (doctor visits, nursing, ward charges) were reduced proportionally to "
        f"<b>{ratio_pct:.0f}%</b> of billed. This rule does NOT apply to procedures, drugs, "
        f"or diagnostic tests."
    )
    p = Paragraph(text, st["body"])
    tbl = Table([[p]], colWidths=[180*mm])
    tbl.setStyle(TableStyle([
        ("BACKGROUND", (0,0), (-1,-1), _CASCADE_BG),
        ("BOX", (0,0), (-1,-1), 0.6, colors.HexColor("#e8c060")),
        ("LEFTPADDING", (0,0), (-1,-1), 10),
        ("RIGHTPADDING", (0,0), (-1,-1), 10),
        ("TOPPADDING", (0,0), (-1,-1), 8),
        ("BOTTOMPADDING", (0,0), (-1,-1), 8),
    ]))
    return tbl


def _waterfall(claim: ClaimEstimate, st: dict) -> Table:
    rows = []
    style_cmds = [
        ("VALIGN", (0,0), (-1,-1), "MIDDLE"),
        ("FONTNAME", (0,0), (-1,-1), _FONT_FAMILY),
        ("FONTSIZE", (0,0), (-1,-1), 10),
        ("LEFTPADDING", (0,0), (-1,-1), 8),
        ("RIGHTPADDING", (0,0), (-1,-1), 8),
        ("TOPPADDING", (0,0), (-1,-1), 5),
        ("BOTTOMPADDING", (0,0), (-1,-1), 5),
        ("ALIGN", (1,0), (1,-1), "RIGHT"),
    ]

    rows.append([Paragraph("<b>Gross Billed Amount</b>", st["deduct_label"]),
                 Paragraph(f"<b>{_money(claim.gross_billed)}</b>", st["body"])])
    style_cmds.append(("LINEBELOW", (0, len(rows)-1), (-1, len(rows)-1), 0.5, _BORDER))

    for d in (claim.deductions or []):
        rows.append([Paragraph(f"− {d.label}", st["deduct_label"]),
                     Paragraph(f"−{_money(d.amount)}", st["deduct_amt"])])
        idx = len(rows) - 1
        style_cmds.append(("BACKGROUND", (0, idx), (-1, idx), colors.HexColor("#fff8f0")))
        if d.explanation:
            rows.append([Paragraph(d.explanation, st["deduct_detail"]), ""])
            idx2 = len(rows) - 1
            style_cmds.append(("BACKGROUND", (0, idx2), (-1, idx2), colors.HexColor("#fff8f0")))
            style_cmds.append(("SPAN", (0, idx2), (1, idx2)))
        if d.items_affected:
            items_str = "; ".join(d.items_affected[:6])
            if len(d.items_affected) > 6:
                items_str += f"; +{len(d.items_affected)-6} more"
            rows.append([Paragraph(f"<i>Items: {items_str}</i>", st["deduct_detail"]), ""])
            idx3 = len(rows) - 1
            style_cmds.append(("BACKGROUND", (0, idx3), (-1, idx3), colors.HexColor("#fff8f0")))
            style_cmds.append(("SPAN", (0, idx3), (1, idx3)))
        style_cmds.append(("LINEBELOW", (0, len(rows)-1), (-1, len(rows)-1), 0.3, _BORDER))

    rows.append([Paragraph("<b>Total Deductions</b>", st["deduct_label"]),
                 Paragraph(f"<b>−{_money(claim.total_deductions)}</b>", st["deduct_amt"])])
    idx = len(rows) - 1
    style_cmds.append(("BACKGROUND", (0, idx), (-1, idx), _LIGHT_BG))
    style_cmds.append(("LINEABOVE", (0, idx), (-1, idx), 1.5, _BORDER))

    rows.append([Paragraph("<b>Estimated Insurance Settlement</b>", st["deduct_label"]),
                 Paragraph(f"<b>{_money(claim.estimated_settlement)}</b>",
                           ParagraphStyle("sg", parent=st["body"], textColor=_GREEN,
                                          alignment=TA_RIGHT, fontSize=11))])
    idx = len(rows) - 1
    style_cmds.append(("BACKGROUND", (0, idx), (-1, idx), _GREEN_BG))
    style_cmds.append(("TOPPADDING", (0, idx), (-1, idx), 7))
    style_cmds.append(("BOTTOMPADDING", (0, idx), (-1, idx), 7))

    tbl = Table(rows, colWidths=[125*mm, 55*mm])
    tbl.setStyle(TableStyle(style_cmds))
    return tbl


def _summary_table(claim: ClaimEstimate, st: dict) -> Table:
    rows = [["Item", "Amount (₹)"]]
    rows.append(["Gross Billed", f"{claim.gross_billed:,.0f}"])
    rows.append(["Total Deductions", f"-{claim.total_deductions:,.0f}"])
    rows.append(["Pre-Deductible Admissible", f"{claim.pre_deductible_admissible:,.0f}"])
    style_cmds = [
        ("BACKGROUND", (0,0), (-1,0), _LIGHT_BG),
        ("FONTNAME", (0,0), (-1,0), _FONT_BOLD),
        ("FONTSIZE", (0,0), (-1,-1), 9.5),
        ("ALIGN", (1,0), (1,-1), "RIGHT"),
        ("LINEBELOW", (0,0), (-1,-1), 0.3, _BORDER),
        ("VALIGN", (0,0), (-1,-1), "MIDDLE"),
        ("LEFTPADDING", (0,0), (-1,-1), 6),
        ("RIGHTPADDING", (0,0), (-1,-1), 6),
        ("TOPPADDING", (0,0), (-1,-1), 4),
        ("BOTTOMPADDING", (0,0), (-1,-1), 4),
        ("TEXTCOLOR", (1,2), (1,2), _RED),
    ]
    if claim.deductible_applied > 0:
        rows.append(["Fixed Deductible", f"-{claim.deductible_applied:,.0f}"])
        style_cmds.append(("TEXTCOLOR", (1, len(rows)-1), (1, len(rows)-1), _RED))
    if claim.co_pay_amount > 0:
        rows.append(["Co-Payment (patient share)", f"-{claim.co_pay_amount:,.0f}"])
        style_cmds.append(("TEXTCOLOR", (1, len(rows)-1), (1, len(rows)-1), _RED))

    rows.append(["Estimated Settlement (Insurer Pays)", f"{claim.estimated_settlement:,.0f}"])
    settle_row = len(rows) - 1
    style_cmds.append(("BACKGROUND", (0, settle_row), (-1, settle_row), _GREEN_BG))
    style_cmds.append(("FONTNAME", (0, settle_row), (-1, settle_row), _FONT_BOLD))
    style_cmds.append(("TEXTCOLOR", (1, settle_row), (1, settle_row), _GREEN))

    rows.append(["Patient Liability (Out-of-Pocket)", f"{claim.patient_liability:,.0f}"])
    liab_row = len(rows) - 1
    style_cmds.append(("BACKGROUND", (0, liab_row), (-1, liab_row), _RED_BG))
    style_cmds.append(("FONTNAME", (0, liab_row), (-1, liab_row), _FONT_BOLD))
    style_cmds.append(("TEXTCOLOR", (1, liab_row), (1, liab_row), _RED))

    style_cmds.insert(0, ("FONTNAME", (0,0), (-1,-1), _FONT_FAMILY))
    tbl = Table(rows, colWidths=[130*mm, 50*mm])
    tbl.setStyle(TableStyle(style_cmds))
    return tbl


def _top_up_box(claim: ClaimEstimate, st: dict):
    if not claim.top_up_triggered: return None
    text = (
        f"<b>Top-Up Triggered:</b> Your base policy ({_money(claim.sum_insured_used)}) is "
        f"exhausted. Your top-up policy contributes {_money(claim.top_up_contribution)}. "
        f"File a separate reimbursement claim with your top-up insurer."
    )
    p = Paragraph(text, st["tip"])
    tbl = Table([[p]], colWidths=[180*mm])
    tbl.setStyle(TableStyle([
        ("BACKGROUND", (0,0), (-1,-1), _TIP_BG),
        ("LINEBEFORE", (0,0), (0,-1), 3, _GREEN),
        ("LEFTPADDING", (0,0), (-1,-1), 8),
        ("RIGHTPADDING", (0,0), (-1,-1), 8),
        ("TOPPADDING", (0,0), (-1,-1), 6),
        ("BOTTOMPADDING", (0,0), (-1,-1), 6),
    ]))
    return tbl


def _tip_or_warn_row(text: str, kind: str, st: dict) -> Table:
    icon = "Tip:" if kind == "tip" else "Warning:"
    bg = _TIP_BG if kind == "tip" else _AMBER_BG
    line = _GREEN if kind == "tip" else _AMBER
    style = st["tip"] if kind == "tip" else st["warn"]
    p = Paragraph(f"<b>{icon}</b> {text}", style)
    tbl = Table([[p]], colWidths=[180*mm])
    tbl.setStyle(TableStyle([
        ("BACKGROUND", (0,0), (-1,-1), bg),
        ("LINEBEFORE", (0,0), (0,-1), 3, line),
        ("LEFTPADDING", (0,0), (-1,-1), 8),
        ("RIGHTPADDING", (0,0), (-1,-1), 8),
        ("TOPPADDING", (0,0), (-1,-1), 5),
        ("BOTTOMPADDING", (0,0), (-1,-1), 5),
    ]))
    return tbl


def _policy_params_table(policy: PolicySummary, st: dict) -> Table:
    rows = [["Parameter", "Value"]]
    rows.append(["Insurer", policy.insurer or "Unknown"])
    rows.append(["Policy Type", policy.policy_type or "unknown"])
    if policy.sum_insured:
        rows.append(["Sum Insured", _money(policy.sum_insured)])
    if policy.room_rent_waiver:
        rows.append(["Room Rent", "Waived (any room covered)"])
    elif policy.room_rent_sub_limit_pct:
        rows.append(["Room Rent Sub-Limit",
                     f"{policy.room_rent_sub_limit_pct}% of SI per day"])
    elif policy.room_rent_sub_limit_abs:
        rows.append(["Room Rent Sub-Limit",
                     f"{_money(policy.room_rent_sub_limit_abs)} per day"])
    else:
        rows.append(["Room Rent", "Not specified in policy"])
    if policy.co_pay_pct:
        rows.append(["Co-Payment", f"{policy.co_pay_pct}%"])
    if policy.deductible_amount:
        rows.append(["Deductible", _money(policy.deductible_amount)])
    rows.append(["Extraction Confidence", (policy.confidence or "low").capitalize()])

    tbl = Table(rows, colWidths=[60*mm, 120*mm])
    tbl.setStyle(TableStyle([
        ("FONTNAME", (0,0), (-1,-1), _FONT_FAMILY),
        ("BACKGROUND", (0,0), (-1,0), _LIGHT_BG),
        ("FONTNAME", (0,0), (-1,0), _FONT_BOLD),
        ("FONTSIZE", (0,0), (-1,-1), 9.5),
        ("LINEBELOW", (0,0), (-1,-1), 0.3, _BORDER),
        ("VALIGN", (0,0), (-1,-1), "MIDDLE"),
        ("LEFTPADDING", (0,0), (-1,-1), 6),
        ("RIGHTPADDING", (0,0), (-1,-1), 6),
        ("TOPPADDING", (0,0), (-1,-1), 4),
        ("BOTTOMPADDING", (0,0), (-1,-1), 4),
    ]))
    return tbl


def _disease_sublimits_table(policy: PolicySummary, st: dict):
    if not policy.disease_sub_limits: return None
    rows = [["Disease / Procedure", "Cap (₹)", "Notes"]]
    for d in policy.disease_sub_limits:
        rows.append([d.disease or "", f"{(d.cap_amount or 0):,.0f}", d.notes or ""])
    tbl = Table(rows, colWidths=[70*mm, 35*mm, 75*mm])
    tbl.setStyle(TableStyle([
        ("FONTNAME", (0,0), (-1,-1), _FONT_FAMILY),
        ("BACKGROUND", (0,0), (-1,0), _LIGHT_BG),
        ("FONTNAME", (0,0), (-1,0), _FONT_BOLD),
        ("FONTSIZE", (0,0), (-1,-1), 9),
        ("ALIGN", (1,0), (1,-1), "RIGHT"),
        ("LINEBELOW", (0,0), (-1,-1), 0.3, _BORDER),
        ("VALIGN", (0,0), (-1,-1), "TOP"),
        ("LEFTPADDING", (0,0), (-1,-1), 6),
        ("RIGHTPADDING", (0,0), (-1,-1), 6),
        ("TOPPADDING", (0,0), (-1,-1), 4),
        ("BOTTOMPADDING", (0,0), (-1,-1), 4),
    ]))
    return tbl


def render_claim_summary_pdf(
    analysis: BillAnalysis,
    claim: Optional[ClaimEstimate],
    policy: Optional[PolicySummary] = None,
) -> bytes:
    """Render the Claim Settlement Estimate as a real PDF (ReportLab)."""
    buf = BytesIO()
    doc = SimpleDocTemplate(
        buf, pagesize=A4,
        leftMargin=16*mm, rightMargin=16*mm,
        topMargin=14*mm, bottomMargin=16*mm,
        title="BillShield - Claim Settlement Estimate",
    )
    st = _styles()
    story: list = []

    story.append(_header_table(analysis, policy, st))
    story.append(Spacer(1, 6))

    if claim is None:
        story.append(Paragraph(
            "No claim estimate is available. Please run a claim estimation first "
            "from the Insurance tab on the dashboard.",
            st["body"],
        ))
        doc.build(story, onFirstPage=_footer, onLaterPages=_footer)
        return buf.getvalue()

    banner = _confidence_banner(claim, st)
    if banner is not None:
        story.append(banner)
        story.append(Spacer(1, 6))

    story.append(_big_stats(claim, st))
    story.append(Spacer(1, 8))

    cascade = _cascade_box(claim, st)
    if cascade is not None:
        story.append(cascade)
        story.append(Spacer(1, 6))

    story.append(Paragraph("Deduction Breakdown", st["h2"]))
    story.append(_waterfall(claim, st))

    top_up = _top_up_box(claim, st)
    if top_up is not None:
        story.append(Spacer(1, 6))
        story.append(top_up)

    story.append(Paragraph("Summary", st["h2"]))
    story.append(_summary_table(claim, st))

    if claim.tips:
        story.append(Paragraph("Claim Tips", st["h2"]))
        for tip in claim.tips:
            story.append(_tip_or_warn_row(str(tip), "tip", st))
            story.append(Spacer(1, 3))

    if claim.warnings:
        story.append(Spacer(1, 4))
        for w in claim.warnings:
            story.append(_tip_or_warn_row(str(w), "warn", st))
            story.append(Spacer(1, 3))

    if policy is not None:
        story.append(Paragraph("Policy Parameters Used", st["h2"]))
        story.append(_policy_params_table(policy, st))
        dst = _disease_sublimits_table(policy, st)
        if dst is not None:
            story.append(Paragraph("Disease Sub-Limits", st["h3"]))
            story.append(dst)
        if policy.exclusions_summary:
            story.append(Paragraph("Key Exclusions (from policy)", st["h3"]))
            for ex in policy.exclusions_summary:
                story.append(Paragraph(f"• {ex}", st["small"]))

    story.append(Spacer(1, 14))
    story.append(Paragraph(
        "This claim estimate is generated by BillShield using AI and public data sources "
        "(IRDAI non-payable circular IRDAI/HLT/CIR/MISC/253/11/2019). It is indicative "
        "only. Actual settlement depends on the TPA, claim documents, and insurer "
        "discretion. This is not legal or financial advice. Always verify against your "
        "policy documents and consult your insurer.",
        st["small"],
    ))

    doc.build(story, onFirstPage=_footer, onLaterPages=_footer)
    return buf.getvalue()


def _footer(canvas, doc):
    canvas.saveState()
    canvas.setFont("Helvetica", 8)
    canvas.setFillColor(_GREY)
    page_num = canvas.getPageNumber()
    canvas.drawRightString(A4[0] - 16*mm, 10*mm, f"{page_num}")
    canvas.restoreState()
