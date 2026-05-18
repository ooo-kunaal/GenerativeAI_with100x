"""Layered KB retrieval. Loads CSVs once at import time. Given a classified
line item, returns a Benchmark dict (matched or not), with citation if matched.

Layers (in order):
  1. Alias table: exact-string match on raw_description against procedure_aliases.csv
  2. Fuzzy: rapidfuzz partial-token match on canonical_name against the relevant KB table
  3. None: returns matched=False with top-3 candidates for the dashboard to show

We never invent rates. A row whose `rate_*_tier` is "TBD_VERIFY" returns
matched=True (alias found) but benchmark_amount=None — the flag layer treats
that as "unbenchmarkable, mapping known"."""
from __future__ import annotations
from pathlib import Path
from functools import lru_cache
import pandas as pd
from rapidfuzz import fuzz, process
from .models import Benchmark

_KB_DIR = Path(__file__).resolve().parents[2] / "kb"

# table_name -> path
_TABLES = {
    "cghs_procedures":  _KB_DIR / "cghs_procedures.csv",
    "cghs_diagnostics": _KB_DIR / "cghs_diagnostics.csv",
    "cghs_room_rent":   _KB_DIR / "cghs_room_rent.csv",
    "nppa_drugs":       _KB_DIR / "nppa_drugs.csv",
    "jan_aushadhi":     _KB_DIR / "jan_aushadhi.csv",
    "consumables":      _KB_DIR / "consumables.csv",
}

_FUZZY_THRESHOLD = 70  # 0-100
_TIER_COL = {"X":"rate_x_tier","Y":"rate_y_tier","Z":"rate_z_tier"}

@lru_cache(maxsize=1)
def _load_aliases() -> pd.DataFrame:
    df = pd.read_csv(_KB_DIR / "procedure_aliases.csv")
    return df.where(pd.notna(df), None)

@lru_cache(maxsize=1)
def _load_table(name: str) -> pd.DataFrame:
    df = pd.read_csv(_TABLES[name])
    return df.where(pd.notna(df), None)

def _coerce_rate(v) -> float | None:
    if pd.isna(v) or str(v).strip().upper() == "TBD_VERIFY": return None
    try: return float(v)
    except (TypeError, ValueError): return None

def _build_benchmark_from_row(row: dict, table: str, city_tier: str, method: str, score: float | None) -> Benchmark:
    """Pull the right rate column from a KB row based on city tier, build a Benchmark."""
    rate: float | None = None
    if table.startswith("cghs_") and table != "cghs_room_rent":
        col = _TIER_COL.get(city_tier, "rate_x_tier")
        rate = _coerce_rate(row.get(col))
    elif table == "cghs_room_rent":
        rate = _coerce_rate(row.get("nabh_rate"))
    elif table == "nppa_drugs":
        rate = _coerce_rate(row.get("ceiling_price_per_unit"))
    elif table == "jan_aushadhi":
        rate = _coerce_rate(row.get("jan_aushadhi_mrp"))
    elif table == "consumables":
        # consumables have a range; we surface the max as a soft ceiling but mark non-authoritative
        rate = _coerce_rate(row.get("indicative_retail_max"))
    citation_om = row.get("source_om") or row.get("source_so") or row.get("source")
    citation_date = row.get("source_date")
    return Benchmark(
        matched=True,
        source_table=table,
        canonical_name=row.get("canonical_name") or row.get("molecule") or row.get("item") or row.get("room_type"),
        benchmark_amount=rate,
        citation_om=citation_om,
        citation_date=citation_date,
        match_method=method,  # type: ignore
        match_score=score,
        notes=row.get("notes"),
    )

def lookup(classification: dict, raw_description: str, city_tier: str = "X") -> Benchmark:
    """Layered lookup. classification has keys: category, canonical_name, needs_benchmark."""
    if not classification.get("needs_benchmark"):
        return Benchmark(matched=False, match_method="none", notes="Classifier marked as not needing benchmark")

    # 1) alias table — exact substring match on raw_description (uppercase normalised)
    aliases = _load_aliases()
    raw_up = (raw_description or "").upper()
    for _, arow in aliases.iterrows():
        pat = str(arow["free_text_pattern"]).upper()
        if pat and pat in raw_up:
            tname = arow["kb_table"]
            if tname == "unbenchmarkable":
                return Benchmark(matched=False, match_method="alias", notes=f"Alias mapped to no-benchmark category ({arow['canonical_name']})")
            if tname not in _TABLES:
                continue
            tbl = _load_table(tname)
            canon = arow["canonical_name"]
            # find that canonical row
            name_col = "canonical_name" if "canonical_name" in tbl.columns else ("molecule" if "molecule" in tbl.columns else ("item" if "item" in tbl.columns else "room_type"))
            hit = tbl[tbl[name_col].astype(str).str.strip() == str(canon).strip()]
            if not hit.empty:
                return _build_benchmark_from_row(hit.iloc[0].to_dict(), tname, city_tier, "alias", 100.0)

    # 2) fuzzy against each table's canonical_name column
    canon = (classification.get("canonical_name") or "").strip()
    if canon:
        best_table, best_row, best_score = None, None, 0.0
        for tname in ("cghs_procedures","cghs_diagnostics","cghs_room_rent","nppa_drugs","consumables"):
            tbl = _load_table(tname)
            name_col = "canonical_name" if "canonical_name" in tbl.columns else ("room_type" if "room_type" in tbl.columns else ("molecule" if "molecule" in tbl.columns else ("item" if "item" in tbl.columns else None)))
            if not name_col: continue
            choices = tbl[name_col].astype(str).tolist()
            result = process.extractOne(canon, choices, scorer=fuzz.token_set_ratio)
            if result and result[1] > best_score:
                best_score = float(result[1])
                best_table = tname
                best_row = tbl.iloc[result[2]].to_dict()
        if best_row is not None and best_score >= _FUZZY_THRESHOLD:
            return _build_benchmark_from_row(best_row, best_table, city_tier, "fuzzy", best_score)
        # below threshold — return ambiguous with top-3 candidates
        cands = _top_candidates(canon)
        return Benchmark(matched=False, match_method="none", candidates=cands, notes=f"Best fuzzy score {best_score:.0f} below threshold {_FUZZY_THRESHOLD}")

    return Benchmark(matched=False, match_method="none", notes="No canonical_name to fuzzy-match")

def _top_candidates(canon: str, n: int = 3, min_score: float = 50.0) -> list[dict]:
    """Return top-n candidates with score >= min_score. Returns empty list if
    no candidate is even remotely similar — better to show no candidates
    than nonsense ones."""
    pool: list[tuple[str,str,float]] = []
    for tname in ("cghs_procedures","cghs_diagnostics","nppa_drugs"):
        tbl = _load_table(tname)
        name_col = "canonical_name" if "canonical_name" in tbl.columns else "molecule"
        for name in tbl[name_col].astype(str).tolist():
            score = float(fuzz.token_set_ratio(canon, name))
            if score >= min_score:
                pool.append((tname, name, score))
    pool.sort(key=lambda x: x[2], reverse=True)
    return [{"table": t, "name": n, "score": s} for t,n,s in pool[:n]]