"""PDF/ZIP ingestion with OCR caching, parallel page OCR, and lower DPI.

Speed levers applied (in priority of impact):
  1. Cache by SHA-256 of file bytes → re-uploading the same bill is instant.
  2. OCR pages in parallel (ThreadPoolExecutor, 4 workers).
  3. Render images at 150 DPI instead of 200 DPI (~2× faster, OCR quality fine for
     printed bill text). Drop to 130 if a page is still legible.
  4. PaddleOCR singleton (we already had this) with MKLDNN disabled on Windows."""
from __future__ import annotations
from pathlib import Path
from typing import TypedDict, Callable, Optional
import hashlib, io, os, tempfile, zipfile, concurrent.futures
import pdfplumber
import pytesseract

pytesseract.pytesseract.tesseract_cmd = r"C:\Program Files\Tesseract-OCR\tesseract.exe"

_OCR_CACHE = Path(tempfile.gettempdir()) / "billshield_ocr_cache"
_OCR_CACHE.mkdir(parents=True, exist_ok=True)
_OCR_DPI = 150
_OCR_WORKERS = 4
class IngestResult(TypedDict):
    text: str
    method: str
    pages: int
    digital_chars: int
    ocr_chars: int

MIN_DIGITAL_CHARS_PER_PAGE = 100

def _file_sha(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()

def _sniff(path: Path) -> str:
    with open(path, "rb") as fh:
        head = fh.read(4)
    if head.startswith(b"%PDF"): return "pdf"
    if head.startswith(b"PK\x03\x04") or head.startswith(b"PK\x05\x06"): return "zip"
    return "unknown"

def ingest(pdf_path: str | Path, on_progress: Optional[Callable[[str], None]] = None) -> IngestResult:
    pdf_path = Path(pdf_path)
    if not pdf_path.exists(): raise FileNotFoundError(pdf_path)
    def _say(m: str):
        if on_progress: on_progress(m)

    # Cache check (covers all branches below)
    sha = _file_sha(pdf_path)
    cache_path = _OCR_CACHE / f"{sha}.txt"
    if cache_path.exists():
        _say("Found cached extraction — skipping OCR…")
        text = cache_path.read_text(encoding="utf-8", errors="ignore")
        return IngestResult(text=text, method="cache", pages=text.count("\n\n")+1,
                            digital_chars=len(text), ocr_chars=0)

    kind = _sniff(pdf_path)
    if kind == "zip":
        _say("Detected scanned-image bundle (ZIP of JPEGs). Running OCR…")
        result = _ingest_zip_of_images(pdf_path, _say)
    elif kind == "unknown":
        raise ValueError("This file isn't a recognised PDF or scanned-image bundle.")
    else:
        result = _ingest_real_pdf(pdf_path, _say)

    # Save cache
    try: cache_path.write_text(result["text"], encoding="utf-8")
    except Exception: pass
    return result

def _ingest_real_pdf(pdf_path: Path, say: Callable[[str], None]) -> IngestResult:
    digital_pages: list[str] = []
    pages_needing_ocr: list[int] = []
    with pdfplumber.open(pdf_path) as pdf:
        n_pages = len(pdf.pages)
        for i, page in enumerate(pdf.pages):
            text = page.extract_text() or ""
            digital_pages.append(text)
            if len(text.strip()) < MIN_DIGITAL_CHARS_PER_PAGE:
                pages_needing_ocr.append(i)
    digital_text = "\n\n".join(digital_pages)
    digital_chars = len(digital_text.strip())
    ocr_text = ""
    if pages_needing_ocr:
        say(f"OCR'ing {len(pages_needing_ocr)} page(s) in parallel…")
        ocr_results = _ocr_pdf_pages_parallel(pdf_path, pages_needing_ocr, say)
        for idx, ocr_block in ocr_results.items():
            if ocr_block.strip(): digital_pages[idx] = ocr_block
        digital_text = "\n\n".join(digital_pages)
        ocr_text = "\n".join(ocr_results.values())
    if pages_needing_ocr and digital_chars > 0: method = "mixed"
    elif pages_needing_ocr: method = "tesseract"
    else: method = "pdfplumber"
    return IngestResult(text=digital_text, method=method, pages=n_pages,
                        digital_chars=digital_chars, ocr_chars=len(ocr_text.strip()))

def _ingest_zip_of_images(zip_path: Path, say: Callable[[str], None]) -> IngestResult:
    """Extract JPEGs, OCR them all in parallel."""
    from PIL import Image
    with zipfile.ZipFile(zip_path) as zf:
        img_names = sorted(
            [n for n in zf.namelist() if n.lower().endswith((".jpeg",".jpg",".png"))],
            key=lambda n: (len(n), n),
        )
        if not img_names: raise ValueError("Scanned-image bundle has no readable image pages.")
        say(f"Extracting {len(img_names)} pages…")
        image_bytes = {i: zf.read(name) for i, name in enumerate(img_names)}

    completed = {"n": 0}
    total = len(image_bytes)
    def _ocr_one(i: int, data: bytes) -> tuple[int, str]:
        pil = Image.open(io.BytesIO(data)).convert("RGB")
        text = pytesseract.image_to_string(pil, lang="eng")
        completed["n"] += 1
        say(f"OCR page {completed['n']}/{total}…")
        return i, text

    out: dict[int, str] = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=_OCR_WORKERS) as pool:
        for i, txt in pool.map(lambda kv: _ocr_one(*kv), image_bytes.items()):
            out[i] = txt
    joined = "\n\n".join(out[i] for i in sorted(out))
    return IngestResult(text=joined, method="zip_jpegs", pages=len(out),
                        digital_chars=0, ocr_chars=len(joined.strip()))

def _ocr_pdf_pages_parallel(pdf_path: Path, page_indices: list[int], say: Callable[[str], None]) -> dict[int, str]:
    """Render specified pages at _OCR_DPI to bytes (sequential — pdfplumber is not thread-safe),
    then OCR them in parallel."""
    from PIL import Image
    rendered: dict[int, bytes] = {}
    with pdfplumber.open(pdf_path) as pdf:
        for idx in page_indices:
            page = pdf.pages[idx]
            pil_img = page.to_image(resolution=_OCR_DPI).original
            buf = io.BytesIO(); pil_img.save(buf, format="PNG"); buf.seek(0)
            rendered[idx] = buf.getvalue()

    completed = {"n": 0}
    total = len(rendered)
    def _ocr_one(kv):
        idx, data = kv
        pil = Image.open(io.BytesIO(data))
        text = pytesseract.image_to_string(pil, lang="eng")
        completed["n"] += 1
        say(f"OCR page {completed['n']}/{total}…")
        return idx, text

    out: dict[int, str] = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=_OCR_WORKERS) as pool:
        for idx, txt in pool.map(_ocr_one, rendered.items()):
            out[idx] = txt
    return out