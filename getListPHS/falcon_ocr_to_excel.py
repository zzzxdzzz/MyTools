#!/usr/bin/env python3
"""
Falcon Directory '25 — OCR to Excel
Usage:
  python falcon_ocr_to_excel.py path/to/IMG_9777.PNG output.xlsx

Requires:
  pip install pillow pytesseract pandas xlsxwriter
  And Tesseract OCR installed on your system (tesseract binary in PATH).
"""
import sys, os
from PIL import Image, ImageOps, ImageFilter
import pytesseract
import pandas as pd

def ocr_to_excel(image_path, out_xlsx, cols=8, rows=3, timeout=12):
    img = Image.open(image_path).convert("RGB")
    proc = ImageOps.autocontrast(img.filter(ImageFilter.SHARPEN).convert("L"))
    W, H = proc.size

    overlap_x, overlap_y = 20, 20
    tile_w = W // cols
    tile_h = H // rows

    records = []
    cfg = r"--oem 3 --psm 6"

    tile_index = 0
    for r in range(rows):
        for c in range(cols):
            left = max(0, c * tile_w - (overlap_x if c>0 else 0))
            right = min(W, (c+1) * tile_w + (overlap_x if c<cols-1 else 0))
            top = max(0, r * tile_h - (overlap_y if r>0 else 0))
            bottom = min(H, (r+1) * tile_h + (overlap_y if r<rows-1 else 0))
            tile = proc.crop((left, top, right, bottom))
            tile_index += 1
            try:
                data = pytesseract.image_to_string(tile, lang="eng", config=cfg, timeout=timeout)
            except pytesseract.pytesseract.TesseractError as e:
                data = e.truncated_result or ""
            for line_no, line in enumerate(data.splitlines(), start=1):
                line = line.strip()
                if line:
                    records.append({
                        "tile_index": tile_index,
                        "row": r+1,
                        "col": c+1,
                        "left": left,
                        "top": top,
                        "right": right,
                        "bottom": bottom,
                        "line_number_in_tile": line_no,
                        "text": line
                    })
    df = pd.DataFrame(records).sort_values(["row","col","line_number_in_tile"]).reset_index(drop=True)
    with pd.ExcelWriter(out_xlsx, engine="xlsxwriter") as writer:
        df.to_excel(writer, sheet_name="Lines", index=False)
    print(f"Wrote {len(df)} lines to {out_xlsx}")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python falcon_ocr_to_excel.py <input_image> <output.xlsx>")
        sys.exit(1)
    ocr_to_excel(sys.argv[1], sys.argv[2])
