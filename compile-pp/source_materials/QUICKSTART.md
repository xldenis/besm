# Quick Start Guide

## Install uv

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Then restart your terminal or run:
```bash
source $HOME/.cargo/env
```

## Run OCR on All Files

```bash
cd /Users/xavier/Desktop/source_materials
uv run ocr_processor.py
```

That's it! The script will:
1. Automatically install PaddleOCR and all dependencies (first run only)
2. Download the Cyrillic (Russian) language model (~10MB)
3. Process all .gif files in the current directory
4. Save results to `ocr_output/` folder
5. Create a summary report

## Results

- **Individual files**: `ocr_output/8_407.md`, `ocr_output/8_408.md`, etc.
- **Summary report**: `ocr_output/processing_summary.md`
- **Log file**: `ocr_processing.log`

## Common Options

```bash
# Process specific files
uv run ocr_processor.py --pattern "8_40*.gif"

# Change output format to JSON
uv run ocr_processor.py --format json

# Specify custom directories
uv run ocr_processor.py -i /path/to/images -o /path/to/output

# Use GPU acceleration (if available)
uv run ocr_processor.py --gpu

# Verbose output
uv run ocr_processor.py -v
```

## Need Help?

See `README_OCR.md` for full documentation.
