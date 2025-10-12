# Russian Table OCR Processor

A Python script for processing images containing Russian text tables with overline notation support (0,1,2,3,4,5,6,7,8,9,1̅,2̅,3̅,4̅,5̅).

## Features

- **Russian Language Support**: Uses PaddleOCR with Cyrillic (Russian) language models
- **High Accuracy**: PaddleOCR is known for its excellent accuracy on table data
- **Image Preprocessing**: Automatic contrast enhancement, sharpening, and noise reduction
- **Batch Processing**: Process multiple files at once
- **Multiple Output Formats**: Save results as Markdown, JSON, or plain text
- **Detailed Logging**: Comprehensive logs for debugging and monitoring
- **Confidence Scoring**: Track OCR confidence for quality assessment
- **Summary Reports**: Automatic generation of processing summaries
- **Zero Setup**: Uses `uv` for automatic dependency management

## Prerequisites

Install `uv` (fast Python package installer):

```bash
# On macOS and Linux
curl -LsSf https://astral.sh/uv/install.sh | sh

# On Windows
powershell -c "irm https://astral.sh/uv/install.ps1 | iex"

# Or with pip
pip install uv
```

## Installation

No installation required! The script uses inline dependency metadata (PEP 723). Just run it with `uv`:

```bash
uv run ocr_processor.py
```

On first run, `uv` will automatically:
1. Create an isolated environment
2. Install all required dependencies (PaddlePaddle, PaddleOCR, etc.)
3. Download PaddleOCR Cyrillic model (~10MB)

This only happens once. Subsequent runs are instant.

## Usage

### Basic Usage

Process all .gif files in the current directory:

```bash
uv run ocr_processor.py
```

### Advanced Usage

```bash
# Specify input and output directories
uv run ocr_processor.py -i /path/to/images -o /path/to/output

# Process specific file types
uv run ocr_processor.py --pattern "*.png"
uv run ocr_processor.py --pattern "8_*.gif"

# Change output format
uv run ocr_processor.py --format json
uv run ocr_processor.py --format txt

# Use GPU acceleration (if available)
uv run ocr_processor.py --gpu

# Disable image preprocessing
uv run ocr_processor.py --no-preprocess

# Verbose logging
uv run ocr_processor.py -v

# Specify language (cyrillic for Russian)
uv run ocr_processor.py --languages cyrillic
uv run ocr_processor.py --languages en
```

### Full Command Options

```
usage: uv run ocr_processor.py [-h] [-i INPUT_DIR] [-o OUTPUT_DIR] [-p PATTERN]
                               [-f {markdown,json,txt}] [--no-preprocess] [--gpu]
                               [--languages LANGUAGES [LANGUAGES ...]] [-v]

Options:
  -i, --input-dir       Input directory (default: current directory)
  -o, --output-dir      Output directory (default: ./ocr_output)
  -p, --pattern         File pattern to match (default: *.gif)
  -f, --format          Output format: markdown, json, or txt
  --no-preprocess       Disable image preprocessing
  --gpu                 Use GPU acceleration
  --languages           Languages for OCR (default: cyrillic en)
                        Use "cyrillic" or "ru" for Russian
  -v, --verbose         Enable verbose logging
```

**Note**: PaddleOCR uses "cyrillic" for Russian text. The script automatically maps "ru" or "russian" to "cyrillic".

## Examples

### Example 1: Process Current Directory

```bash
uv run ocr_processor.py
```

This will:
- Process all .gif files in the current directory
- Save results to `./ocr_output/` as markdown files
- Create a processing summary
- Generate a log file `ocr_processing.log`

### Example 2: Custom Directories and Format

```bash
uv run ocr_processor.py \
  -i ~/Documents/scans \
  -o ~/Documents/ocr_results \
  --format json
```

### Example 3: Specific Files Only

```bash
uv run ocr_processor.py --pattern "8_40*.gif"
```

This processes only files matching the pattern (e.g., 8_406.gif, 8_407.gif, etc.)

## Output Structure

### Markdown Format (default)

Each processed file generates a `.md` file containing:
- Filename and timestamp
- Average OCR confidence score
- Extracted text
- Detailed results table with confidence scores

Example output: `8_407.md`

```markdown
# OCR Results: 8_407.gif

**Processed:** 2025-01-15T14:30:00

**Average Confidence:** 87.45%

## Extracted Text

```
[OCR extracted text here]
```

## Detailed Results

| Text | Confidence |
|------|------------|
| константы | 92.30% |
| для | 85.20% |
| ... | ... |
```

### JSON Format

```json
{
  "filename": "8_407.gif",
  "timestamp": "2025-01-15T14:30:00",
  "text": "extracted text...",
  "detailed_results": [
    {
      "text": "константы",
      "confidence": 0.923,
      "bbox": [[x1, y1], [x2, y2], [x3, y3], [x4, y4]]
    }
  ],
  "average_confidence": 0.8745
}
```

### Processing Summary

A `processing_summary.md` file is generated with statistics:
- Total files processed
- Success/failure counts
- Average confidence scores
- Individual file results

## Overline Notation

The script includes support for overline notation commonly used in mathematical and technical documents:

- Standard digits: 0,1,2,3,4,5,6,7,8,9
- Overline digits: 1̅,2̅,3̅,4̅,5̅,6̅,7̅,8̅,9̅,0̅

The `detect_overline_notation()` method can be customized based on how your specific documents represent overlined numbers in the source images.

## Troubleshooting

### Issue: uv not found

Make sure `uv` is installed:
```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Then restart your terminal or run:
```bash
source $HOME/.cargo/env
```

### Issue: Low OCR accuracy

1. Try enabling preprocessing (default):
   ```bash
   uv run ocr_processor.py
   ```

2. Check image quality - ensure images are:
   - At least 300 DPI
   - Not heavily compressed
   - Good contrast between text and background

3. Review the confidence scores in the output files

### Issue: Out of memory errors

1. Disable GPU if it's causing issues:
   ```bash
   uv run ocr_processor.py  # GPU disabled by default
   ```

2. Process files in smaller batches:
   ```bash
   uv run ocr_processor.py --pattern "8_40*.gif"
   uv run ocr_processor.py --pattern "8_41*.gif"
   ```

### Issue: Dependency conflicts

`uv` automatically manages dependencies in isolated environments, so conflicts are rare. If you encounter issues:

1. Clear the uv cache:
   ```bash
   uv cache clean
   ```

2. Run again:
   ```bash
   uv run ocr_processor.py
   ```

## Performance Tips

1. **GPU Acceleration**: If you have a CUDA-capable GPU, use `--gpu` flag
2. **Batch Processing**: Process multiple files at once rather than one at a time
3. **Image Quality**: Higher quality source images = better OCR results
4. **Preprocessing**: Usually helps, but can be disabled with `--no-preprocess` if needed

## Log Files

The script generates `ocr_processing.log` with detailed information:
- Processing progress
- Errors and warnings
- Confidence scores
- File-by-file results

Review this file if you encounter issues.

## Project Structure

```
source_materials/
├── ocr_processor.py          # Main script with inline dependencies
├── requirements.txt          # Optional: for pip/traditional installs
├── README_OCR.md            # This file
├── 8_407.gif                # Input images
├── 8_408.gif
├── ...
├── ocr_output/              # Generated output (created automatically)
│   ├── 8_407.md
│   ├── 8_408.md
│   ├── ...
│   └── processing_summary.md
└── ocr_processing.log       # Processing log
```

## Why PaddleOCR?

- **High Accuracy**: Excellent performance on Russian/Cyrillic text and tables
- **Fast**: Optimized for production use
- **Lightweight**: Smaller model downloads (~10MB vs ~100MB for EasyOCR)
- **Table-Optimized**: Better at detecting structured table layouts
- **Well-Maintained**: Active development and regular updates

## Why uv?

- **Fast**: 10-100x faster than pip
- **Reliable**: Deterministic dependency resolution
- **Simple**: No virtual environment management needed
- **Modern**: Uses PEP 723 inline script metadata
- **Isolated**: Each script runs in its own environment

## Traditional Installation (Optional)

If you prefer using pip and virtual environments:

```bash
# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Run script
python ocr_processor.py
```

However, using `uv run` is recommended for simpler dependency management.

## License

This script is provided as-is for processing Russian table OCR tasks.

## Support

For issues or questions:
1. Check the log file: `ocr_processing.log`
2. Run with verbose mode: `uv run ocr_processor.py -v`
3. Review the processing summary: `ocr_output/processing_summary.md`
