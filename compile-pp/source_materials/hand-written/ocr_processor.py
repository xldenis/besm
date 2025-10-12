#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = [
#   "paddlepaddle>=2.6.0",
#   "paddleocr>=2.7.0",
#   "pillow>=10.0.0",
#   "numpy>=1.24.0",
#   "opencv-python-headless>=4.8.0",
# ]
# ///
"""
OCR Processor for Russian Tables with Overline Notation

This script processes image files containing tables in Russian with overline notation
(0,1,2,3,4,5,6,7,8,9,1̅,2̅,3̅,4̅,5̅) and extracts text using PaddleOCR.

Run with uv:
    uv run ocr_processor.py
    uv run ocr_processor.py -i /path/to/images -o /path/to/output
"""

import argparse
import logging
import sys
from pathlib import Path
from typing import List, Optional, Dict
import json
from datetime import datetime
import os

try:
    from paddleocr import PaddleOCR
except ImportError:
    PaddleOCR = None

try:
    from PIL import Image, ImageEnhance, ImageFilter
except ImportError:
    Image = None

import re


# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('ocr_processing.log'),
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__)


class OCRProcessor:
    """Process images with OCR for Russian text with overline notation."""

    # Mapping for overline notation Unicode combining character
    OVERLINE_CHAR = '\u0305'  # Combining overline
    OVERLINE_DIGITS = {
        '1': '1̅',
        '2': '2̅',
        '3': '3̅',
        '4': '4̅',
        '5': '5̅',
        '6': '6̅',
        '7': '7̅',
        '8': '8̅',
        '9': '9̅',
        '0': '0̅'
    }

    def __init__(self,
                 use_gpu: bool = False,
                 languages: List[str] = None,
                 preprocess: bool = True):
        """
        Initialize OCR processor.

        Args:
            use_gpu: Whether to use GPU acceleration
            languages: List of languages for OCR (default: ['ru'])
            preprocess: Whether to preprocess images before OCR
        """
        if PaddleOCR is None:
            raise ImportError(
                "paddleocr is not installed. Run with: uv run ocr_processor.py"
            )
        if Image is None:
            raise ImportError(
                "Pillow is not installed. Run with: uv run ocr_processor.py"
            )

        # PaddleOCR language codes
        # Common mappings for convenience
        lang_map = {
            'russian': 'ru',
            'cyrillic': 'ru',
            'ru': 'ru',
            'en': 'en',
            'english': 'en',
            'ch': 'ch',
            'chinese': 'ch'
        }

        self.languages = languages or ['ru', 'en']
        self.use_gpu = use_gpu
        self.preprocess = preprocess

        # Map to PaddleOCR language codes
        mapped_lang = self.languages[0].lower() if self.languages else 'ru'
        self.paddle_lang = lang_map.get(mapped_lang, mapped_lang)

        logger.info(f"Initializing PaddleOCR with language: {self.paddle_lang}")
        try:
            # Suppress PaddleOCR's verbose output
            os.environ.setdefault('FLAGS_FLAGS_logtostderr', '0')

            self.reader = PaddleOCR(
                use_textline_orientation=True,  # Enable text angle classification
                lang=self.paddle_lang,
                show_log=False
            )
            logger.info("PaddleOCR initialized successfully")
        except Exception as e:
            logger.error(f"Failed to initialize PaddleOCR: {e}")
            raise

    def preprocess_image(self, image_path: Path) -> Image.Image:
        """
        Preprocess image to improve OCR accuracy.

        Args:
            image_path: Path to the image file

        Returns:
            Preprocessed PIL Image
        """
        logger.debug(f"Preprocessing image: {image_path}")

        try:
            img = Image.open(image_path)

            # Convert to RGB if needed
            if img.mode != 'RGB':
                img = img.convert('RGB')

            # Enhance contrast
            enhancer = ImageEnhance.Contrast(img)
            img = enhancer.enhance(1.5)

            # Enhance sharpness
            enhancer = ImageEnhance.Sharpness(img)
            img = enhancer.enhance(1.3)

            # Reduce noise
            img = img.filter(ImageFilter.MedianFilter(size=3))

            return img

        except Exception as e:
            logger.error(f"Error preprocessing image {image_path}: {e}")
            raise

    def detect_overline_notation(self, text: str) -> str:
        """
        Post-process text to detect and mark potential overline notation.

        This is a heuristic approach - you may need to adjust based on actual patterns.

        Args:
            text: OCR extracted text

        Returns:
            Text with overline notation markers
        """
        # This is a placeholder - actual implementation depends on how
        # overline notation appears in your OCR results
        # You might need to manually review and create rules

        # Example: Look for patterns like "bar{1}" or similar OCR artifacts
        # that might represent overlined numbers

        return text

    def process_image(self, image_path: Path) -> Dict:
        """
        Process a single image file with OCR.

        Args:
            image_path: Path to the image file

        Returns:
            Dictionary containing OCR results and metadata
        """
        logger.info(f"Processing: {image_path.name}")

        try:
            # Preprocess if enabled
            if self.preprocess:
                img = self.preprocess_image(image_path)
                # Save to temporary location for OCR
                temp_path = image_path.parent / f"temp_{image_path.name}"
                img.save(temp_path)
                ocr_path = str(temp_path)
            else:
                ocr_path = str(image_path)

            # Perform OCR with PaddleOCR
            # PaddleOCR returns: [[bbox_points, (text, confidence)], ...]
            results = self.reader.ocr(ocr_path, cls=True)

            # Clean up temp file
            if self.preprocess and temp_path.exists():
                temp_path.unlink()

            # Extract text and confidence scores
            extracted_text = []

            # PaddleOCR returns results for each page (even for single images)
            if results and results[0]:
                for line in results[0]:
                    if line:
                        bbox_points = line[0]  # Bounding box coordinates
                        text_info = line[1]     # (text, confidence) tuple

                        text = text_info[0]
                        confidence = float(text_info[1])

                        extracted_text.append({
                            'text': text,
                            'confidence': confidence,
                            'bbox': bbox_points
                        })

            # Combine all text
            full_text = '\n'.join([item['text'] for item in extracted_text])

            # Post-process for overline notation
            processed_text = self.detect_overline_notation(full_text)

            result = {
                'filename': image_path.name,
                'timestamp': datetime.now().isoformat(),
                'text': processed_text,
                'detailed_results': extracted_text,
                'average_confidence': sum(item['confidence'] for item in extracted_text) / len(extracted_text) if extracted_text else 0
            }

            logger.info(f"Successfully processed {image_path.name} - Confidence: {result['average_confidence']:.2f}")
            return result

        except Exception as e:
            logger.error(f"Error processing {image_path}: {e}")
            return {
                'filename': image_path.name,
                'timestamp': datetime.now().isoformat(),
                'error': str(e),
                'text': '',
                'detailed_results': [],
                'average_confidence': 0
            }

    def save_results(self, result: Dict, output_dir: Path, format: str = 'markdown'):
        """
        Save OCR results to file.

        Args:
            result: OCR result dictionary
            output_dir: Output directory path
            format: Output format ('markdown', 'json', 'txt')
        """
        output_dir.mkdir(parents=True, exist_ok=True)

        stem = Path(result['filename']).stem

        if format == 'markdown':
            output_file = output_dir / f"{stem}.md"
            content = self._format_as_markdown(result)
        elif format == 'json':
            output_file = output_dir / f"{stem}.json"
            content = json.dumps(result, indent=2, ensure_ascii=False)
        else:  # txt
            output_file = output_dir / f"{stem}.txt"
            content = result['text']

        try:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(content)
            logger.info(f"Saved results to: {output_file}")
        except Exception as e:
            logger.error(f"Error saving results to {output_file}: {e}")

    def _format_as_markdown(self, result: Dict) -> str:
        """Format OCR results as markdown."""
        md = f"# OCR Results: {result['filename']}\n\n"
        md += f"**Processed:** {result['timestamp']}\n\n"

        if 'error' in result:
            md += f"**Error:** {result['error']}\n\n"
        else:
            md += f"**Average Confidence:** {result['average_confidence']:.2%}\n\n"
            md += "## Extracted Text\n\n"
            md += "```\n"
            md += result['text']
            md += "\n```\n\n"

            if result.get('detailed_results'):
                md += "## Detailed Results\n\n"
                md += "| Text | Confidence |\n"
                md += "|------|------------|\n"
                for item in result['detailed_results']:
                    md += f"| {item['text']} | {item['confidence']:.2%} |\n"

        return md

    def process_directory(self,
                         input_dir: Path,
                         output_dir: Path,
                         pattern: str = "*.gif",
                         output_format: str = 'markdown') -> List[Dict]:
        """
        Process all images in a directory.

        Args:
            input_dir: Input directory path
            output_dir: Output directory path
            pattern: File pattern to match (e.g., '*.gif', '*.png')
            output_format: Output format ('markdown', 'json', 'txt')

        Returns:
            List of result dictionaries
        """
        logger.info(f"Processing directory: {input_dir}")
        logger.info(f"Looking for files matching: {pattern}")

        image_files = sorted(input_dir.glob(pattern))

        if not image_files:
            logger.warning(f"No files found matching pattern '{pattern}' in {input_dir}")
            return []

        logger.info(f"Found {len(image_files)} files to process")

        results = []
        for i, image_path in enumerate(image_files, 1):
            logger.info(f"Progress: {i}/{len(image_files)}")
            result = self.process_image(image_path)
            results.append(result)
            self.save_results(result, output_dir, output_format)

        # Save summary
        self._save_summary(results, output_dir)

        logger.info(f"Completed processing {len(results)} files")
        return results

    def _save_summary(self, results: List[Dict], output_dir: Path):
        """Save a summary of all processed files."""
        summary_file = output_dir / "processing_summary.md"

        total = len(results)
        successful = sum(1 for r in results if 'error' not in r)
        failed = total - successful
        avg_confidence = sum(r['average_confidence'] for r in results if 'error' not in r) / successful if successful > 0 else 0

        summary = f"# OCR Processing Summary\n\n"
        summary += f"**Generated:** {datetime.now().isoformat()}\n\n"
        summary += f"## Statistics\n\n"
        summary += f"- Total files: {total}\n"
        summary += f"- Successfully processed: {successful}\n"
        summary += f"- Failed: {failed}\n"
        summary += f"- Average confidence: {avg_confidence:.2%}\n\n"
        summary += f"## Files Processed\n\n"
        summary += "| Filename | Status | Confidence |\n"
        summary += "|----------|--------|------------|\n"

        for result in results:
            status = "Error" if 'error' in result else "Success"
            conf = f"{result['average_confidence']:.2%}" if 'error' not in result else "N/A"
            summary += f"| {result['filename']} | {status} | {conf} |\n"

        try:
            with open(summary_file, 'w', encoding='utf-8') as f:
                f.write(summary)
            logger.info(f"Summary saved to: {summary_file}")
        except Exception as e:
            logger.error(f"Error saving summary: {e}")


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(
        description="OCR processor for Russian tables with overline notation",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument(
        '-i', '--input-dir',
        type=Path,
        default=Path.cwd(),
        help='Input directory containing images (default: current directory)'
    )

    parser.add_argument(
        '-o', '--output-dir',
        type=Path,
        default=Path.cwd() / 'ocr_output',
        help='Output directory for results (default: ./ocr_output)'
    )

    parser.add_argument(
        '-p', '--pattern',
        type=str,
        default='*.gif',
        help='File pattern to match (default: *.gif)'
    )

    parser.add_argument(
        '-f', '--format',
        choices=['markdown', 'json', 'txt'],
        default='markdown',
        help='Output format (default: markdown)'
    )

    parser.add_argument(
        '--no-preprocess',
        action='store_true',
        help='Disable image preprocessing'
    )

    parser.add_argument(
        '--gpu',
        action='store_true',
        help='Use GPU acceleration if available'
    )

    parser.add_argument(
        '--languages',
        type=str,
        nargs='+',
        default=['ru'],
        help='Languages for OCR (default: ru). Use "ru" or "russian" for Russian'
    )

    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose logging'
    )

    args = parser.parse_args()

    if args.verbose:
        logger.setLevel(logging.DEBUG)

    # Validate input directory
    if not args.input_dir.exists():
        logger.error(f"Input directory does not exist: {args.input_dir}")
        sys.exit(1)

    if not args.input_dir.is_dir():
        logger.error(f"Input path is not a directory: {args.input_dir}")
        sys.exit(1)

    try:
        # Initialize processor
        processor = OCRProcessor(
            use_gpu=args.gpu,
            languages=args.languages,
            preprocess=not args.no_preprocess
        )

        # Process directory
        results = processor.process_directory(
            input_dir=args.input_dir,
            output_dir=args.output_dir,
            pattern=args.pattern,
            output_format=args.format
        )

        # Print summary
        successful = sum(1 for r in results if 'error' not in r)
        print(f"\n{'='*60}")
        print(f"Processing complete!")
        print(f"Total files: {len(results)}")
        print(f"Successful: {successful}")
        print(f"Failed: {len(results) - successful}")
        print(f"Results saved to: {args.output_dir}")
        print(f"{'='*60}\n")

    except Exception as e:
        logger.error(f"Fatal error: {e}", exc_info=True)
        sys.exit(1)


if __name__ == "__main__":
    main()
