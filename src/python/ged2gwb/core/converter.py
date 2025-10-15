"""Main GED2GWB converter."""

import logging
import sys
from pathlib import Path
from typing import Any, Dict

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from gedcom.parser import create_parser

from ..converters.gedcom_to_geneweb import GedcomToGenewebConverter
from ..utils.options import ConversionOptions


class Ged2GwbConverter:
    """Main converter from GEDCOM to GeneWeb pickle format."""

    def __init__(self, options: ConversionOptions):
        self.options = options
        self.logger = logging.getLogger(__name__)

        if options.log_file:
            # Configure logging to file
            logging.basicConfig(
                level=logging.INFO,
                format="%(message)s",
                handlers=[
                    logging.FileHandler(options.log_file),
                    logging.StreamHandler(sys.stdout),
                ],
            )
        else:
            # No logging, only errors
            logging.getLogger().setLevel(logging.ERROR)

        self.parser = create_parser(**options.get_gedcom_parser_options())
        self.gedcom_converter = GedcomToGenewebConverter(options)

    def convert(self) -> Dict[str, Any]:
        try:
            # Handle charset if specified
            if self.options.charset:
                gedcom_database = self._parse_with_charset(
                    self.options.input_file, self.options.charset
                )
            else:
                gedcom_database = self.parser.parse_file(self.options.input_file)

            # Display mode like OCaml
            output_file = self.options.output_file
            print(f"Mode: classic, for base {output_file}")

            # Convert to GeneWeb format silently
            conversion_result = self.gedcom_converter.convert(gedcom_database)

            # Use the pickle database structure directly
            geneweb_data = conversion_result.get("geneweb_data")
            if geneweb_data is None:
                raise ValueError("No geneweb_data in conversion result")

            self.logger.info("Step 3: Saving to pickle format")
            output_file = self._get_output_path()
            stats = self._save_geneweb_data(geneweb_data, output_file)

            stats.update(
                {
                    "input_file": str(self.options.input_file),
                    "output_file": str(output_file),
                    "charset": self.options.charset,
                    "dates_dm": self.options.dates_dm,
                    "dates_md": self.options.dates_md,
                    "efn": self.options.efn,
                    "epn": self.options.epn,
                    "lf": self.options.lf,
                    "ls": self.options.ls,
                    "us": self.options.us,
                    "udi": self.options.udi,
                    "uin": self.options.uin,
                    "default_source": self.options.default_source,
                    "compressed": self.options.compress,
                    "force": self.options.force,
                    "verbose": self.options.verbose,
                    "conversion_successful": True,
                }
            )
            return stats

        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            raise

    def _parse_with_charset(self, file_path, charset):
        """Parse GEDCOM file with specific charset."""
        import codecs
        from gedcom.parser import GedcomParser
        from gedcom.tokenizer import GedcomTokenizer
        from gedcom.validators import GedcomValidator

        # Map GEDCOM charsets to Python encodings
        charset_map = {
            "ANSEL": "utf-8",  # ANSEL is not directly supported, use UTF-8
            "ASCII": "ascii",
            "MSDOS": "cp850",  # MS-DOS encoding
        }

        python_encoding = charset_map.get(charset, "utf-8")

        # Read file with specified charset
        with codecs.open(file_path, "r", encoding=python_encoding) as f:
            content = f.read()

        # Create parser components
        tokenizer = GedcomTokenizer()
        validator = GedcomValidator()
        parser = GedcomParser(tokenizer, validator)

        # Parse the content
        return parser.parse_content(content)

    def validate_input(self) -> bool:
        if not self.options.input_file.exists():
            raise FileNotFoundError(f"Input file not found: {self.options.input_file}")
        final_output_file = self._get_output_path()
        output_dir = final_output_file.parent

        if not output_dir.exists():
            print(f"Creating output directory: {output_dir}")
            output_dir.mkdir(parents=True, exist_ok=True)

        pkl_file = final_output_file.with_suffix(".pkl")
        pkl_gz_file = final_output_file.with_suffix(".pkl.gz")

        if (pkl_file.exists() or pkl_gz_file.exists()) and not self.options.force:
            existing_file = pkl_file if pkl_file.exists() else pkl_gz_file
            raise FileExistsError(
                f"Output file exists: {existing_file}. Use --force to overwrite."
            )

        self._validate_database_name(final_output_file)

        return True

    def _get_output_path(self) -> Path:
        """Get the correct output path considering base_dir option."""
        output_file = self.options.output_file
        if output_file.is_absolute():
            return output_file
        if self.options.base_dir:
            self.options.base_dir.mkdir(parents=True, exist_ok=True)
            return self.options.base_dir / output_file.name

        return output_file

    def _validate_database_name(self, output_file: Path) -> None:
        """Validate database name according to OCaml ged2gwb rules."""
        base_name = output_file.stem
        import re

        if not re.match(r"^[a-zA-Z0-9-]+$", base_name):
            raise ValueError(
                f'The database name "{base_name}" contains a forbidden character.\n'
                f"Allowed characters: a..z, A..Z, 0..9, -"
            )

    def _save_geneweb_data(self, geneweb_data, filepath: Path) -> Dict[str, Any]:
        """Save GeneWeb data to pickle format using PickleWriter."""
        from lib.db_pickle.io.writer import PickleWriter

        writer = PickleWriter(verbose=True)
        stats = writer.save_database(
            geneweb_data, filepath, compress=self.options.compress
        )

        return {
            "format": "pickle",
            "compressed": self.options.compress,
            "file_path": stats["file_path"],
            "file_size": stats["file_size"],
            "serialization_time": stats["save_time"],
            "individuals_count": stats["persons_count"],
            "families_count": stats["families_count"],
            "throughput_mb_s": (
                stats["file_size"] / stats["save_time"] / 1024 / 1024
                if stats["save_time"] > 0
                else 0
            ),
        }

    def get_conversion_info(self) -> Dict[str, Any]:
        return {
            "input_file": str(self.options.input_file),
            "output_file": str(self.options.output_file),
            "format": "pickle",
            "compression": self.options.compress,
            "charset": self.options.charset,
            "options": {
                "dates_dm": self.options.dates_dm,
                "dates_md": self.options.dates_md,
                "efn": self.options.efn,
                "epn": self.options.epn,
                "lf": self.options.lf,
                "ls": self.options.ls,
                "us": self.options.us,
            },
        }
