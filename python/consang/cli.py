"""Command-line interface for consang using shared utilities."""

import argparse
import sys
import os
from typing import Optional, List
from pathlib import Path

from common.exceptions import DatabaseError, ArgumentError
from .calculator import ConsanguinityCalculator
from .database import GenewebDatabase
from .exceptions import ConsangError


class ConsangCLI:
    """Command Line Interface for consang."""

    def __init__(self):
        """Initialize CLI."""
        self.program_name = "consang"
        self.parser = self._setup_parser()

    def _ocaml_prog_for_usage(self) -> str:
        """Return OCaml binary path for usage line if available."""
        possible_paths = [
            Path("../distribution/gw/consang"),  # From python/ directory
            Path("./distribution/gw/consang"),  # From project root
            Path("distribution/gw/consang"),  # Relative path
        ]
        if os.getenv("CONSANG_OCAML_HELP") == "1":
            for path in possible_paths:
                if path.exists():
                    return str(path)
        return self.program_name

    def _setup_parser(self) -> argparse.ArgumentParser:
        """Setup argument parser with OCaml-compatible format."""
        parser = argparse.ArgumentParser(
            prog=self.program_name,
            description="Calculate consanguinity in genealogical databases",
            formatter_class=argparse.RawDescriptionHelpFormatter,
            add_help=False,
        )
        parser.add_argument("-fast", action="store_true", help="faster, but use more memory")
        parser.add_argument(
            "-mem", action="store_true", help="Save memory, but slower when rewritting database"
        )
        parser.add_argument("-nolock", action="store_true", help="do not lock database.")
        parser.add_argument("-q", action="count", default=0, help="quiet mode")
        parser.add_argument("-scratch", action="store_true", help="from scratch")
        parser.add_argument("--help", action="store_true", help="show this help message and exit")

        parser.add_argument("database", nargs="?", help="database file name")

        return parser

    def _format_help_message(self) -> str:
        """Format help message to match OCaml version exactly."""
        usage_prog = self._ocaml_prog_for_usage()
        return f"""usage: {usage_prog} [options] <file_name>
  -fast     faster, but use more memory
  -mem      Save memory, but slower when rewritting database
  -nolock   do not lock database.
  -q        quiet mode
  -qq       very quiet mode
  -scratch  from scratch
  -help     Display this list of options
  --help    Display this list of options"""

    def _handle_invalid_option(self, option: str) -> int:
        """Handle invalid option with OCaml-compatible behavior."""
        program_label = self._ocaml_prog_for_usage()
        print(f"{program_label}: unknown option '{option}'.", file=sys.stderr)

        # For -h specifically, show help after error (OCaml behavior)
        if option == "-h":
            print(self._format_help_message(), file=sys.stderr)
        return 2

    def _show_help(self) -> None:
        """Show help message and exit like argparse."""
        print(self._format_help_message())
        raise SystemExit(0)

    def _show_missing_filename_error(self) -> int:
        """Show missing filename error with OCaml-compatible message."""
        print("Missing file name", file=sys.stderr)
        print("Use option -help for usage@.", file=sys.stderr)
        return 2

    def run(self, args: List[str] = None) -> int:
        """Run the CLI with OCaml-compatible behavior."""
        if args is None:
            args = sys.argv[1:]

        if not args:
            return self._show_missing_filename_error()

        if "-h" in args:
            return self._handle_invalid_option("-h")
        if "-help" in args or "--help" in args:
            return self._show_help()

        try:
            parsed_args = self.parser.parse_args(args)
        except SystemExit as e:
            return 2 if e.code != 0 else 0
        except Exception:
            return 2

        # Validate required arguments
        if not parsed_args.database:
            return self._show_missing_filename_error()

        # Convert parsed args to calculator config
        try:
            db_path = Path(parsed_args.database)
            if not db_path.exists():
                raise FileNotFoundError(parsed_args.database)

            calculator = ConsanguinityCalculator(
                quiet_level=parsed_args.q,
                fast_mode=parsed_args.fast,
                from_scratch=parsed_args.scratch,
                save_memory=parsed_args.mem,
                no_lock=parsed_args.nolock,
            )

            database = GenewebDatabase(parsed_args.database)
            result = calculator.calculate(database)

            if parsed_args.q == 0:  # Not quiet
                print(
                    f"Processed {result.persons_processed} persons in {result.calculation_time:.2f}s"
                )

            try:
                if result.persons_processed == 0 and database.total_persons == 0:
                    return 2
            except Exception:
                pass

            return 0

        except FileNotFoundError:
            print(f"consang: {parsed_args.database}: No such file or directory", file=sys.stderr)
            return 2
        except Exception as e:
            print(f"consang: {e}", file=sys.stderr)
            return 2

    def main(self) -> None:
        """Main entry point for CLI."""
        exit_code = self.run()
        sys.exit(exit_code)
