"""Command-line interface for consang matching OCaml implementation."""

import argparse
import sys
import os
from typing import List, Optional
from pathlib import Path

from .calculator import ConsanguinityCalculator
from .database import GenewebDatabase
from .exceptions import ConsangError


class ConsangCLI:
    """Command Line Interface for consang matching OCaml consang.ml."""

    def __init__(self):
        """Initialize CLI."""
        self.program_name = "consang"

    def _create_parser(self) -> argparse.ArgumentParser:
        """Create argument parser matching OCaml speclist."""
        parser = argparse.ArgumentParser(
            prog=self.program_name,
            description="Calculate consanguinity in genealogical databases",
            add_help=False,  # We handle help manually to match OCaml
            formatter_class=argparse.RawDescriptionHelpFormatter
        )

        # Options matching OCaml speclist
        parser.add_argument("-q", action="store_true", help="quiet mode")
        parser.add_argument("-qq", action="store_true", help="very quiet mode")
        parser.add_argument("-fast", action="store_true", help="faster, but use more memory")
        parser.add_argument("-scratch", action="store_true", help="from scratch")
        parser.add_argument("-mem", action="store_true", help="Save memory, but slower when rewritting database")
        parser.add_argument("-nolock", action="store_true", help="do not lock database.")
        parser.add_argument("-help", "--help", action="store_true", help="Display this list of options")

        # Anonymous argument (filename)
        parser.add_argument("database", nargs="?", help="database file name")

        return parser

    def _format_help_message(self) -> str:
        """Format help message matching OCaml errmsg."""
        return f"""usage: {self.program_name} [options] <file_name>
  -fast     faster, but use more memory
  -mem      Save memory, but slower when rewritting database
  -nolock   do not lock database.
  -q        quiet mode
  -qq       very quiet mode
  -scratch  from scratch
  -help     Display this list of options
  --help    Display this list of options"""

    def run(self, args: List[str] = None) -> int:
        """Run the CLI matching OCaml consang.ml behavior."""
        if args is None:
            args = sys.argv[1:]

        # Handle special cases before parsing
        if not args:
            print("Missing file name", file=sys.stderr)
            print("Use option -help for usage", file=sys.stderr)
            return 2

        # Handle unknown options manually (like OCaml)
        for arg in args:
            if arg.startswith("-") and arg not in ["-q", "-qq", "-fast", "-scratch", "-mem", "-nolock", "-help", "--help"]:
                print(f"{self.program_name}: unknown option '{arg}'.", file=sys.stderr)
                if arg == "-h":
                    print(self._format_help_message(), file=sys.stderr)
                return 2

        # Handle help manually
        if "-help" in args or "--help" in args:
            print(self._format_help_message())
            return 0

        parser = self._create_parser()

        try:
            parsed_args = parser.parse_args(args)
        except SystemExit:
            return 2

        if not parsed_args.database:
            print("Missing file name", file=sys.stderr)
            print("Use option -help for usage", file=sys.stderr)
            return 2

        if parsed_args.qq:
            verbosity = 0
        elif parsed_args.q:
            verbosity = 1
        else:
            verbosity = 2

        try:
            db_path = Path(parsed_args.database)
            if not db_path.exists():
                print(f"consang: {parsed_args.database}: No such file or directory", file=sys.stderr)
                return 2

            # TODO: Implement Secure.set_base_dir equivalent

            database = GenewebDatabase(parsed_args.database)

            calculator = ConsanguinityCalculator(
                verbosity=verbosity,
                fast_mode=parsed_args.fast,
                save_memory=parsed_args.mem,
                from_scratch=parsed_args.scratch,
                no_lock=parsed_args.nolock
            )

            if parsed_args.fast:
                database.load_fast_arrays()

            success = calculator.compute(database)

            if success:
                calculator.sync_database(database)

            return 0

        except KeyboardInterrupt:
            print("\nInterrupted", file=sys.stderr)
            return 2
        except Exception as e:
            if "loop in database" in str(e) or "topological" in str(e).lower():
                person_info = getattr(e, 'person_info', 'unknown person')
                print(f"\nError: loop in database, {person_info} is his/her own ancestor.", file=sys.stderr)
                return 2
            else:
                print(f"consang: {e}", file=sys.stderr)
                return 2

    def main(self) -> None:
        """Main entry point for CLI."""
        try:
            exit_code = self.run()
            sys.exit(exit_code)
        except SystemExit:
            raise
        except Exception as e:
            print(f"consang: {e}", file=sys.stderr)
            sys.exit(2)
