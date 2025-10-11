"""Command-line interface for GED2GWB."""

import argparse
import sys
import logging
from pathlib import Path

from ..core.converter import Ged2GwbConverter
from ..utils.options import ConversionOptions
from ...db_pickle.database.base import PickleBase

class Ged2GwbCLI:
    """Command-line interface for GED2GWB."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def create_parser(self) -> argparse.ArgumentParser:
        parser = argparse.ArgumentParser(
            prog='ged2gwb',
            description='Convert GEDCOM 5.5.1 files to GeneWeb pickle databases',
            formatter_class=argparse.RawDescriptionHelpFormatter
        )

        parser.add_argument('gedcom_file', type=Path, nargs='?', help='Input GEDCOM file')
        parser.add_argument('-o', '--output', type=Path, default=Path('base.pkl'),
                           help='Output database file (default: base.pkl)')

        parser.add_argument('--charset', choices=['ANSEL', 'ASCII', 'MSDOS'],
                           help='Force charset decoding, overriding the possible setting in GEDCOM')
        parser.add_argument('-bd', '--base-dir', type=Path, metavar='DIR',
                           help='Directory where the "bases" directory with databases is installed (default: ".")')

        parser.add_argument('--dates-dm', action='store_true',
                           help='Interpret months-numbered dates as day/month/year')
        parser.add_argument('--dates-md', action='store_true',
                           help='Interpret months-numbered dates as month/day/year')
        parser.add_argument('--no-nd', action='store_true',
                           help="Don't interpret a year preceded by a minus sign as a negative year")

        parser.add_argument('--efn', action='store_true',
                           help='When creating a person, if the GEDCOM first name part holds several names, '
                                'the first of this names becomes the person "first name" and the complete '
                                'GEDCOM first name part a "first name alias"')
        parser.add_argument('--epn', action='store_true',
                           help='When creating a person, if the GEDCOM first name part looks like a public name')
        parser.add_argument('--no-efn', action='store_true',
                           help='Cancels the previous --efn option')
        parser.add_argument('--no-epn', action='store_true',
                           help='Cancels the previous --epn option')
        parser.add_argument('--fne', type=str, metavar='<be>',
                           help='When creating a person, if the GEDCOM first name part holds a part between '
                                '\'b\' (any character) and \'e\' (any character), it is considered to be '
                                'the usual first name: e.g. --fne \'""\' or --fne "()"')
        parser.add_argument('--lf', action='store_true',
                           help='Convert first names to lowercase letters, with initials in uppercase')
        parser.add_argument('--ls', action='store_true',
                           help='Convert surnames to lowercase letters, with initials in uppercase. '
                                'Try to keep lowercase particles')
        parser.add_argument('--us', action='store_true',
                           help='Convert surnames to uppercase letters')

        parser.add_argument('--particles', type=Path, metavar='<FILE>',
                           help='Use the given file as list of particles')

        parser.add_argument('--ds', type=str, metavar='<source>',
                           help='Set the source field for persons and families without source data')

        parser.add_argument('--rs-no-mention', action='store_true',
                           help='Force relation status to NoMention (default is Married)')

        parser.add_argument('--no-pit', action='store_true',
                           help='Do not consider persons having titles as public')

        parser.add_argument('--nopicture', action='store_true',
                           help="Don't extract individual picture")

        parser.add_argument('--udi', type=str, metavar='x-y',
                           help='Set the interval for persons whose death part is undefined: '
                                'if before x years, they are considered as alive; '
                                'if after y year, they are considered as death; '
                                'between x and y year, they are considered as "don\'t know". '
                                'Default x is 80 and y is 120')

        parser.add_argument('--uin', action='store_true',
                           help='Put untreated GEDCOM tags in notes')

        parser.add_argument('--tnd', action='store_true',
                           help='Set negative dates when inconsistency (e.g. birth after death)')

        parser.add_argument('--compress', action='store_true', default=False,
                           help='Compress output with gzip (default: False)')
        parser.add_argument('--no-compress', action='store_true',
                           help='Do not compress output')
        parser.add_argument('--force', '-f', action='store_true',
                           help='Remove database if already existing')
        parser.add_argument('--load', type=str, metavar='<file>',
                           help='Load existing pickle database and display information')

        parser.add_argument('--log', type=Path, metavar='<file>',
                           help='Redirect log trace to this file')
        parser.add_argument('--verbose', '-v', action='store_true',
                           help='Verbose output')
        parser.add_argument('--trackid', action='store_true',
                           help='Print gedcom id to gw id matches')

        parser.add_argument('--no-consistency-check', action='store_true',
                           help='No consistency check')
        parser.add_argument('-nc', '--nc', action='store_true',
                           help='No consistency check (alias)')
        parser.add_argument('--reorg', action='store_true',
                           help='Enable reorg mode')

        return parser

    def setup_logging(self, args) -> None:
        log_level = logging.INFO
        if args.verbose:
            log_level = logging.DEBUG

        handlers = [logging.StreamHandler()]
        if args.log:
            handlers.append(logging.FileHandler(args.log))

        logging.basicConfig(
            level=log_level,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=handlers
        )

    def parse_udi(self, udi_str: str) -> tuple:
        try:
            parts = udi_str.split('-')
            if len(parts) != 2:
                raise ValueError("UDI must be in format 'x-y'")
            return (int(parts[0]), int(parts[1]))
        except ValueError as e:
            raise argparse.ArgumentTypeError(f"Invalid UDI format: {e}")

    def load_database(self, filepath: str) -> PickleBase:
        """Load and display pickle database information using PickleReader."""
        from ...db_pickle.io.reader import PickleReader
        from ...db_pickle import PickleBase, create_pickle_base_func

        try:
            file_path = Path(filepath)
            if not file_path.exists():
                print(f"Error: File not found: {filepath}", file=sys.stderr)
                return None

            print(f"Loading pickle database: {filepath}")

            # Use PickleReader to load the database
            reader = PickleReader(verbose=True)
            data = reader.load_database(file_path)

            # Create base and functions
            pickle_base = PickleBase(data)
            func = create_pickle_base_func(data)

            # Display database statistics
            print(f"\n=== Database Statistics ===")
            print(f"Persons: {func.nb_of_persons()}")
            print(f"Families: {func.nb_of_families()}")
            print(f"Strings: {func.nb_of_strings()}")
            print(f"Real persons: {func.nb_of_real_persons()}")

            # Additional statistics
            print(f"Couples: {len(data.couples)}")
            print(f"Descendants: {len(data.descends)}")

            print(f"\nDatabase loaded successfully!")
            return pickle_base

        except Exception as e:
            print(f"Error loading database: {e}", file=sys.stderr)
            return None

    def run(self, args=None) -> int:
        """Run the CLI with given arguments."""
        parser = self.create_parser()
        args = parser.parse_args(args)

        self.setup_logging(args)

        if args.no_compress:
            args.compress = False

        if args.udi:
            args.udi = self.parse_udi(args.udi)

        try:
            if not args.load and not args.gedcom_file:
                print("Error: Either GEDCOM file or --load option is required", file=sys.stderr)
                return 1

            if args.load:
                return self.load_database(args.load)

            if args.nc:
                args.no_consistency_check = True
            options = ConversionOptions.from_args(args)
            converter = Ged2GwbConverter(options)
            converter.validate_input()
            if args.trackid:
                print("ID tracking not yet implemented")

            converter.convert()
            return 0

        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            if args.verbose:
                import traceback
                traceback.print_exc()
            return 1


def main():
    cli = Ged2GwbCLI()
    sys.exit(cli.run())

if __name__ == '__main__':
    main()
