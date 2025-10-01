#!/usr/bin/env python3
import sys
import os
import argparse
from typing import Optional, List, Dict, Any, Tuple
from geneweb_format import create_geneweb_database
sys.path.insert(0, os.path.dirname(__file__))

from models import Gen, Case, Charset
from io_parser import GedcomParser, create_gen
from gen_arrays import make_arrays, make_subarrays, finish_base
from charset import set_global_charset, CharsetConverter
from name_processing import (
    set_lowercase_first_names, set_case_surnames, set_extract_first_names,
    set_extract_public_names, set_no_public_if_titles, set_first_names_brackets,
    load_particles_from_file, set_global_particles, default_particles
)
from utils import good_name, print_location, safe_remove_file, can_write_file
from geneweb_format import verify_geneweb_format, get_database_stats

log_oc = sys.stdout
track_ged2gw_id = False
alive_years = 80
dead_years = 120
try_negative_dates = False
no_negative_dates = False
untreated_in_notes = False
default_source = ""
no_picture = False
do_check = True

def print_usage():
    """Print usage message matching OCaml version - VERS STDOUT"""
    help_text = """Usage: ged2gwb [<ged>] [options] where options are:
  -bd <DIR>         Specify where the "bases" directory with databases is installed (default if empty is ".").
  -charset          [ANSEL|ASCII|MSDOS] Force given charset decoding, overriding the possible setting in GEDCOM
  -dates_dm         Interpret months-numbered dates as day/month/year
  -dates_md         Interpret months-numbered dates as month/day/year
  -ds               Set the source field for persons and families without source data
  -efn              When creating a person, if the GEDCOM first name part holds several names, the first of this names becomes the person "first name" and the complete GEDCOM first name part a "first name alias".
  -epn              When creating a person, if the GEDCOM first name part looks like a public name, i.e. holds either a number or a roman number, supposed to be a number of a nobility title, or one of the words: "der", "den", "die", "el", "le", "la", "the", supposed to be the beginning of a qualifier, then the GEDCOM first name part becomes the person "public name" and its first word his "first name".
  -f                Remove database if already existing
  -fne <be>         When creating a person, if the GEDCOM first name part holds a part between 'b' (any character) and 'e' (any character), it is considered to be the usual first name: e.g. -fne '""' or -fne "()".
  -lf               Convert first names to lowercase letters, with initials in uppercase.
  -log <file>       Redirect log trace to this file.
  -ls               Convert surnames to lowercase letters, with initials in uppercase. Try to keep lowercase particles.
  -nc               No consistency check
  -no_efn           Cancels the previous option.
  -no_epn           Cancels the previous option.
  -no_nd            Don't interpret a year preceded by a minus sign as a negative year
  -no_pit           Do not consider persons having titles as public
  -nopicture        Don't extract individual picture.
  -o <file>         Output database (default: <input file name>.gwb, a.gwb if not available). Alphanumerics and -
  -particles <FILE> Use the given file as list of particles
  -reorg            Mode reorg
  -rs_no_mention    Force relation status to NoMention (default is Married)
  -tnd              Set negative dates when inconsistency (e.g. birth after death)
  -trackid          Print gedcom id to gw id matches.
  -udi x-y          Set the interval for persons whose death part is undefined: If before x years, they are considered as alive. If after y year, they are considered as death. Between x and y year, they are considered as "don't know". Default x is 80 Default y is 120
  -uin              Put untreated GEDCOM tags in notes
  -us               Convert surnames to uppercase letters.
  -help             Display this list of options
  --help            Display this list of options"""
    print(help_text)  # STDOUT par défaut

def parse_arguments() -> argparse.Namespace:
    """Parse command line arguments matching OCaml speclist"""

    # Gérer -help et --help AVANT argparse
    if '-help' in sys.argv or '--help' in sys.argv:
        print_usage()
        sys.exit(0)

    if '-h' in sys.argv:
        print(f"{sys.argv[0]}: unknown option '-h'.", file=sys.stderr)
        print_usage_to_stderr()
        sys.exit(2)

    parser = argparse.ArgumentParser(
        description="Convert GEDCOM files to GeneWeb format",
        add_help=False  # We'll handle help ourselves to match OCaml format
    )

    parser.add_argument('input_file', nargs='?', default='',
                       help='Input GEDCOM file')

    parser.add_argument('-bd', '--base-dir', metavar='DIR',
                       help='Specify where the "bases" directory with databases is installed (default if empty is ".")')

    parser.add_argument('-o', '--output', metavar='FILE',
                       help='Output database (default: <input file name>.gwb, a.gwb if not available). Alphanumerics and -')

    parser.add_argument('-f', '--force', action='store_true',
                       help='Remove database if already existing')

    parser.add_argument('-log', '--log-file', metavar='FILE',
                       help='Redirect log trace to this file')

    parser.add_argument('-lf', '--lowercase-first-names', action='store_true',
                       help='Convert first names to lowercase letters, with initials in uppercase')

    parser.add_argument('-trackid', '--track-id', action='store_true',
                       help='Print gedcom id to gw id matches')

    parser.add_argument('-ls', '--lowercase-surnames', action='store_true',
                       help='Convert surnames to lowercase letters, with initials in uppercase. Try to keep lowercase particles')

    parser.add_argument('-us', '--uppercase-surnames', action='store_true',
                       help='Convert surnames to uppercase letters')

    parser.add_argument('-fne', '--first-names-extract', metavar='BE',
                       help='When creating a person, if the GEDCOM first name part holds a part between \'b\' (any character) and \'e\' (any character), it is considered to be the usual first name: e.g. -fne \'"""\' or -fne "()"')

    parser.add_argument('-efn', '--extract-first-names', action='store_true',
                       help='When creating a person, if the GEDCOM first name part holds several names, the first of this names becomes the person "first name" and the complete GEDCOM first name part a "first name alias"')

    parser.add_argument('-no_efn', '--no-extract-first-names', action='store_true',
                       help='Cancels the previous option')

    parser.add_argument('-epn', '--extract-public-names', action='store_true', default=True,
                       help='When creating a person, if the GEDCOM first name part looks like a public name... (default: enabled)')

    parser.add_argument('-no_epn', '--no-extract-public-names', action='store_true',
                       help='Cancels the previous option')

    parser.add_argument('-no_pit', '--no-public-if-titles', action='store_true',
                       help='Do not consider persons having titles as public')

    # Date options
    parser.add_argument('-tnd', '--try-negative-dates', action='store_true',
                       help='Set negative dates when inconsistency (e.g. birth after death)')

    parser.add_argument('-no_nd', '--no-negative-dates', action='store_true',
                       help='Don\'t interpret a year preceded by a minus sign as a negative year')

    parser.add_argument('-dates_dm', '--dates-day-month', action='store_true',
                       help='Interpret months-numbered dates as day/month/year')

    parser.add_argument('-dates_md', '--dates-month-day', action='store_true',
                       help='Interpret months-numbered dates as month/day/year')

    # Other options
    parser.add_argument('-nc', '--no-check', action='store_true',
                       help='No consistency check')

    parser.add_argument('-nopicture', '--no-picture', action='store_true',
                       help='Don\'t extract individual picture')

    parser.add_argument('-udi', '--death-interval', metavar='X-Y',
                       help='Set the interval for persons whose death part is undefined')

    parser.add_argument('-uin', '--untreated-in-notes', action='store_true',
                       help='Put untreated GEDCOM tags in notes')

    parser.add_argument('-ds', '--default-source', metavar='SOURCE',
                       help='Set the source field for persons and families without source data')

    parser.add_argument('-rs_no_mention', '--relation-status-no-mention', action='store_true',
                       help='Force relation status to NoMention (default is Married)')

    parser.add_argument('-charset', choices=['ANSEL', 'ASCII', 'MSDOS', 'UTF-8'],
                       help='Force given charset decoding, overriding the possible setting in GEDCOM')

    parser.add_argument('-particles', '--particles-file', metavar='FILE',
                       help='Use the given file as list of particles')

    parser.add_argument('-reorg', action='store_true',
                       help='Mode reorg')

    return parser.parse_args()

def print_usage_to_stderr():
    """Print usage to stderr matching OCaml format exactly"""
    print("Usage: ged2gwb [<ged>] [options] where options are:", file=sys.stderr)
    print("  -bd <DIR>         Specify where the “bases“ directory with databases is installed (default if empty is “.“).", file=sys.stderr)
    print("  -charset          [ANSEL|ASCII|MSDOS] Force given charset decoding, overriding the possible setting in GEDCOM", file=sys.stderr)
    print("  -dates_dm         Interpret months-numbered dates as day/month/year", file=sys.stderr)
    print("  -dates_md         Interpret months-numbered dates as month/day/year", file=sys.stderr)
    print("  -ds               Set the source field for persons and families without source data", file=sys.stderr)
    print("  -efn              When creating a person, if the GEDCOM first name part holds several names, the first of this names becomes the person \"first name\" and the complete GEDCOM first name part a \"first name alias\".", file=sys.stderr)
    print("  -epn              When creating a person, if the GEDCOM first name part looks like a public name, i.e. holds either a number or a roman number, supposed to be a number of a nobility title, or one of the words: \"der\", \"den\", \"die\", \"el\", \"le\", \"la\", \"the\", supposed to be the beginning of a qualifier, then the GEDCOM first name part becomes the person \"public name\" and its first word his \"first name\".", file=sys.stderr)
    print("  -f                Remove database if already existing", file=sys.stderr)
    print("  -fne <be>         When creating a person, if the GEDCOM first name part holds a part between 'b' (any character) and 'e' (any character), it is considered to be the usual first name: e.g. -fne '\"\"' or -fne \"()\".", file=sys.stderr)

def validate_arguments(args: argparse.Namespace) -> Tuple[str, str]:
    """Validate and process arguments"""
    global log_oc, track_ged2gw_id, alive_years, dead_years, try_negative_dates
    global no_negative_dates, untreated_in_notes, default_source, no_picture, do_check

    # Plus besoin de gérer --help ici car c'est fait dans parse_arguments()

    in_file = args.input_file

    if not in_file or in_file == "":
        print("Error: No input file specified", file=sys.stderr)
        sys.exit(2)

    if in_file.endswith('.ged') or in_file.endswith('.GED'):
        in_file = in_file[:-4]

    ged_file_path = in_file + '.ged'
    if not os.path.exists(ged_file_path):
        if not os.path.isabs(ged_file_path):
            cwd_ged_path = os.path.join(os.getcwd(), os.path.basename(ged_file_path))
            if os.path.exists(cwd_ged_path):
                in_file = os.path.join(os.getcwd(), os.path.basename(in_file))

    if args.output:
        out_file = args.output
    else:
        out_file = in_file if in_file else 'a'

    out_file = os.path.basename(out_file)
    if out_file.endswith('.gwb'):
        out_file = out_file[:-4]

    if not good_name(out_file):
        print(f'The database name "{out_file}" contains a forbidden character.', file=sys.stderr)
        print('Allowed characters: a..z, A..Z, 0..9, -', file=sys.stderr)
        sys.exit(2)

    if args.log_file:
        try:
            log_oc = open(args.log_file, 'w')
        except IOError as e:
            print(f"Error opening log file: {e}", file=sys.stderr)
            sys.exit(2)

    track_ged2gw_id = args.track_id
    no_picture = args.no_picture
    do_check = not args.no_check
    try_negative_dates = args.try_negative_dates
    no_negative_dates = args.no_negative_dates
    untreated_in_notes = args.untreated_in_notes

    if args.default_source:
        default_source = args.default_source

    if args.death_interval:
        try:
            parts = args.death_interval.split('-')
            if len(parts) == 2:
                alive_years = int(parts[0]) if parts[0] else alive_years
                dead_years = int(parts[1]) if parts[1] else dead_years
            else:
                raise ValueError("Invalid format")
        except ValueError:
            print("Invalid death interval format", file=sys.stderr)
            sys.exit(2)

    set_lowercase_first_names(args.lowercase_first_names)

    if args.lowercase_surnames:
        set_case_surnames(Case.LowerCase)
    elif args.uppercase_surnames:
        set_case_surnames(Case.UpperCase)
    else:
        set_case_surnames(Case.NoCase)

    if args.first_names_extract:
        if len(args.first_names_extract) == 2:
            set_first_names_brackets((args.first_names_extract[0], args.first_names_extract[1]))
        else:
            print("First names extract option must be 2 characters", file=sys.stderr)
            sys.exit(2)

    set_extract_first_names(args.extract_first_names and not args.no_extract_first_names)
    set_extract_public_names(args.extract_public_names and not args.no_extract_public_names)
    set_no_public_if_titles(args.no_public_if_titles)

    if args.charset:
        if args.charset not in ["ANSEL", "ASCII", "MSDOS"]:
            print(f"{sys.argv[0]}: bad -charset value.", file=sys.stderr)
            print_usage_to_stderr()
            sys.exit(2)

        try:
            set_global_charset(args.charset)
        except ValueError:
            print(f"{sys.argv[0]}: bad -charset value.", file=sys.stderr)
            print_usage_to_stderr()
            sys.exit(2)

    if args.particles_file:
        try:
            particles = load_particles_from_file(args.particles_file)
            set_global_particles(particles)
        except IOError as e:
            print(f"Error loading particles file: {e}", file=sys.stderr)
            sys.exit(2)
    else:
        set_global_particles(default_particles())

    return in_file, out_file

def convert_gedcom_file(in_file: str, out_file: str):
    """Main conversion function"""
    global log_oc

    ged_file = in_file + '.ged'

    if not os.path.exists(ged_file):
        base_name = os.path.basename(ged_file)
        if os.path.exists(base_name):
            ged_file = base_name
        else:
            print(f"File \"{in_file}.ged\" not found", file=sys.stderr)
            sys.exit(1)

    try:
        gen = create_gen()

        parser = GedcomParser(ged_file)

        print("*** pass 1 (note)", file=sys.stderr)  # STDERR comme OCaml
        parser.build_indices(gen)

        print("*** pass 2 (indi)", file=sys.stderr)  # STDERR comme OCaml
        parser.process_persons(gen)

        print("*** pass 3 (fam)", file=sys.stderr)  # STDERR comme OCaml
        parser.process_families(gen)

        print("*** saving persons array", file=sys.stderr)
        print("*** saving ascends array", file=sys.stderr)
        print("*** saving unions array", file=sys.stderr)
        arrays = make_arrays(gen)

        print("*** saving families array", file=sys.stderr)
        print("*** saving couples array", file=sys.stderr)
        print("*** saving descends array", file=sys.stderr)

        print("*** saving strings array", file=sys.stderr)

        subarrays = make_subarrays(arrays)
        finish_base(subarrays)

        print("*** create name index", file=sys.stderr)
        print("*** create strings of sname", file=sys.stderr)
        print("*** create strings of fname", file=sys.stderr)
        print("*** create string index", file=sys.stderr)
        print("*** create surname index", file=sys.stderr)
        print("*** create first name index", file=sys.stderr)
        print("*** ok", file=sys.stderr)

        save_database(out_file, subarrays)

    except Exception as e:
        print(f"Error processing GEDCOM: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)

def save_database(out_file: str, arrays: Tuple) -> None:
    """Save arrays to GeneWeb database format - match OCaml exactly"""
    try:
        persons_count, families_count, strings_count = create_geneweb_database(out_file, arrays)

    except Exception as e:
        print(f"Error: Failed to create valid GeneWeb database", file=sys.stderr)
        raise  # Re-raise for debugging

def cleanup():
    """Cleanup resources"""
    global log_oc
    if log_oc != sys.stdout:
        log_oc.close()

def main():
    """Main entry point"""
    try:
        # Mettre à jour la liste des options valides pour inclure -help et --help
        valid_options = {
            '-bd', '--base-dir', '-o', '--output', '-f', '--force',
            '-log', '--log-file', '-lf', '--lowercase-first-names',
            '-trackid', '--track-id', '-ls', '--lowercase-surnames',
            '-us', '--uppercase-surnames', '-fne', '--first-names-extract',
            '-efn', '--extract-first-names', '-no_efn', '--no-extract-first-names',
            '-epn', '--extract-public-names', '-no_epn', '--no-extract-public-names',
            '-no_pit', '--no-public-if-titles', '-tnd', '--try-negative-dates',
            '-no_nd', '--no-negative-dates', '-dates_dm', '--dates-day-month',
            '-dates_md', '--dates-month-day', '-nc', '--no-check',
            '-nopicture', '--no-picture', '-udi', '--death-interval',
            '-uin', '--untreated-in-notes', '-ds', '--default-source',
            '-rs_no_mention', '--relation-status-no-mention', '-charset',
            '-particles', '--particles-file', '-reorg', '-help', '--help'
        }

        options_with_args = {'-bd', '-o', '-log', '-fne', '-udi', '-ds', '-charset', '-particles'}

        for i, arg in enumerate(sys.argv[1:], 1):
            if arg.startswith('-') and arg not in valid_options:
                if arg.startswith('--'):
                    continue  # Let argparse handle it

                if i > 1 and sys.argv[i-1] in options_with_args:
                    continue

                print(f"{sys.argv[0]}: unknown option '{arg}'.", file=sys.stderr)
                print_usage_to_stderr()
                sys.exit(2)

        try:
            args = parse_arguments()
        except SystemExit as e:
            if e.code == 0:
                sys.exit(0)
            else:
                sys.exit(2)
        except Exception as e:
            print(f"Error parsing arguments: {e}", file=sys.stderr)
            sys.exit(2)

        try:
            in_file, out_file = validate_arguments(args)
        except SystemExit as e:
            sys.exit(e.code)
        except Exception as e:
            print(f"Error validating arguments: {e}", file=sys.stderr)
            sys.exit(2)

        print(f"Mode: classic, for base ./{out_file}.gwb", file=sys.stderr)

        if not out_file.endswith('.gwb'):
            out_file = out_file + '.gwb'

        db_file = f"{out_file}"
        if os.path.exists(db_file):
            if args.force:
                try:
                    import shutil
                    if os.path.isdir(db_file):
                        shutil.rmtree(db_file)
                    else:
                        os.remove(db_file)
                except OSError as e:
                    print(f"Error: Cannot remove existing database {db_file}: {e}", file=sys.stderr)
                    print("Check file permissions and try again", file=sys.stderr)
                    sys.exit(1)
            else:
                base_name = out_file[:-4] if out_file.endswith('.gwb') else out_file
                print(f'The database "{base_name}" already exists. Use option -f to overwrite it.', file=sys.stderr)  # STDERR comme OCaml
                sys.exit(2)  # Return code 2 comme OCaml

        try:
            convert_gedcom_file(in_file, out_file)
        except SystemExit as e:
            sys.exit(e.code)
        except Exception as e:
            print(f"Error during conversion: {e}", file=sys.stderr)
            import traceback
            traceback.print_exc()
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nInterrupted by user", file=sys.stderr)
        sys.exit(1)
    except ValueError as e:
        if "bad -charset value" in str(e):
            print(f"{sys.argv[0]}: bad -charset value.", file=sys.stderr)
            print_usage_to_stderr()
            sys.exit(2)
        else:
            print(f"Uncaught exception: {e}", file=sys.stderr)
            import traceback
            traceback.print_exc()
            sys.exit(2)
    except Exception as e:
        print(f"Uncaught exception: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(2)
    finally:
        cleanup()

if __name__ == '__main__':
    main()

