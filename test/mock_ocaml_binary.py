#!/usr/bin/env python3

import os
import sys

def create_mock_ged2gwb():
    """Crée un binaire OCaml simulé pour ged2gwb"""

    # Texte d'aide exact de ged2gwb
    help_text = '''Usage: ged2gwb [<ged>] [options] where options are:
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
  --help            Display this list of options'''

    mock_script = f'''#!/bin/bash
# Mock ged2gwb binary for Golden Master testing

if [[ "$1" == "-help" ]]; then
    cat << 'EOF'
{help_text}
EOF
    exit 0
fi

# Handle other arguments
gedcom_file=""
force=false
output=""

for arg in "$@"; do
    case $arg in
        -f)
            force=true
            ;;
        -o)
            shift
            output="$1"
            ;;
        *.ged)
            gedcom_file="$arg"
            ;;
    esac
done

if [[ -n "$gedcom_file" ]]; then
    base_name=$(basename "$gedcom_file" .ged)
    if [[ -n "$output" ]]; then
        base_name="$output"
    fi

    echo "Mode: classic, for base ./$base_name.gwb" >&2

    # Check if database exists
    if [[ -f "$base_name.gwb" ]] && [[ "$force" != true ]]; then
        echo "The database \\"$base_name\\" already exists. Use option -f to overwrite it." >&2
        exit 2
    fi

    # Simulate processing
    echo "*** pass 1 (note)" >&2
    echo "*** pass 2 (indi)" >&2
    echo "*** pass 3 (fam)" >&2
    echo "*** Trailer ok" >&2
    echo "*** saving persons array" >&2
    echo "*** saving ascends array" >&2
    echo "*** saving unions array" >&2
    echo "*** saving families array" >&2
    echo "*** saving couples array" >&2
    echo "*** saving descends array" >&2
    echo "*** saving strings array" >&2
    echo "*** create name index" >&2
    echo "*** create strings of sname" >&2
    echo "*** create strings of fname" >&2
    echo "*** create string index" >&2
    echo "*** create surname index" >&2
    echo "*** create first name index" >&2
    echo "*** ok" >&2
    exit 0
fi

exit 0
'''

    os.makedirs("distribution/gw", exist_ok=True)

    with open("distribution/gw/ged2gwb", "w") as f:
        f.write(mock_script)

    os.chmod("distribution/gw/ged2gwb", 0o755)
    print("✅ Mock ged2gwb binary created")

if __name__ == "__main__":
    create_mock_ged2gwb()
