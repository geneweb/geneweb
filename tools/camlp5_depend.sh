#!/bin/bash -e

usage() {
  PROG_NAME=${0##*/}
  if [ -n "$1" ]; then
    echo "Unexpected argument '$1'" >&2
    echo >&2
  fi
  echo "Usage: $PROG_NAME [<camlp5 flags>] [-I incdir] -- <files>" >&2
  exit 1
}

ARGTYPE=CAMLP5_OTHER_OPTIONS
FILES=
CAMLP5_OTHER_OPTIONS=
INC=
while [ -n "$1" ]; do
    case $1 in
    -h|--help) usage;;
    --) [ $ARGTYPE != "FILES" ] || usage "$1"
        ARGTYPE=FILES;;
    -I) [ $ARGTYPE != "FILES" ] || usage "$1"
        shift
        [ -n "$1" ] || usage
        INC="$INC -I $1";;
    *) declare $ARGTYPE="${!ARGTYPE} $1";;
    esac
    shift
done

[ -n "$FILES" ] || usage;

CAMLP5_LOAD_OPTIONS="pr_depend.cmo pa_macro.cmo"
CAMLP5_OTHER_OPTIONS="$CAMLP5_OTHER_OPTIONS $INC"

for FILE in $FILES; do
    head -1 $FILE >/dev/null || exit 1
    set - $(head -1 $FILE)
    case "$2" in
    nocamlp5)
      COMMAND="ocamldep $INC $FILE";;
    camlp5|camlp5r|camlp5o)
      COMMAND="$2"
      shift; shift
      ARGS=$(echo $* | sed -e "s/[()*]//g")
      DEPS=
      for i in $ARGS; do
        if [[ $i =~ ^\./ ]]; then
          DEPS="$DEPS ${i:2}"
        fi
      done
      if [ -n "$DEPS" ]; then
        case $FILE in
        *.ml)  BASE=$(basename $FILE .ml);  echo $BASE.cmo $BASE.cmx: $DEPS;;
        *.mli) BASE=$(basename $FILE .mli); echo $BASE.cmi: $DEPS;;
        esac
      fi
      COMMAND="$COMMAND $CAMLP5_LOAD_OPTIONS $ARGS -- $CAMLP5_OTHER_OPTIONS $FILE";;
    *)
      COMMAND="camlp5r $CAMLP5_LOAD_OPTIONS $ARGS -- $CAMLP5_OTHER_OPTIONS $FILE";;
    esac
    echo $COMMAND $FILE >&2
    # camlp5 on Windows generates backslashes -> replace them with slashes
    $COMMAND $FILE | sed "s/[\\\\]\(.\)/\/\\1/g"
done
