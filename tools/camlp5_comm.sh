#!/bin/bash

ARGS="pa_macro.cmo"
OUTPUT=
FILE=
while [ -n "$1" ]; do
    case $1 in
    *.ml*) FILE=$1;;
    -o) OUTPUT=$2; ARGS="$ARGS $1";;
    *) ARGS="$ARGS $1";;
    esac
    shift
done

head -1 $FILE >/dev/null || exit 1

set - $(head -1 $FILE)
case "$2" in
nocamlp5)
  COMMAND="ln -fs $FILE $OUTPUT";;
camlp5|camlp5r|camlp5o)
  COMMAND="$2"
  shift; shift
  MORE_ARGS=$(echo $* | sed -e "s/[()*]//g")
  COMMAND="$COMMAND $MORE_ARGS $ARGS $FILE";;
*)
  COMMAND="camlp5r $ARGS $FILE";;
esac

echo $COMMAND 1>&2
$COMMAND
