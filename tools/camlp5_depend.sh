#!/bin/sh -e
# $Id: camlp5_depend.sh,v 5.2 2009-03-11 10:56:09 ddr Exp $

FILES=
DEPARGS=
while [ "$1" != "" ]; do
    case $1 in
    *.ml*) FILES="$FILES $1";;
    *) DEPARGS="$DEPARGS $1";;
    esac
    shift
done
PR_DEP="-I $TOP/src pr_dep.cmo"

for FILE in $FILES; do
    head -1 $FILE >/dev/null || exit 1
    set - $(head -1 $FILE)
    if test "$2" = "camlp5r" -o "$2" = "camlp5o" -o "$2" = "camlp5"; then
	COMM=$(echo "$2" | sed -e 's/camlp5/camlp5/g')
        shift; shift
        ARGS=$(echo $* | sed -e "s/[()*]//g")
    else
        COMM=camlp5r
        ARGS=
    fi
    ARGS2="$DEPARGS"
    echo $COMM $PR_DEP pa_macro.cmo $ARGS -- $ARGS2 $FILE >&2
    $COMM $PR_DEP pa_macro.cmo $ARGS -- $ARGS2 $FILE
done

for FILE in $FILES; do
    head -1 $FILE >/dev/null || exit 1
    set - $(head -1 $FILE)
    if test "$2" = "camlp5r" -o "$2" = "camlp5o" -o "$2" = "camlp5"; then
	COMM=$(echo "$2" | sed -e 's/camlp5/camlp5/g')
	shift; shift
        ARGS=$(echo $* | sed -e "s/[()*]//g")
	DEPS=
	for i in $ARGS; do
	    if test $(echo $i | sed "s/^\(..\).*$/\1/") = "./"; then
		DEP=$(echo $i | sed "s/^..\(.*\)$/\1/")
		DEPS="$DEPS $DEP"
	    fi
	done
        if test "$DEPS" != ""; then
	    BASE=$(basename $FILE .ml)
	    echo $BASE.cmo $BASE.cmx: $DEPS
        fi
    fi
done
