#!/bin/sh
# $Id: camlp4_depend.sh,v 2.1 1999-03-08 11:19:39 ddr Exp $

FILES=
DEPARGS=
for i in $*; do
    case $i in
    *.ml*) FILES="$FILES $i";;
    *) DEPARGS="$DEPARGS $i";;
    esac
done

for FILE in $FILES; do
    head -1 $FILE >/dev/null || exit 1
    set - `head -1 $FILE`
    if test "$2" = "camlp4r" -o "$2" = "camlp4o" -o "$2" = "camlp4"; then
        COMM=$2
	shift; shift
        ARGS=`echo $* | sed -e "s/[()*]//g"`
    else
        COMM=camlp4r
	ARGS=
    fi
    echo $COMM pr_depend.cmo pa_ifdef.cmo $ARGS -- $DEPARGS $FILE >&2
    $COMM pr_depend.cmo pa_ifdef.cmo $ARGS -- $DEPARGS $FILE
done

for FILE in $FILES; do
    head -1 $FILE >/dev/null || exit 1
    set - `head -1 $FILE`
    if test "$2" = "camlp4r" -o "$2" = "camlp4o" -o "$2" = "camlp4"; then
        COMM=$2
	shift; shift
        ARGS=`echo $* | sed -e "s/[()*]//g"`
	DEPS=
	for i in $ARGS; do
	    if test `echo $i | sed "s/^\(..\).*$/\1/"` = "./"; then
		DEP=`echo $i | sed "s/^..\(.*\)$/\1/"`
		DEPS="$DEPS $DEP"
	    fi
	done
        if test "$DEPS" != ""; then
	    BASE=`basename $FILE .ml`
	    echo $BASE.cmo $BASE.cmx: $DEPS
        fi
    fi
done
