#!/bin/sh
# $Id: camlp4_depend.sh,v 1.1 1998-12-07 19:38:44 ddr Exp $

FILES=$*

for FILE in $FILES; do
    head -1 $FILE >/dev/null || exit 1
    set - `head -1 $FILE`
    if test "$2" = "camlp4r" -o "$2" = "camlp4o" -o "$2" = "camlp4"; then
	shift; shift
        ARGS=`echo $* | sed -e "s/[()*]//g"`
	DEPS=
	for i in $ARGS; do
	    if test `echo $i | sed "s/^\(..\).*$/\1/"` = "./"; then
		DEPS="$DEPS $i"
	    fi
	done
       if test "$DEPS" != ""; then
	    BASE=`basename $FILE .ml`
	    echo $BASE.cmo $BASE.cmx: $DEPS
        fi
    fi
done
