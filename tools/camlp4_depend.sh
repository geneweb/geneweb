#!/bin/sh
# $Id: camlp4_depend.sh,v 1.2 1998-12-13 11:56:44 ddr Exp $

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
