#!/bin/sh
# $Id: camlp4_comm.sh,v 4.0 2001-03-16 19:35:13 ddr Exp $

ARGS1=
FILE=
while test "" != "$1"; do
        case $1 in
	*.ml*) FILE=$1;;
	*) ARGS1="$ARGS1 $1";;
	esac
        shift
done

head -1 $FILE >/dev/null || exit 1

set - `head -1 $FILE`
if test "$2" = "camlp4r" -o "$2" = "camlp4o" -o "$2" = "camlp4"; then
        COMM="$2"
        shift; shift
        ARGS2=`echo $* | sed -e "s/[()*]//g"`
else
	COMM=camlp4r
	ARGS2=
fi

echo $COMM $ARGS2 $ARGS1 $FILE
$COMM $ARGS2 $ARGS1 $FILE
