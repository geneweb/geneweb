#!/bin/sh
# $Id: camlp4_comm.sh,v 4.1 2001-04-13 17:55:09 ddr Exp $

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

echo $COMM $ARGS2 $ARGS1 $FILE 1>&2
$COMM $ARGS2 $ARGS1 $FILE
