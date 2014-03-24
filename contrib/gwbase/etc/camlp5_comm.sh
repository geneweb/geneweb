#!/bin/sh
# $Id: camlp5_comm.sh,v 4.1 2007-09-04 03:06:50 deraugla Exp $

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
if test "$2" = "camlp5r" -o "$2" = "camlp5o" -o "$2" = "camlp5"; then
        COMM="$2"
        shift; shift
        ARGS2=`echo $* | sed -e "s/[()*]//g"`
else
	COMM=camlp5r
	ARGS2=
fi

echo $COMM $ARGS2 $ARGS1 $FILE
$COMM $ARGS2 $ARGS1 $FILE
