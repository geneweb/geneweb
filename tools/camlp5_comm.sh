#!/bin/sh
# $Id: camlp5_comm.sh,v 5.2 2010-09-23 17:16:49 ddr Exp $

ARGS1="../wserver/pa_macro5.cmo"
FILE=
while test "" != "$1"; do
	case $1 in
	*.ml*) FILE=$1;;
	*) ARGS1="$ARGS1 $1";;
	esac
	shift
done

head -1 $FILE >/dev/null || exit 1

set - $(head -1 $FILE)
if test "$2" = "camlp5r" -o "$2" = "camlp5o" -o "$2" = "camlp5"; then
	COMM=$(echo "$2" | sed -e 's/camlp5/camlp5/g')
	shift; shift
	ARGS2=$(echo $* | sed -e "s/[()*]//g")
else
	COMM=camlp5r
	ARGS2=
fi

echo $COMM $ARGS2 $ARGS1 $FILE 1>&2
$COMM $ARGS2 $ARGS1 $FILE
