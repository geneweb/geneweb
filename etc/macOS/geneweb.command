#!/bin/sh

case "$LANG" in
  de*) LANG=de;;
  es*) LANG=es;;
  fr*) LANG=fr;;
  it*) LANG=it;;
  lv*) LANG=lv;;
  sv*) LANG=sv;;
  *) LANG=en;;
esac

DIR=`dirname "$0"`/
BASE=`dirname "$0"`/bases

cd "$DIR"
DIR="$PWD"
export LANG

gwd_pid=`ps -ef|grep '/gwd'|grep -v grep|awk '{print $2}'`
kill $gwd_pid
gws_pid=`ps -ef|grep '/gwsetup'|grep -v grep|awk '{print $2}'`
kill $gws_pid

if [ -f gwsetup.log ]; then
  mv gwsetup.log gwseup.log.old
fi

mkdir -p "$BASE"
cd "$BASE"

if [ -f gwd.log ]; then
  mv gwd.log gwd.log.old
fi

if test "$LANG" = "fr"; then
  echo "Demarrage de gwsetup..."
else
  echo "Starting gwsetup..."
fi

"$DIR/gw/gwsetup" -gd "$DIR/gw" -lang $LANG > gwsetup.log 2>&1 &
sleep 1
gws_pid=`ps -ef|grep '/gwsetup'|grep -v grep|awk '{print $2}'`
if test "$gws_pid" = ""; then
  if test "$LANG" = "fr"; then echo Echec gwsetup; else echo Failed gwsetup; fi
  cat
  exit 1
fi
pids="$pids $pid"

if test "$LANG" = "fr"; then
  echo "Demarrage de gwd..."
else
  echo "Starting gwd..."
fi
"$DIR/gw/gwd" -hd "$DIR/gw" > gwd.log 2>&1 &
sleep 1
gwd_pid=`ps -ef|grep '/gwd'|grep -v grep|awk '{print $2}'`
if test "$gwd_pid" = ""; then
  if test "$LANG" = "fr"; then echo Echec gwd; else echo Failed gwd; fi
  cat
  exit 1
fi

echo
if test "$LANG" = "fr"; then
  echo "Gardez cette fenetre ouverte tant que"
  echo "vous voulez utiliser GeneWeb dans votre"
  echo "navigateur"
else
  echo "Keep this window open while you"
  echo "are using GeneWeb on your browser"
fi

open "$DIR/START.htm"

/usr/bin/osascript -e 'tell application "Terminal" to set miniaturized of first window whose name contains "GeneWeb" to true'

cat
