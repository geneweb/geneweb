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

DIR=`dirname $0`/
BASE=`dirname $0`/bases

cd $DIR
DIR=$PWD
export LANG

# echo -n "]2;GeneWeb"
# echo -n "]1;GeneWeb"

pids=""
trap 'kill $pids' 1 2

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
pid=$!
sleep 1
if test "`ps $pid | wc -l`" -ne 2; then
  if test "$LANG" = "fr"; then echo Echec; else echo Failed; fi
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
pids="$pids $!"

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

cat
