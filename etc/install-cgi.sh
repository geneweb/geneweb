#!/bin/sh

# This script assumes that the distribution folder,
# (or a link to it) is installed at the Document root
# of the Web server (Apache or other).
# Please update the search for DocumentRoot according to
# your understanding of other environments.

# The purpose of the script is to install
# - gwd.cgi and test.cgi in the cgi-bin folder
# - a test base in the bases folder

OS_ENV=`uname` 

# Find the document root under Apache

if [ $OS_ENV = "Linux" ]; then
  if [ -f /etc/apache2/httpd.conf ]; then
    TMP=`grep -i "DocumentRoot" /etc/apache2/httpd.conf| sed '/#/d'`
  elif [ -f /etc/apache2/sites-enabled/000-default.conf ]; then
    TMP=`grep DocumentRoot /etc/apache2/sites-enabled/000-default.conf | sed '/#/d'`
  fi
elif [ $OS_ENV = "Darwin" ]; then
  TMP="$HOME/Sites"
else
  echo "Please find value of DocumentRoot of your server and set it manually"
fi

WEB_ROOT=`echo $TMP | sed -e "s/DocumentRoot//g" \
        | sed "s/^[ \t]*//" | sed -e 's/"//g'`

# You might change the distribution name to your liking
# but this is not recommended.
# The name "distribution" seems to be hard-coded in some places (gwd.ml)
DISTRIB_NAME="distribution"
# Provide the location of your bases.
# If no value is given, the default is $DISTRIB_NAME/bases 
MY_BASES="/Users/Henri/Genea/GeneWeb-Bases"
DIR="$(dirname "$0")"
cd "$DIR"
PWD=`pwd`

if ! [ $MY_BASES == "" ]; then
  echo "Set link to my bases"
  rm -f -R $WEB_ROOT/$DISTRIB_NAME/bases
  ln -s $MY_BASES $WEB_ROOT/$DISTRIB_NAME/bases
fi
BASES=$WEB_ROOT/$DISTRIB_NAME/bases
echo "Bases: $BASES"

# Apache follows SymLinks
#if ! [ -d $WEB_ROOT/$DISTRIB_NAME ]; then
#  ln -s ../$DISTRIB_NAME $WEB_ROOT
#fi
# Apache does not follow SymLinks
rm -f -R $WEB_ROOT/$DISTRIB_NAME
cp -f -R ../$DISTRIB_NAME $WEB_ROOT

cd ./install-cgi

BIN_DIR=$WEB_ROOT/$DISTRIB_NAME/gw
if [ -d $WEB_ROOT/cgi-bin ]; then
  cp gwd.cgi $WEB_ROOT/cgi-bin
  cp test.cgi $WEB_ROOT/cgi-bin
  cp Lenna.jpg $WEB_ROOT
  chmod +x $WEB_ROOT/cgi-bin/gwd.cgi
else
  echo "missing cgi-bin"
  exit -1
fi

LOG_DIR="tmp"
if ! [ -d $WEB_ROOT/$LOG_DIR ]; then
  mkdir -f $WEB_ROOT/$LOG_DIR
fi

# Copy test base elements
rm -f -R $BASES/test.*
cp test.gwf $BASES
if ! [ -d $BASES/src ]; then
  mkdir $BASES/src
fi
if ! [ -d $BASES/src/test ]; then
  mkdir $BASES/src/test
fi
if ! [ -d $BASES/src/test/images ]; then
  mkdir $BASES/src/test/images
fi

if ! [ -d $BASES/images ]; then
  mkdir $BASES/images
fi
if ! [ -d $BASES/images/test ]; then
  mkdir $BASES/images/test
fi

cp Lenna.jpg $BASES/src/test/images/aatest.jpg
cp Lenna.jpg $BASES/images/test/tiny.0.mouse.jpg
cp Lenna-full.jpg $BASES/src/test/images/
cp Lenna-icon.jpg $BASES/src/test/images/

$BIN_DIR/gwc -f -o $BASES/test test.gw

open "http://localhost/~$USER/cgi-bin/gwd.cgi?b=test"

