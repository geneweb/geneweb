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
  TMP=`grep -i "DocumentRoot" /etc/apache2/httpd.conf | sed '/#/d'`
else
  echo "Please find value of DocumentRoot of your server and set it manually"
fi

WEB_ROOT=`echo $TMP | sed -e "s/DocumentRoot//g" \
        | sed "s/^[ \t]*//" | sed -e 's/"//g'`

echo Document root is: $WEB_ROOT

BIN_DIR=$WEB_ROOT/distribution/gw
BASES=$WEB_ROOT/distribution/bases

DIR="$(dirname "$0")"
cd "$DIR"

if [ -d $WEB_ROOT/cgi-bin ]; then
  cp gwd.cgi $WEB_ROOT/cgi-bin
  cp test.cgi $WEB_ROOT/cgi-bin
  cp Lenna.jpg $WEB_ROOT
else
  echo "missing cgi-bin"
  exit -1
fi

chmod +x $WEB_ROOT/cgi-bin/gwd.cgi

cp test.gwf $BASES
$BIN_DIR/gwc -f -o $BASES/test test.gw
