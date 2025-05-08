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

echo Web-root: $WEB_ROOT

# You might change the distribution name to your liking
# but this is not recommended.
# The name "distribution" seems to be hard-coded in some places (gwd.ml)
DISTRIB_NAME="distribution"
# Provide the location of your bases.
# If no value is given, the default is $DISTRIB_NAME/bases 
MY_BASES=""
DIR=$(dirname $0)
INSTALL_CGI="$DIR/install-cgi"
PWD=`pwd`
echo `pwd`

BASES=$DIR/../$DISTRIB_NAME/bases
echo "Bases: $BASES"

BIN_DIR=$WEB_ROOT/$DISTRIB_NAME/gw
if [ -d $WEB_ROOT/cgi-bin ]; then
  echo "Copy some files to Web-root"
  cp $INSTALL_CGI/gwd.cgi $WEB_ROOT/cgi-bin
  cp $INSTALL_CGI/test.cgi $WEB_ROOT/cgi-bin
  cp $INSTALL_CGI/Lenna.jpg $WEB_ROOT
  chmod +x $WEB_ROOT/cgi-bin/gwd.cgi
  if [ $OS_ENV = "Darwin" ]; then
    # Apple extended attributes
    xattr -d com.apple.quarantine $WEB_ROOT/gwd.cgi
  fi
else
  echo "missing cgi-bin"
  exit -1
fi

# To verify that Apache works, execute test.cgi from the cgi-bin folder

# (base) Henri@iMac-H cgi-bin % ./test.cgi 
# Content-type: text/html

# <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"   
#   "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">  
# <html xmlns="http://www.w3.org/1999/xhtml">  
# <body>
# This is a test for cgi commands<br>
# This should display Lenna:<br>
# <img src="/Lenna.jpg">
# <br>
# End of test
# </body>
# </html>
# $ % 

# next step is to do the same from your browser :
# http://localhost/~Username/cgi-bin/test.cgi
# you should get:
# This is a test for cgi commands
# This should display Lenna:
# <image of Lenna.jpg>
# End of test 

LOG_DIR="tmp"
if ! [ -d $WEB_ROOT/$LOG_DIR ]; then
  echo "Make tmp dir"
  mkdir -f $WEB_ROOT/$LOG_DIR
fi

if ! [ -d $DIR/../$DISTRIB_NAME ]; then
  echo "$DIR/../$DISTRIB_NAME does not exist!"
  exit 1
fi

echo "Copy test base elements"
if ! [ -d $BASES ]; then
  mkdir $BASES
fi
rm -f -R $BASES/test.*
cp $INSTALL_CGI/test.gwf $BASES
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

cp $INSTALL_CGI/Lenna.jpg $BASES/src/test/images/aatest.jpg
cp $INSTALL_CGI/Lenna.jpg $BASES/images/test/tiny.0.mouse.jpg

echo "Create test base in $BASES"
$BIN_DIR/gwc -f -bd $BASES -o test $INSTALL_CGI/test.gw

echo "Create galichet base in $BASES"
$DIR/gwu_test.sh

rm -f -R $WEB_ROOT/$DISTRIB_NAME
# Apache follows SymLinks
#echo "Create sym link to distribution"
#ln -s $DIR/../$DISTRIB_NAME $WEB_ROOT
# Apache does not follow SymLinks
echo "Copy distribution to WEB_ROOT"
cp -R $DIR/../$DISTRIB_NAME $WEB_ROOT

echo "Open test base"

open "http://localhost/~$USER/cgi-bin/gwd.cgi?b=test"

