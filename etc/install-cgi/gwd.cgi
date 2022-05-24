#!/bin/sh
#   -- GW3.cgi --

cd ..  # le script s'exécute dans cgi-bin

# assumes distribution, or a link to distribution has been
# installed next to cgi-bin
BIN_DIR=distribution/gw 
BASES_DIR=distribution/bases
# you may want to replace BASES_DIR according to your own base location
LOG_DIR=tmp

substr="cgi-bin/gwd.cgi"
PREFIX=${SCRIPT_NAME%%$substr*}
export GW_STATIC_PATH=$PREFIX$BIN_DIR/etc/

OPTIONS="-plugins -unsafe $BIN_DIR/plugins"
$BIN_DIR/gwd -cgi -hd $BIN_DIR -bd $BASES_DIR $OPTIONS 2>$LOG_DIR/gwd.log
