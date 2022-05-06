#!/bin/sh
#   -- GW3.cgi --

BIN_DIR=distribution/gw
BASES_DIR=distribution/bases
cd ..  # le script s'exécute dans cgi-bin
$BIN_DIR/gwd -cgi -hd $BIN_DIR -bd $BASES_DIR 2>./gwd.log  
