#!/bin/sh
#   -- gwd.cgi --

# le script s'exécute dans cgi-bin
cd ..

# assumes distribution, or a link to distribution has been
# installed to the web-root, next to cgi-bin
BIN_DIR="./distribution/gw"
BASES_DIR="./distribution/bases"
# you may want to replace BASES_DIR according to your own base location
# see also install-cgi.sh
LOGS_DIR="./tmp"

# assumes that etc has been copied to the web-root
# if your Apache handles FollowSymLinks, 
# or if you have copied the full distribution to the web-root
# you may replace by
# export GW_STATIC_PATH="../distribution/gw/etc/"

# if you use the -robot_xcl n,s option,
# beware that each image counts as one access

"$BIN_DIR"/gwd \
  -cgi \
  -bd "$BASES_DIR" \
  -hd "$BIN_DIR" -hd "$BASES_DIR" \
  -plugins -unsafe "$BIN_DIR"/plugins \
  -log_level 7 -log "<stderr>" 2>> "$LOGS_DIR"/gwd.log
