#!/bin/sh
#   -- gwd.cgi --

# the script exécutes in cgi-bin (hence the ../)
# assumes that the distribution, or a link to the distribution has been
# installed, in a folder names distribution, next to cgi-bin
# these parameters may vary according to your Apache configuration
BIN_DIR="../distribution/gw"
BASES_DIR="../distribution/bases"
# you may want to replace BASES_DIR according to your own base location
# see also install-cgi.sh
LOGS_DIR="../tmp"

# if you use the -robot_xcl n,s option,
# beware that each image counts as one access

"$BIN_DIR"/gwd \
  -cgi \
  -bd "$BASES_DIR" \
  -hd "$BIN_DIR" \
  -etc_prefix "../distribution/gw/etc" \
  -blang \
  -plugins -unsafe "$BIN_DIR"/plugins \
  -log_level 7 -log "<stderr>" 2>> "$LOGS_DIR"/gwd.log
