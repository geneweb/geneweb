#!/usr/bin/env bash
set -euo pipefail

: ${HTTP_COOKIE=""}
: ${CONTENT_TYPE="text/html; encoding UTF-8"}
: ${HTTP_ACCEPT_LANGUAGE="en"}
: ${HTTP_ACCEPT_ENCODING="UTF-8"}
: ${HTTP_REFERER=""}
: ${HTTP_USER_AGENT="Oz"}

GWD_BIN="$GWD_BIN"

# HACK: The dune sandbox prevents us from accessing assets files directly.
# The template engine prints absolute paths of ressources, which is not
# predictable and break cram tests. The right solution is to abstract the
# gw_prefix in all the paths and print a dummy value in predictable mode.
# This solution is to extensive and should be implemented later.
GW_PREFIX="../../../../../install/default/share/geneweb/hd"

QUERY_STRING="${1-}"

echo "=========== QUERY_STRING: $QUERY_STRING ========="

export HTTP_COOKIE
export CONTENT_TYPE
export HTTP_ACCEPT_LANGUAGE
export HTTP_ACCEPT_ENCODING
export HTTP_REFERER
export HTTP_USER_AGENT
export QUERY_STRING

"$GWD_BIN" \
  --predictable-mode \
  --cgi \
  --gw-prefix "$GW_PREFIX" \
  --bd .
