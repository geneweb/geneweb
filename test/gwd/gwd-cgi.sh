#!/usr/bin/env bash
set -euo pipefail

: ${HTTP_COOKIE=""}
: ${CONTENT_TYPE="text/html; encoding utf-8"}
: ${HTTP_ACCEPT_LANGUAGE="en"}
: ${HTTP_ACCEPT_ENCODING="utf-8"}
: ${HTTP_REFERER=""}
: ${HTTP_USER_AGENT="Oz"}

GWD_BIN="$GWD_BIN"
GW_PREFIX="../../hd"
BASE_DIR="./bases"
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
  -debug \
  -predictable_mode \
  -blang \
  -cgi \
  -bd "$BASE_DIR" \
  -hd "$GW_PREFIX"
