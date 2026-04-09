#!/bin/sh
cd `dirname "$0"`
exec gw/gwd \
  --bd ./bases \
  --setup-link \
  --browser-lang \
  --log gw/gwd.log \
  "$@"
