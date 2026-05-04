#!/bin/sh
cd `dirname "$0"`
exec gw/gwd \
  --bd ./bases \
  --setup-link \
  --log gw/gwd.log \
  "$@"
