#!/bin/sh
cd `dirname "$0"`
exec gw/gwd \
  --bd ./bases \
  --log gw/gwd.log \
  "$@"
