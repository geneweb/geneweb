#!/bin/sh
cd `dirname "$0"`
exec gw/gwd --browser-lang --log gw/gwd.log "$@" > gw/gwd.log
