#!/bin/sh
cd `dirname "$0"`
exec gw/gwd -blang -log gw/gwd.log "$@" > gw/gwd.log
