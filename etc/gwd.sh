#!/bin/sh
cd `dirname "$0"`
cd bases
exec ../gw/gwd -bd . -hd ../gw "$@" > ../gw/gwd.log 2>&1
