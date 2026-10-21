#!/bin/sh
cd `dirname "$0"`
cd bases
exec ../gw/gwsetup -bd ../bases -gd ../gw "$@"
