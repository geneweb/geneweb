#!/bin/sh
cd `dirname "$0"`
exec gw/gwd -blang -log "<stderr>" $@" >> gw/gwd.log
