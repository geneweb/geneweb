#!/bin/bash

set -e

_show_help () {
    echo "Build and run geneweb in the docker container"
    echo ""
    echo "USAGE" 
    echo "./install [--options]"
    echo ""
    echo "OPTIONS"
    echo "--api: adds API build" 
    echo "--clean: cleans before build " 
    echo "--run: runs geneweb and gwsetup" 
    echo "--help: this beautiful help" 
    echo ""
}

WITH_API=false
WITH_CLEAN=false
WITH_RUN=false

for i in "$@"
do 
case $i in 
    --api)
    WITH_API=true 
    ;;
     --clean)
    WITH_CLEAN=true
    ;;
    --run)
    WITH_RUN=true
    ;;
    --help|-h|*)
    _show_help
    ;;
esac
done

cd /home/opam/geneweb

if [ "$WITH_API" = true ]; then 
    ./configure --api
else 
    ./configure
fi

if [ "$WITH_CLEAN" = true ]; then 
    make clean
fi

make distrib

if [ "$WITH_RUN" = true ]; then

    /sbin/ip route|awk '/default/ { print $3 }' > distribution/gw/only.txt

    export BASE_DIR="/home/opam/bases/"
    export BIN_DIR="/home/opam/geneweb/distribution/gw/"

    $BIN_DIR/gwd -hd $BIN_DIR -bd $BASE_DIR &

    cd $BASE_DIR;

    $BIN_DIR/gwsetup -lang fr -gd $BIN_DIR -bindir $BIN_DIR
fi
