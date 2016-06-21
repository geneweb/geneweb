#!/bin/bash

# Installation script

PWD=$1
DISTRIB=$2
RESOURCES=$3
TARGET=~/Desktop
read -p "Where do you want to install? $HOME/Desktop/[GeneWeb-7.00-Mac]:" mot

if [ "$mot" = "" ] ; then
  TARGET=$HOME/Desktop/GeneWeb-7.00-Mac
else
  TARGET=$HOME/Desktop/$mot
fi
pwd
echo "Installing GeneWeb in $TARGET"

rm -f -R $TARGET
mkdir -p $TARGET
mkdir -p $TARGET/bases

cp $PWD/$DISTRIB/CHANGES.txt  $TARGET
cp $PWD/$DISTRIB/LICENSE.txt  $TARGET
cp $PWD/$DISTRIB/README.txt   $TARGET
cp $PWD/$DISTRIB/LISEZMOI.txt $TARGET
cp $PWD/$DISTRIB/START.htm    $TARGET
cp $PWD/$DISTRIB/geneweb.command $TARGET
cp $PWD/$DISTRIB/gwd.command $TARGET
cp $PWD/$DISTRIB/gwsetup.command $TARGET
cp -R $PWD/$DISTRIB/gw $TARGET

$PWD/$RESOURCES/seticon.sh $PWD/$RESOURCES/icons/gw-folder.icns $TARGET > /dev/null
$PWD/$RESOURCES/seticon.sh $PWD/$RESOURCES/icons/gw.icns $TARGET/geneweb.command > /dev/null
$PWD/$RESOURCES/seticon.sh $PWD/$RESOURCES/icons/gwd.icns $TARGET/gwd.command > /dev/null
$PWD/$RESOURCES/seticon.sh $PWD/$RESOURCES/icons/gwsetup.icns $TARGET/gwsetup.command > /dev/null

echo "Done install"
