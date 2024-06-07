#!/bin/sh
#set -ex
usage()
{
echo "Usage: $cmd [Options]
Compare gwc input and gwu output expecting no changes.
Need to properly set 'hardcoded vars' in script header.
Options:
-f  file to be sourced in to overwrite hardcoded vars
-h  To display this help.
"
exit 1
}

setenv_file="./test-gw-vars.txt"

#=== hardcoded vars (start) ===
# assumes we are running in the repo folder
# ./test/testgwu.sh
TESTGW='testgalichet' # name of gw file input to gwc (w/o extension)
BASES_DIR="$HOME/Genea/GeneWeb-Bases"
DIST_DIR="./distribution"
BIN_DIR="$DIST_DIR/gw"
SUDOPRFX=   # something like 'sudo -u aSpecificId' if access fs required.
#=== hardcoded vars (end)   ===

#===  main ====================
cmd=$(basename $0)
while getopts "f:h" Option
do
case $Option in
    f ) setenv_file=$OPTARG
        test -f "$setenv_file" || \
            { echo "invalid -f $setenv_file  option file"; exit 1; }
        ;;
    h ) usage;;
    * ) usage;;
esac
done
shift $(($OPTIND - 1))

# overwrite above hardcoded vars by an input file.
test -f "$setenv_file" && . $setenv_file

if test ! -f $BASES_DIR/$TESTGW.gw ; then
    if test -f test/$TESTGW.gw ; then
        cp -f  test/$TESTGW.gw $BASES_DIR/
    else
        echo "$TESTGW.gw not found in $BASES_DIR or test/"
        exit 1
    fi
fi

fqbindir=$(realpath $BIN_DIR)

cd $BASES_DIR
$SUDOPRFX rm -rf $TESTGW.lck $TESTGW.gwo $TESTGW.log $TESTGW.gwb $TESTGW_nouveau.gw $TESTGW.gwu_stderr outdir.$TESTGW
$SUDOPRFX mkdir outdir.$TESTGW $TESTGW.gwb $TESTGW.gwb/wiznotes || exit 1
$SUDOPRFX $fqbindir/gwc -v -f -cg -o $TESTGW $TESTGW.gw >$TESTGW.log 2>&1 || \
  { echo "gwc failure, details in $TESTGW.log"; exit 1; }
$SUDOPRFX $fqbindir/gwu $TESTGW -v -o ${TESTGW}.gwu.o.gw 2>$TESTGW.gwu.o.stderr || \
  { echo "gwu failure, details in $TESTGW.gwu.o.stderr"; exit 1; }
$SUDOPRFX $fqbindir/gwu $TESTGW -v -o ${TESTGW}_nouveau.gw -odir outdir.$TESTGW 2>$TESTGW.gwu_stderr || \
  { echo "gwu failure, details in $TESTGW.gwu_stderr"; exit 1; }

RC=0
for xx in "${TESTGW}.gwu.o.gw" "outdir.$TESTGW/$TESTGW.gw" ; do
    if diff -q $TESTGW.gw $xx >/dev/null ; then
        : # nop
    else
        if diff -qZ $TESTGW.gw $xx >/dev/null ; then
            echo "Warning: trailing whitespace ignored"
        else
            diff -u $TESTGW.gw $xx || RC=$(($RC+1))
        fi
    fi
done

if test "$RC" != 0; then
    echo "at least $RC detected error(s)."
    exit 1
else
    echo "No detected error."
fi
