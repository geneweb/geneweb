#!/bin/sh
set -e
usage()
{
echo "Usage: $cmd [Options]
Compare gwc input and gwu output expecting no changes.
By default:
* assume we are running as test/$cmd
  in the repo folder where geneweb was built
  to access gwc/gwu tools in $BIN_DIR
* gwc is using test/$REFDBNAME.gw input file and creating
  $BASES_DIR/$REFDBNAME.gwb database.
* if using $REFDBNAME then unzip related images to prepare DB
  for usage by test/run_gw_test.sh other tool.
If needed use -f option to change from default.

Options:
-d  print out some debug traces
-f  file to be sourced in to overwrite hardcoded vars
-h  To display this help.
-r  To pass -reorg option to gwc
    (if not set then gwc will create a legacy DB)
"
exit 1
}

setenv_file="./test-gw-vars.txt"

REFDBNAME='galichet' # reference gw file
#=== hardcoded vars (start) ===
# assumes we are running in the repo folder
# ./test/testgwu.sh
DBNAME='galichet' # name of gw file input to gwc (w/o extension)
ZIP_IMG='galichet_src_images.zip' # zip of images and src files for legacy DB
DIST_DIR="./distribution"
BIN_DIR="$DIST_DIR/gw"
BASES_DIR="$DIST_DIR/bases"
SUDOPRFX=   # something like 'sudo -u aSpecificId' if access fs required.
GWCOPT='-v -f -cg'
#=== hardcoded vars (end)   ===

#===  main ====================
cmd=$(basename $0)
cmddir=$(dirname $0)
echo "$0 $@ started"
while getopts "df:hr" Option
do
case $Option in
    d ) debug=1;;
    f ) setenv_file=$OPTARG
        test -f "$setenv_file" || \
            { echo "invalid -f $setenv_file  option file"; exit 1; }
        ;;
    h ) usage;;
    r ) optreorg='-reorg';;
    * ) usage;;
esac
done
shift $(($OPTIND - 1))

# overwrite above hardcoded vars by an input file.
test -f "$setenv_file" && . $setenv_file

if test ! -d $BASES_DIR/ ; then
    $SUDOPRFX mkdir -pm 775 $BASES_DIR
    if test ! -d $BASES_DIR/ ; then
        echo "$BASES_DIR/ not accessible, change your default parms."
        exit 1
    fi
    # for Legacy, bypass to avoid gwd Warning messages  (as per issue #2143)
    if test ! -n "$optreorg"; then
        for xx in cnt etc lang; do
            $SUDOPRFX mkdir -m 755 $BASES_DIR/$xx
        done
    fi
fi
if test ! -f $BASES_DIR/$DBNAME.gw ; then
    if test -f $cmddir/$DBNAME.gw ; then
        $SUDOPRFX cp -f  $cmddir/$DBNAME.gw $BASES_DIR/
    else
        echo "$DBNAME.gw not found in $BASES_DIR or $cmddir/"
        exit 1
    fi
else
    if test "$DBNAME" = "$REFDBNAME"; then
        $SUDOPRFX rsync -a $cmddir/$DBNAME.gw $BASES_DIR/
    fi
fi

for xx in .lck .gwo .log .gwb _nouveau.gw .gwu_stderr _outdir; do
    $SUDOPRFX rm -rf $BASES_DIR/${DBNAME}${xx}
done
for xx in .gwb _outdir; do
    $SUDOPRFX mkdir $BASES_DIR/${DBNAME}${xx} || exit 1
done

gwcopt="$GWCOPT -bd $BASES_DIR $optreorg"
test -n "$debug" && set -x
$SUDOPRFX $BIN_DIR/gwc $gwcopt -o $DBNAME $BASES_DIR/$DBNAME.gw >$BASES_DIR/$DBNAME.log 2>&1 || \
  { echo "gwc failure, details in $BASES_DIR/$DBNAME.log"; exit 1; }
test -n "$debug" && cat $BASES_DIR/$DBNAME.log

if test "$DBNAME" = "$REFDBNAME"; then
    tmpdir="$BASES_DIR/unzip_tmp"
    mkdir -p $tmpdir && rm -rf $tmpdir/*
    unzip -q $cmddir/$ZIP_IMG -d $tmpdir
    if test -n "$optreorg"; then
        tmpname=$BASES_DIR/${DBNAME}.gwb/documents
        $SUDOPRFX mkdir -p $tmpname/portraits
        $SUDOPRFX cp -Rp $tmpdir/src/$DBNAME/* $tmpname/
        $SUDOPRFX cp -Rp $tmpdir/images/$DBNAME/* $tmpname/portraits/
    else
        for xx in src images; do
            tmpname=$xx/$DBNAME
            test -e $BASES_DIR/$tmpname || $SUDOPRFX mkdir -p $BASES_DIR/$tmpname
            $SUDOPRFX cp -Rp $tmpdir/$tmpname/* $BASES_DIR/$tmpname/
        done
    fi
    rm -rf $tmpdir
fi

$SUDOPRFX $BIN_DIR/gwu $BASES_DIR/$DBNAME -v -o $BASES_DIR/${DBNAME}.gwu.o.gw 2>$BASES_DIR/$DBNAME.gwu.o.stderr || \
  { echo "gwu failure, details in $BASES_DIR/$DBNAME.gwu.o.stderr"; exit 1; }
test -n "$debug" && cat $BASES_DIR/$DBNAME.gwu.o.stderr
$SUDOPRFX $BIN_DIR/gwu $BASES_DIR/$DBNAME -v -o $BASES_DIR/${DBNAME}_nouveau.gw -odir $BASES_DIR/outdir.$DBNAME 2>$BASES_DIR/$DBNAME.gwu_stderr || \
  { echo "gwu failure, details in $BASES_DIR/$DBNAME.gwu_stderr"; exit 1; }
test -n "$debug" && cat $BASES_DIR/$DBNAME.gwu_stderr

RC=0
for xx in "${DBNAME}.gwu.o.gw" "outdir.$DBNAME/$DBNAME.gw" ; do
    if diff -q $BASES_DIR/$DBNAME.gw $BASES_DIR/$xx >/dev/null ; then
        : # nop
    else
        if diff -qZ $BASES_DIR/$DBNAME.gw $BASES_DIR/$xx >/dev/null ; then
            echo "Warning: trailing whitespace ignored"
        else
            diff -u $BASES_DIR/$DBNAME.gw $BASES_DIR/$xx || RC=$(($RC+1))
        fi
    fi
done

$SUDOPRFX $BIN_DIR/update_nldb -bd $BASES_DIR $DBNAME  >$BASES_DIR/$DBNAME.update_nldb.log 2>&1 || \
  { echo "update_nldb failure, details in $BASES_DIR/$DBNAME.update_nldb.log"; exit 1; }

if test "$RC" != 0; then
    echo "$0 failed, at least $RC detected error(s)."
    exit 1
else
    echo "$0 completed, No detected error."
fi
