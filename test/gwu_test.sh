#!/bin/sh
#set -ex
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
ZIPREORG_IMG='galichet_reorg_documents.zip' # zip of documents for reorg DB
BASES_DIR="$HOME/Genea/GeneWeb-Bases"
DIST_DIR="./distribution"
BIN_DIR="$DIST_DIR/gw"
SUDOPRFX=   # something like 'sudo -u aSpecificId' if access fs required.
GWCOPT='-v -f -cg'
#=== hardcoded vars (end)   ===

#===  main ====================
cmd=$(basename $0)
cmddir=$(realpath $0); cmddir=$(dirname $cmddir)
echo "starting $0 $@"
while getopts "f:hr" Option
do
case $Option in
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
    mkdir -p $BASES_DIR
    if test ! -d $BASES_DIR/ ; then
        echo "$BASES_DIR/ not accessible, change your default parms."
        exit 1
    fi
fi
if test ! -f $BASES_DIR/$DBNAME.gw ; then
    if test -f $cmddir/$DBNAME.gw ; then
        cp -f  $cmddir/$DBNAME.gw $BASES_DIR/
    else
        echo "$DBNAME.gw not found in $BASES_DIR or $cmddir/"
        exit 1
    fi
else
    if test "$DBNAME" = "$REFDBNAME"; then
        rsync -a $cmddir/$DBNAME.gw $BASES_DIR/
    fi
fi

fqbindir=$(realpath $BIN_DIR)

cd $BASES_DIR
$SUDOPRFX rm -rf $DBNAME.lck $DBNAME.gwo $DBNAME.log $DBNAME.gwb $DBNAME_nouveau.gw $DBNAME.gwu_stderr outdir.$DBNAME
$SUDOPRFX mkdir outdir.$DBNAME $DBNAME.gwb $DBNAME.gwb/wiznotes || exit 1
gwcopt="$GWCOPT $optreorg"
$SUDOPRFX $fqbindir/gwc $gwcopt -o $DBNAME $DBNAME.gw >$DBNAME.log 2>&1 || \
  { echo "gwc failure, details in $BASES_DIR/$DBNAME.log"; exit 1; }

if test "$DBNAME" = "$REFDBNAME"; then
    if test -n "$optreorg"; then
        zipsrc="$cmddir/$ZIPREORG_IMG"
    else
        zipsrc="$cmddir/$ZIP_IMG"
    fi
    unzip -qu $zipsrc || { echo "stop on unzip failure"; exit 1; }
fi

$SUDOPRFX $fqbindir/gwu $DBNAME -v -o ${DBNAME}.gwu.o.gw 2>$DBNAME.gwu.o.stderr || \
  { echo "gwu failure, details in $BASES_DIR/$DBNAME.gwu.o.stderr"; exit 1; }
$SUDOPRFX $fqbindir/gwu $DBNAME -v -o ${DBNAME}_nouveau.gw -odir outdir.$DBNAME 2>$DBNAME.gwu_stderr || \
  { echo "gwu failure, details in $BASES_DIR/$DBNAME.gwu_stderr"; exit 1; }

RC=0
for xx in "${DBNAME}.gwu.o.gw" "outdir.$DBNAME/$DBNAME.gw" ; do
    if diff -q $DBNAME.gw $xx >/dev/null ; then
        : # nop
    else
        if diff -qZ $DBNAME.gw $xx >/dev/null ; then
            echo "Warning: trailing whitespace ignored"
        else
            diff -u $DBNAME.gw $xx || RC=$(($RC+1))
        fi
    fi
done

if test "$RC" != 0; then
    echo "at least $RC detected error(s)."
    exit 1
else
    echo "No detected error."
fi
