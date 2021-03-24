#! /bin/sh -e
. ./config.sh

PGM="hevea hacha esponja bibhva"

for f in $PGM imagen
do
    rm -f $BINDIR/$(basename $f .byte)
done

rm -rf $LIBDIR

