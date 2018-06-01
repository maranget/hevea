#! /bin/sh -e
. ./config.sh

cpv () {
  echo "$1 -> $2"
  cp $1 ${DESTDIR}$2
}

mkdirv () {
  mkdir -p ${DESTDIR}$1
}

install () {
  SRC=$1
  shift
  DEST=$1
  shift
  mkdirv $DEST
  for f in $*
  do
    cpv $SRC/$f $DEST
  done
}

installbin () {
  mkdirv $BINDIR
  EXT=$1
  shift
  for f in $*
  do
    cpv $f.$EXT $BINDIR/$f
  done
}

case $1 in
  byte|opt)
    TARGET=$1
    ;;
  *)
    echo "Usage: install.sh (byte|opt)" 1>&2
    exit 2
    ;;
esac

install . $LIBDIR imagen xxcharset.exe xxdate.exe contents_motif.gif next_motif.gif previous_motif.gif $ALLLIB
install . $LIBDIR contents_motif.svg next_motif.svg previous_motif.svg
install . $LATEXLIBDIR hevea.sty mathjax.sty
install html $LIBDIR/html $HTMLLIB
install text $LIBDIR/text $TEXTLIB
install info $LIBDIR/info $INFOLIB
MAPPINGS=`( cd ./mappings  && echo *.map )`
install mappings $LIBDIR/mappings $MAPPINGS
installbin $TARGET hevea hacha esponja bibhva
cpv imagen $BINDIR
