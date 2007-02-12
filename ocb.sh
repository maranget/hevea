#!/bin/sh -

source ./config.sh

ocamlbuild -version > /dev/null 2>/dev/null || \
case $1 in
  opt|byte|both)
     make $1-make
     exit 0
     ;;
  clean)
     exit 0
     ;;
  *) echo "Bad ocb argument '$1'"
     exit 2
esac

OCBOCAMLFLAGS=''
for i in $OCAMLFLAGS
do
  OCBOCAMLFLAGS="$OCBOCAMLFLAGS -cflag $i"
done

ocb() {
    ocamlbuild $OCBFLAGS $OCBOCAMLFLAGS $*
}

toopt () {
  for f in $*
  do
    mv $f `basename $f .native`.opt
  done
}

rule() {
  case $1 in
    clean)
      ocb -clean
      ;;
    byte)
      ocb $PGM
      ;;
    opt)
      ocb $PGMNATIVE && toopt $PGMNATIVE
      ;;
    both)
      ocb $PGM $PGMNATIVE && toopt $PGMNATIVE
      ;;
    *)      echo "Unknown action $1";;
  esac;
}

if [ $# -eq 0 ]
then
  rule opt
else
  for i
  do
    rule $i
  done
fi