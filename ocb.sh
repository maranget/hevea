#!/bin/sh -e
CHECK=yes
export CHECK
. ./config.sh

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
