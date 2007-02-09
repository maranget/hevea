#!/bin/sh -

source ./config.sh

ocamlbuild -version > /dev/null 2>/dev/null || \
case $1 in
  opt|byte|both)
     make $1-make
     exit 0
     ;;
  clean)
     ;;
  *) echo "Bad ocb argument '$1'"
     exit 2
esac

ocb() {
    ocamlbuild -j 2 -classic-display $*
}

toopt () {
  for f in $*
  do
    mv $f `basename .native`.opt
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