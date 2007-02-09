#!/bin/sh

FLAGS=
OCAMLBUILD=ocamlbuild

ocb()
{
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)
      ocb -clean
      ;;
    byte)
      ocb hevea.byte hacha.byte esponjamain.byte bibhva.byte && \
      mv esponjamain.byte esponja.byte
      ;;
    opt)
      ocb hevea.native hacha.native esponjamain.native bibhva.native && \
      mv hevea.native hevea.opt && \
      mv hacha.native hacha.opt && \
      mv esponjamain.native esponja.opt && \
      mv bibhva.native bibhva.opt
      ;;
    *)      echo "Unknown action $1";;
  esac;
}

if [ $# -eq 0 ]; then
  rule opt
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi