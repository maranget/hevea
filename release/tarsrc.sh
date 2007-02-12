#! /bin/sh -

. config.sh
. tag.sh

buildtar () {
  DIRNAME=$1
  shift
  /bin/rm  -rf ${RELEASENAME}
  cvs -d ${CVSDIR} export ${CVSEXPORT}  -l $*
  mv htmlgen ${DIRNAME}
  tar cfo ${DIRNAME}.tar ${DIRNAME}
  gzip -f --best ${DIRNAME}.tar
  /bin/rm -rf ${DIRNAME}
}

buildtar ${RELEASENAME}  htmlgen htmlgen/html htmlgen/text htmlgen/info htmlgen/mappings