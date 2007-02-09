#! /bin/sh -

source config.sh
source tags.sh

/bin/rm  -rf ${RELEASENAME}
cvs -d ${CVSDIR} export ${CVSEXPORT}  -l htmlgen htmlgen/html htmlgen/text htmlgen/info htmlgen/mappings
mv htmlgen $RELEASENAME
tar cfo ${RELEASENAME}.tar ${RELEASENAME}
gzip -f --best ${RELEASENAME}.tar
/bin/rm -rf ${RELEASENAME}
