#! /bin/sh -

cd `dirname $0`/..
. release/config.sh
. release/tag.sh


#Clean-up 
/bin/rm -rf ${WORKDIR}/htmlgen ${WORKDIR}/${RELEASENAME}

#build source tar (with doc) 
( cd $WORKDIR ;
  cvs -d ${CVSDIR} export ${CVSEXPORT} -l htmlgen htmlgen/html htmlgen/text htmlgen/info htmlgen/mappings htmlgen/doc htmlgen/doc/thai )

#Recompile (test)
( cd $WORKDIR/htmlgen ; make opt )
#Recompile (produce doc)
( cd $WORKDIR/htmlgen/doc ; make manual.ps manual.pdf opt docclean )
#Make final files with their final names
/bin/rm -rf ${WORKDIR}/final
mkdir -p  ${WORKDIR}/final
mv ${WORKDIR}/htmlgen/doc/manual.ps ${WORKDIR}/final/${RELEASENAME}-manual.ps
gzip -f --best ${RELEASENAME}-manual.ps
mv ${WORKDIR}/htmlgen/doc/manual.pdf ${WORKDIR}/final/${RELEASENAME}-manual.pdf
mv  ${WORKDIR}/htmlgen/doc/doc ${WORKDIR}/final/${RELEASENAME}-manual
( cd  ${WORKDIR}/final &&
  tar cf  ${RELEASENAME}-manual.tar ${RELEASENAME}-manual &&\
  gzip -f --best ${RELEASENAME}-manual.tar )

