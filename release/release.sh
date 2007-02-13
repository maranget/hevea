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
mv ${WORKDIR}/htmlgen/doc/manual.pdf ${WORKDIR}/final/${RELEASENAME}-manual.pdf
mv ${WORKDIR}/htmlgen/doc/doc ${WORKDIR}/final/${RELEASENAME}-manual
( cd  ${WORKDIR}/final &&
  gzip -f --best ${RELEASENAME}-manual.ps &&\
  tar cf  ${RELEASENAME}-manual.tar ${RELEASENAME}-manual &&\
  gzip -f --best ${RELEASENAME}-manual.tar &&
  /bin/rm -rf ${RELEASENAME}-manual )
( cd  ${WORKDIR}/htmlgen && make clean && /bin/rm -r doc )
mv  ${WORKDIR}/htmlgen  ${WORKDIR}/${RELEASENAME}
( cd  ${WORKDIR} && tar cf final/${RELEASENAME}.tar ${RELEASENAME} &&\
  gzip -f --best final/${RELEASENAME}.tar &&
  /bin/rm -rf ${RELEASENAME} )
#Now install files
#FTP
if $DEV
then
  /bin/rm -rf $FTPDIR/unstable
  mkdir  $FTPDIR/unstable
  ( cd $WORKDIR/final && cp * $FTPDIR/unstable )
else
  ( cd $WORKDIR/final && cp * $FTPDIR )
fi
 #HTTP
( cd $WORKDIR/final && tar zxf ${RELEASENAME}-manual.tar.gz )
if $DEV
then
  DESTDIR=${HTMLDIR}/newdoc
else
  DESTDIR=${HTMLDIR}/doc
fi
/bin/rm -rf $DESTDIR
mv $WORKDIR/final/${RELEASENAME}-manual $DESTDIR
/bin/rm -rf ${WORKDIR}/final


