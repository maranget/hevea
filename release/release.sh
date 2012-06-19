#! /bin/sh -e

cd `dirname $0`/..
. release/config.sh
. release/tag.sh


#Clean-up 
/bin/rm -rf ${WORKDIR}/htmlgen ${WORKDIR}/${RELEASENAME}

#build source tar (with doc) 
( cd $WORKDIR && svn export ${SVNEXPORT} hevea && /bin/rm -rf hevea/examples hevea/release )

#Recompile (test)
( cd $WORKDIR/hevea && make opt )
#Recompile (produce doc)
( cd $WORKDIR/hevea/doc && make manual.ps manual.pdf opt docclean )
#Make final files with their final names
/bin/rm -rf ${WORKDIR}/final
mkdir -p  ${WORKDIR}/final
mv ${WORKDIR}/hevea/doc/manual.ps ${WORKDIR}/final/${RELEASENAME}-manual.ps
mv ${WORKDIR}/hevea/doc/manual.pdf ${WORKDIR}/final/${RELEASENAME}-manual.pdf
mv ${WORKDIR}/hevea/doc/doc ${WORKDIR}/final/${RELEASENAME}-manual
( cd  ${WORKDIR}/final &&
  gzip -f --best ${RELEASENAME}-manual.ps &&\
  tar cf  ${RELEASENAME}-manual.tar ${RELEASENAME}-manual &&\
  gzip -f --best ${RELEASENAME}-manual.tar &&
  /bin/rm -rf ${RELEASENAME}-manual )
( cd  ${WORKDIR}/hevea && make clean && /bin/rm -r doc )
mv  ${WORKDIR}/hevea  ${WORKDIR}/${RELEASENAME}
( cd  ${WORKDIR} && tar cf final/${RELEASENAME}.tar ${RELEASENAME} &&\
  gzip -f --best final/${RELEASENAME}.tar &&
  /bin/rm -rf ${RELEASENAME} )
#Now install files
TOINSTALL="${RELEASENAME}-manual.tar.gz ${RELEASENAME}-manual.ps.gz ${RELEASENAME}-manual.pdf  ${RELEASENAME}.tar.gz"
EXTRA="LICENSE README CHANGES hevea.sty"
#FTP
FTPDIR=/tmp/ftp.$$
mkdir -p $FTPDIR
if $DEV
then
  /bin/rm -rf $FTPDIR/unstable
  mkdir  $FTPDIR/unstable
  ( cd $WORKDIR/final && cp ${TOINSTALL} $FTPDIR/unstable )
  DFTP=$FTPDIR/unstable
else
  ( cd $WORKDIR/final && cp ${TOINSTALL} $FTPDIR )
  DFTP=$FTPDIR
fi
#complements
( cd $WORKDIR/final && tar zxf ${RELEASENAME}.tar.gz &&\
  cd ${RELEASENAME} && cp ${EXTRA} ${DFTP} )
#copy to httpd-dir
if $DEV
then
  /bin/rm -rf ${HTMLDIR}/distri/unstable
  cp -r $DFTP ${HTMLDIR}/distri
else
  /bin/rm -rf ${HTMLDIR}/distri
  mkdir ${HTMLDIR}/distri
  ( cd $DFTP &&\
    cp  ${TOINSTALL} ${EXTRA} ${HTMLDIR}/distri )
fi
#HTTP DOC
( cd $WORKDIR/final && tar zxf ${RELEASENAME}-manual.tar.gz )
if $DEV
then
  DHTML=${HTMLDIR}/newdoc
else
  DHTML=${HTMLDIR}/doc
fi
/bin/rm -rf $DHTML
mv $WORKDIR/final/${RELEASENAME}-manual $DHTML
/bin/rm -rf ${WORKDIR}/final
/bin/rm -rf ${FTPDIR}
