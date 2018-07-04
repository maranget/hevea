#! /bin/sh -e

cd `dirname $0`/..
. release/config.sh
. release/tag.sh


#Clean-up 
/bin/rm -rf ${WORKDIR}/htmlgen ${WORKDIR}/${RELEASENAME}
HEVEA=${RELEASENAME}
TAR=$(basename $SRC)
#build source tar (with doc) 
( cd $WORKDIR && rm -rf ${TAR} ${HEVEA} && wget $SRC && tar zxmf $TAR && rm -rf bugs release )
#Recompile (test)
( cd $WORKDIR/${HEVEA} && make opt )
#Recompile (produce doc)
( cd $WORKDIR/${HEVEA}/doc && make manual.ps manual.pdf opt docclean )
#Make final files with their final names
/bin/rm -rf ${WORKDIR}/final
mkdir -p  ${WORKDIR}/final
mv ${WORKDIR}/${HEVEA}/doc/manual.ps ${WORKDIR}/final/${RELEASENAME}-manual.ps
mv ${WORKDIR}/${HEVEA}/doc/manual.pdf ${WORKDIR}/final/${RELEASENAME}-manual.pdf
mv ${WORKDIR}/${HEVEA}/doc/doc ${WORKDIR}/final/${RELEASENAME}-manual
( cd  ${WORKDIR}/final &&
  gzip -f --best ${RELEASENAME}-manual.ps &&\
  tar cf  ${RELEASENAME}-manual.tar ${RELEASENAME}-manual &&\
  gzip -f --best ${RELEASENAME}-manual.tar &&
  /bin/rm -rf ${RELEASENAME}-manual )
( cd  ${WORKDIR}/${HEVEA} && make clean && /bin/rm -r doc )
( cd  ${WORKDIR} && tar cf final/${RELEASENAME}.tar ${RELEASENAME} &&\
  gzip -f --best final/${RELEASENAME}.tar &&
  /bin/rm -rf ${RELEASENAME} )
#Now install files
TOINSTALL="${RELEASENAME}-manual.tar.gz ${RELEASENAME}-manual.ps.gz ${RELEASENAME}-manual.pdf ${RELEASENAME}.tar.gz"
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
  cp ${HTMLDIR}/distri/${RELEASENAME}.tar.gz ${HTMLDIR}/old
fi
#HTTP DOC
( cd $WORKDIR/final && tar zxf ${RELEASENAME}-manual.tar.gz )
if $DEV
then
  DHTML=${HTMLDIR}/distri/unstable/doc
else
  DHTML=${HTMLDIR}/doc
fi
/bin/rm -rf $DHTML
mv $WORKDIR/final/${RELEASENAME}-manual $DHTML
/bin/rm -rf ${WORKDIR}/final
/bin/rm -rf ${FTPDIR}
