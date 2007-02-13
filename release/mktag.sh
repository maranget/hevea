#! /bin/sh -

cd `dirname $0`/..

VERSIONFILE=version.ml
case $1 in
  zyva)
    VERSION=`sed -n -e 's/^let real_version = "\(.*\)".*$/\1/p' ${VERSIONFILE}`
  ;;
  *)
  VERSION=test
  ;;
esac
DATE=`date +%Y-%m-%d`
echo DATE=$DATE
case $VERSION in
  *+*)
     echo DEV=true
     echo RELEASENAME=hevea-\${DATE}
     ;;  
    *)
    echo DEV=false 
    echo RELEASENAME=hevea-\${VERSION}
   ;;
esac
echo VERSION=${VERSION}

case $VERSION in
  test)
    echo CVSEXPORT=\"-D now\"
    ;;
  *)
    TMP=/tmp/tag.$$
    RELEASETAG=`sed -n -e 's/^let real_version = "\(.\)\.\(.*\)".*$/\1-\2/p' ${VERSIONFILE}`
    echo RELEASETAG=$RELEASETAG
    echo CVSEXPORT=\"-r release-\${RELEASETAG}\"
    sed  -e "s/^let release_date = .*/let release_date = \"$DATE\"/" ${VERSIONFILE} > $TMP && mv $TMP $VERSIONFILE
    ( cvs commit -m tag && cvs tag -F  release-${RELEASETAG} ) >/dev/null
    ;;
esac

