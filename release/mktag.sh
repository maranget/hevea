#! /bin/sh -
VERSIONFILE=../version.ml
case $1 in
  zyva)
    VERSION=`sed -n -e 's/^let real_version = "\(.*\)".*$/\1/p' ${VERSIONFILE}`
  ;;
  *)
  VERSION=test
  ;;
esac
echo CVSDIR=${HOME}/CVS
echo VERSION=${VERSION}
echo RELEASENAME=hevea-\${VERSION}

case $VERSION in
  test)
    echo CVSEXPORT=\"-D now\"
    ;;
  *)
    TMP=/tmp/tag.$$
    RELEASETAG=`sed -n -e 's/^let real_version = "\(.\)\.\(.*\)".*$/\1-\2/p' ${VERSIONFILE}`
    echo RELEASETAG=$RELEASETAG
    echo CVSEXPORT=\"-r \${RELEASETAG}\"
    sed  -e "s/^let release_date = .*/let release_date = \"`date +%Y-%m-%d`\"/" ${VERSIONFILE} > $TMP && mv $TMP $VERSIONFILE
    ( cd .. ; cvs commit -m tag )
    ( cd .. ; cvs tag -F $RELEASETAG )
    ;;
esac

