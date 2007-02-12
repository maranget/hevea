#! /bin/sh -

cd `dirname $0`
VERSIONFILE=version.ml
case $1 in
  zyva)
    VERSION=`sed -n -e 's/^let real_version = "\(.*\)".*$/\1/p' ${VERSIONFILE}`
  ;;
  *)
  VERSION=test
  ;;
esac
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
    echo CVSEXPORT=\"-r release-\${RELEASETAG}\"
    sed  -e "s/^let release_date = .*/let release_date = \"`date +%Y-%m-%d`\"/" ${VERSIONFILE} > $TMP && mv $TMP $VERSIONFILE
    ( cvs commit -m tag && cvs tag -F  release-${RELEASETAG} ) >/dev/null
    ;;
esac

