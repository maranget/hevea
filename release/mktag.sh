#! /bin/sh -

cd `dirname $0`/..
REPOS=https://github.com/maranget/hevea/archive
VERSIONFILE=version.ml
VERSION=`sed -n -e 's/^let real_version = "\(.*\)".*$/\1/p' ${VERSIONFILE}`
DATE=`date +%Y-%m-%d`
echo DATE=$DATE
echo VERSION=${VERSION}
case $VERSION in
  *+*)
     echo DEV=true
     echo RELEASENAME=hevea-${DATE}
     ;;  
    *)
    echo DEV=false 
    echo RELEASENAME=hevea-${VERSION}
   ;;
esac


TMP=/tmp/tag.$$
RELEASETAG=`sed -n -e 's/^let real_version = "\(.\)\.\(.*\)".*$/\1.\2/p' ${VERSIONFILE}`
echo SRC=${REPOS}/v${VERSION}.tar.gz
