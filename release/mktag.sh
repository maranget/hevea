#! /bin/sh -

cd `dirname $0`/..
REPOS=svn+ssh://yquem.inria.fr/home/yquem/moscova/maranget/repos
VERSIONFILE=version.ml
VERSION=`sed -n -e 's/^let real_version = "\(.*\)".*$/\1/p' ${VERSIONFILE}`
DATE=`date +%Y-%m-%d`
echo DATE=$DATE
echo VERSION=${VERSION}
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


TMP=/tmp/tag.$$
RELEASETAG=`sed -n -e 's/^let real_version = "\(.\)\.\(.*\)".*$/\1-\2/p' ${VERSIONFILE}`
echo RELEASETAG=$RELEASETAG
echo SVNEXPORT=${REPOS}/hevea-release/hevea-${RELEASETAG}
sed  -e "s/^let release_date = .*/let release_date = \"$DATE\"/" ${VERSIONFILE} > $TMP && mv $TMP $VERSIONFILE
( svn delete ${REPOS}/hevea-release/hevea-${RELEASETAG} -m 'deleting stale release copy' || true )  >/dev/null
( svn commit -m tag && svn copy ${REPOS}/hevea ${REPOS}/hevea-release/hevea-${RELEASETAG} -m "Tagging hevea-${RELEASETAG}" ) >/dev/null

