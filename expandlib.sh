#! /bin/sh
. `dirname $0`/config.sh
sed -e "s,LIBDIR,$LIBDIR,g" $1
