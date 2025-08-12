#! /bin/sh
. `dirname $0`/config.sh
LIBDIR=$(echo "$LIBDIR" | sed -e 's|\\|/|g')
sed -e "s,LIBDIR,$LIBDIR,g" $1
