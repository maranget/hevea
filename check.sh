#! /bin/sh -e

. config.sh
TMP1=/tmp/check.1.$$
TMP2=/tmp/check.2.$$
TMP3=/tmp/check.3.$$

check () {
  DIR=$1
  shift
  ( cd $DIR ; ls *.hva > $TMP1 )
  echo > $TMP2
  for i in $*
  do
    echo $i >> $TMP2
  done
  sort $TMP1 > $TMP3 && mv $TMP3 $TMP1
  sort $TMP2 > $TMP3 && mv $TMP3 $TMP2
  diff $TMP1 $TMP2
  /bin/rm -f $TMP1 $TMP2 $TMP3
}

check . $ALLLIB
check html $HTMLLIB
check text $TEXTLIB
check info $INFOLIB

