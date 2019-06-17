#!/bin/sh

DIR0=`pwd`
REP2CLEAN="
CASES
UTIL/install
atlas
atlas/config
atlas/util
"

for rr in $REP2CLEAN
do
  cd $DIR0/$rr
  echo `pwd`
  rm -f *.pyc
done
