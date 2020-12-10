#!/bin/sh

#ecoclimapdir=ecoclimap
ecoclimapdir=ecoclimap_cnrm_cm6.02
#makelinks="no"
makelinks="yes"

PGD=./PGD
PREPSURF=./PREP


if [ $makelinks = "yes" ] ; then
  rm -f ecoclimapII_eu_covers_param.bin
  ln -s $ecoclimapdir/ecoclimapII_eu_covers_param.bin ecoclimapII_eu_covers_param.bin
  rm -f ecoclimapI_covers_param.bin
  ln -s $ecoclimapdir/ecoclimapI_covers_param.bin ecoclimapI_covers_param.bin
fi

#######################
# PGD

cp namsurf OPTIONS.nam

$PGD > PGD.log 2>&1

cat LISTING_PGD.txt >> PGD.log

rm -f fort.10 OPTIONS.nam PRE_PGD1.nam LISTING_PGD.txt PGD.des class_cover_data.tex

########################
# PREP

cp namsurf OPTIONS.nam

$PREPSURF > PREP.log 2>&1

cat LISTING_PREP.txt >> PREP.log

rm -f LISTING_PREP.txt OPTIONS.nam PGD.des PREP.des

