#!/bin/sh

# seems necessary in some circumstances (deep shells?)
unset PYTHONHOME

convert2p=no
convert2z=no

convertkday=no
convertpday=no
convertzday=no

convertk1h=no
convertp1h=no
convertz1h=no

convertk3h=no # not coded for yes
convertp3h=no #yes
convertz3h=no # not coded for yes

if [ ! -d netcdf ] ; then
  mkdir netcdf
else
  rm netcdf/*.nc
fi

/usr/bin/python lfa2nc_part1.py
cdat lfa2nc_part2.py

mv Out_klevel.nc netcdf/

if [ $convertk1h ==  "yes" ] ; then
  cdat convertk_to_1hourly.py
fi

if [ $convertk3h ==  "yes" ] ; then
  cdat convertk_to_3hourly.py
fi

if [ $convertkday ==  "yes" ] ; then
  cdat convertk_to_daily.py
fi


if [ $convert2p == "yes" ] ; then
  cdat convert2p.py
  if [ $convertp1h ==  "yes" ] ; then
    cdat convertp_to_1hourly.py
  fi
  if [ $convertp3h ==  "yes" ] ; then
    cdat convertp_to_3hourly.py
  fi
  if [ $convertpday ==  "yes" ] ; then
    cdat convertp_to_daily.py
  fi
fi

if [ $convert2z == "yes" ] ; then
  cdat convert2z.py
  if [ $convertz1h ==  "yes" ] ; then
    cdat convertz_to_1hourly.py
  fi
  if [ $convertz3h ==  "yes" ] ; then
    cdat convertz_to_3hourly.py
  fi
  if [ $convertzday ==  "yes" ] ; then
    cdat convertz_to_daily.py
  fi

fi

