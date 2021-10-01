#!/bin/sh

set -evx

#####################################################
# User specific

# EMS Version
EMS_VERSION=2.2

# Directory where EMS is installed
REP_EMS=$HOME/Tools/EMS/V${EMS_VERSION}

# Directory where MUSC will be run
REP_MUSC=$HOME/MUSC/V${EMS_VERSION}

#####################################################

DIR0=`pwd`

#####################################################
# Some tests to avoid overwriting

if [ -d "$REP_EMS" ]; then
  echo "REP_EMS="$REP_EMS
  echo "REP_EMS already exists. Please remove it or modify REP_EMS at the top of install_ems.sh"
  exit
fi

if [ -d "$REP_MUSC" ]; then
  echo "REP_MUSC="$REP_MUSC
  echo "REP_MUSC already exists. Please remove it or modify REP_MUSC at the top of install_ems.sh"
  exit
fi

#####################################################
# Download and install EMS in REP_EMS
[ -d $REP_EMS ] || mkdir -p $REP_EMS
cd $REP_EMS

wget https://github.com/romainroehrig/EMS/archive/V${EMS_VERSION}.tar.gz
tar zxvf V${EMS_VERSION}.tar.gz
rm -f V${EMS_VERSION}.tar.gz
mv EMS-${EMS_VERSION}/* .
rm -rf EMS-${EMS_VERSION}

# Some compilation if you want
compile="y"

if [ $compile == "y" ]; then

  # lfa python library
  cd $REP_EMS/aux/lfa4py
  make

  # ascii2lfa binary
  cd $REP_EMS/aux/ASCII2FA/src
  make all
  make clean

fi

#####################################################
# Prepare what is needed to run MUSC simulations in REP_MUSC
[ -d "$REP_MUSC" ] || mkdir -p $REP_MUSC
cd $REP_MUSC
cp -r $REP_EMS/examples/* .
ln -s $REP_EMS/apptools/MUSC.py

for ff in convertLFA2nc.py lfa2nc.py convert2p.py convert2z.py 
do

  ln -s $REP_EMS/apptools/ems_$ff $REP_MUSC/post/$ff

done

tmp=$(printf '%s' "$REP_EMS" | sed -e 's/[\/&]/\\&/g')
sed -i.bak "s/__REP_EMS__/"$tmp"/" setenv

cd config
for ff in `ls config_*.py`
do
  tmp=$(printf '%s' "$REP_MUSC" | sed -e 's/[\/&]/\\&/g')
  sed -i.bak "s/__REP_MUSC__/"$tmp"/" $ff

done
cd ..

#####################################################
# Some Testing
testing="y"

test_arp631="y"

if [ $testing == "y" ]; then

  # Testing ARPEGE-Climat 6.3.1
  if [ $test_arp631 == 'y' ]; then
    cd $REP_MUSC
    source setenv

    MPLBACKEND=Agg ./MUSC.py -config config/config_arp631_CMIP6.py -case ARMCU -subcase REF
    [ -f $REP_MUSC/ATM/V631/arp631_CMIP6/ARMCU/REF/initfile_L91 ] || echo "PROBLEM when preparing atmospheric files"
    [ -f $REP_MUSC/SURFEX/V631/arp631_CMIP6/ARMCU/REF/PGD.lfi ] || echo "PROBLEM when prepararing PGD"
    [ -f $REP_MUSC/SURFEX/V631/arp631_CMIP6/ARMCU/REF/PREP.lfi ] || echo "PROBLEM when preparing PREP"
    [ -f $REP_MUSC/simulations/V631/arp631_CMIP6/ARMCU/REF/Output/netcdf/Out_klevel.nc ] || echo "PROBLEM when running MUSC"

  fi

fi

#####################################################
# Back in directory where installation was launched
cd $DIR0
