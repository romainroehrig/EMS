#!/bin/bash

set -e

#####################################################
# User specific defaults

# EMS Version
EMS_VERSION=2.4.3

# Directory where EMS is installed
REP_EMS=$HOME/Tools/EMS/V${EMS_VERSION}

# Directory where MUSC will be run
REP_MUSC=$HOME/MUSC/V${EMS_VERSION}

# Debug mode (0: unactivated, 1: activated)
DEBUG=0

# Testing. So fatr only on CNRM computer 
# and for ARPEGE-Climat 6.3.2
TESTS="n"

# Force to remove already installed directories
REMOVE="n"

#####################################################


#####################################################
# Usage
#
bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

usage() {

PROGNAME=`basename $0`

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - Installation script for EMS - Environment for MUSC Simulations

${bold}USAGE${normal}
        ${PROGNAME} [-i <install-directory>] [-r <musc-run-directory>]
        [-v <ems-version>] [ -d ] [ -t ] [ -f ] [ -h ]

${bold}DESCRIPTION${normal}
        Description of what the EMS install script does

${bold}OPTIONS${normal}
        -i ${unline}install-directory${normal}
           PATH to where you want to install EMS
           REP_EMS [$REP_EMS]

        -r ${unline}musc-run-directory${normal}
           PATH to where you want to run MUSC
           REP_MUSC [$REP_MUSC]

        -v ${unline}ems-version${normal}
           EMS_VERSION [$EMS_VERSION]
        
        -t Testing! Only on CNRM computer and for ARPEGE-Climat 6.3.2

        -d Debug! Add debug information with set -xv

        -f Force to remove already installed directories
           that is $REP_EMS and $REP_MUSC

        -h Help! Print usage information.

USAGE
}

#####################################################
# Some defaults

USAGE=0

#####################################################

while getopts i:r:v:dtfh option
do
  case $option in
    i)
       REP_EMS=$OPTARG
       ;;
    r)
       REP_MUSC=$OPTARG
       ;;
    v)
       EMS_VERSION=$OPTARG
       ;;
    d)
       DEBUG=1
       ;;
    t)
       TESTS="y"
       ;;
    f)
       REMOVE="y"
       ;;
    h)
       USAGE=1
       ;;
    *)
       USAGE=1
       ;;
  esac
done

if [ ${USAGE} -eq 1 ]; then
  usage
  exit 1
fi

if [ ${DEBUG} -eq 1 ]; then
  set -vx
fi

DIR0=`pwd`

#####################################################
# Some tests to avoid overwriting

if [ -d "$REP_EMS" ]; then
  echo "REP_EMS="$REP_EMS
  echo "REP_EMS already exists. Please remove it or modify REP_EMS at the top of install.sh"
  echo "You can force removal of REP_EMS using the -f argument"
  echo "!!! It may remove REP_MUSC if it exists !!!"
  if [ "$REMOVE" == "y" ]; then
    /bin/rm -r $REP_EMS
  else
    exit
  fi
fi

if [ -d "$REP_MUSC" ]; then
  echo "REP_MUSC="$REP_MUSC
  echo "REP_MUSC already exists. Please remove it or modify REP_MUSC at the top of install.sh"
  echo "You can force removal of REP_MUSC using the -f argument"
  if [ "$REMOVE" == "y" ]; then
    /bin/rm -r $REP_MUSC
  else
    exit
  fi
fi

#####################################################
# Download and install EMS in REP_EMS
if [ "${EMS_VERSION}" == 'git' ]; then
  git clone https://github.com/romainroehrig/EMS $REP_EMS
else
  [ -d $REP_EMS ] || mkdir -p $REP_EMS
  cd $REP_EMS
  
  wget https://github.com/romainroehrig/EMS/archive/V${EMS_VERSION}.tar.gz
  tar zxvf V${EMS_VERSION}.tar.gz
  rm -f V${EMS_VERSION}.tar.gz
  mv EMS-${EMS_VERSION}/* .
  rm -rf EMS-${EMS_VERSION}
fi

# Some compilation if you want
compile="y"

if [ $compile = "y" ]; then

  # lfa python library
  cd $REP_EMS/aux/lfa4py
  make

  # ascii2lfa binary
  #The ASCII2FA_LIBS environment variable can be set (a default value is used if not set)
  #If set, the variable should contain paths to all the neede lib. Below is an example to link
  #ascii2fa with libraries from cycle 46t1op1
  #ASCII2FA_LIBS="/home/common/opt/pack/shared/46t1_op1.01.MPIGFORTRAN920DBL.xfftw/lib/libifsaux.local.a \
  #               /home/common/sync/gfortran/auxlibs-gcc-9.2.0.so/lib/libmpidummy.a \
  #               /home/common/sync/gfortran/auxlibs-gcc-9.2.0.so/lib/libgribex.a \
  #               /home/common/sync/gfortran/eccodes-2.18.0_gcc-9.2.0/lib/libeccodes.a \
  #               /home/common/sync/gfortran/eccodes-2.18.0_gcc-9.2.0/lib/libeccodes_f90.a \
  #               /home/common/opt/pack/shared/46t1_op1.01.MPIGFORTRAN920DBL.xfftw/lib/libalgor.local.a"
  #export ASCII2FA_LIBS
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
testing="$TESTS"

test_arp632="y"

if [ $testing = "y" ]; then

  # Testing ARPEGE-Climat 6.3.2
  if [ $test_arp632 = "y" ]; then
    cd $REP_MUSC
    source setenv

    MPLBACKEND=Agg ./MUSC.py -config config/config_arp632_CMIP6.py -case ARMCU -subcase REF
    [ -f $REP_MUSC/ATM/V632/arp632_CMIP6/ARMCU/REF/initfile_L91 ] || echo "PROBLEM when preparing atmospheric files"
    [ -f $REP_MUSC/SURFEX/V632/arp632_CMIP6/ARMCU/REF/PGD.lfi ] || echo "PROBLEM when prepararing PGD"
    [ -f $REP_MUSC/SURFEX/V632/arp632_CMIP6/ARMCU/REF/PREP.lfi ] || echo "PROBLEM when preparing PREP"
    [ -f $REP_MUSC/simulations/V632/arp632_CMIP6/ARMCU/REF/Output/netcdf/Out_klevel.nc ] || echo "PROBLEM when running MUSC"

  fi

fi

#####################################################
# Back in directory where installation was launched
cd $DIR0
