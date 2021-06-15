#!/bin/sh

set -evx

#####################################################
# User specific

# EMS Version
EMS_VERSION=TO BE DEFINED

# Directory where EMS is installed
REP_EMS=$HOME/Tools/EMS

# Directory where MUSC will be run
REP_MUSC=$HOME/MUSC

# Environment file to use
PROFILE=.bash_profile

#####################################################

DIR0=`pwd`

#####################################################
# Some tests to avoid overwriting

if [ -d "$REP_EMS/$EMS_VERSION" ]; then
  echo "REP_EMS="$REP_EMS/$EMS_VERSION
  echo "REP_EMS already exists. Please remove it or modify REP_EMS at the top of install_macRR.sh"
  exit
fi

if [ -d "$REP_MUSC/$EMS_VERSION" ]; then
  echo "REP_MUSC="$REP_MUSC/$EMS_VERSION
  echo "REP_MUSC already exists. Please remove it or modify REP_MUSC at the top of install_macRR.sh"
  exit
fi

#####################################################
# Download and install EMS in REP_EMS
[ -d $REP_EMS ] || mkdir -p $REP_EMS
cd $REP_EMS
#git clone --depth 1 https://github.com/romainroehrig/EMS.git --branch macRR_dephy2 --single-branch .

wget https://github.com/romainroehrig/EMS/archive/v${EMS_VERSION}.tar.gz
tar zxvf v${EMS_VERSION}.tar.gz
rm -f v${EMS_VERSION}.tar.gz
mv EMS-${EMS_VERSION} V${EMS_VERSION}

# Modify your .bash_profile to initialize a few environment variables
cd ~/

# save bash_profile
cat $PROFILE > $PROFILE.EMS-saved_$(date +"%Y-%m-%d_at_%H-%M-%S")

# Modify it
sed -i '' "/^export REP_EMS=/ s/$/ #commented on $(date)/" $PROFILE
sed -i '' "s/^export REP_EMS=/#&/" $PROFILE
sed -i '' "/^export REP_MUSC=/ s/$/ #commented on $(date)/" $PROFILE
sed -i '' "s/^export REP_MUSC=/#&/" $PROFILE
sed -i '' "/^export PYTHONPATH=.:\$REP_EMS/ s/$/ #commented on $(date)/" $PROFILE
sed -i '' "s/^export PYTHONPATH=.:\$REP_EMS/#&/" $PROFILE

cat << EOF >> $PROFILE

# Modifications for Environment for MUSC simulations (EMS)
# included on $(date)
export REP_EMS=$REP_EMS/V$EMS_VERSION
export REP_MUSC=$REP_MUSC/V$EMS_VERSION
export PYTHONPATH=.:\$REP_EMS:\$REP_EMS/aux:\$PYTHONPATH
EOF

. ~/$PROFILE

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

  # LFA tools
  #cd $REP_EMS/aux/lfatools
  #./install

fi

#####################################################
# Prepare what is needed to run MUSC simulations in REP_MUSC
[ -d "$REP_MUSC" ] || mkdir -p $REP_MUSC
cd $REP_MUSC
cp -r $REP_EMS/examples/* .
ln -s $REP_EMS/ems/apptools/MUSC.py

for ff in convertLFA2nc.py lfa2nc.py convert2p.py convert2z.py 
do

  ln -s $REP_EMS/ems/apptools/ems_$ff $REP_MUSC/post/$ff

done

#####################################################
# Some Testing
testing="y"

test_arp631="y"

if [ $testing == "y" ]; then

  # Testing ARPEGE-Climat 6.3.1
  if [ $test_arp631 == 'y' ]; then
    cd $REP_MUSC

    ./MUSC.py -config config/config_arp631_CMIP6.py -case ARMCU -subcase REF
    [ -f $REP_MUSC/ATM/ARPCLIMAT/ARMCU/REF/initfile_L91 ] || echo "PROBLEM with install_ATM_cases.py"

    [ -f $REP_MUSC/SURFEX/V631/arp631_CMIP6/ARMCU/REF/PGD.lfi ] || echo "PROBLEM with install_SFX_cases.py: PGD"
    [ -f $REP_MUSC/SURFEX/V631/arp631_CMIP6/ARMCU/REF/PREP.lfi ] || echo "PROBLEM with install_SFX_cases.py: PREP"

    [ -f $REP_MUSC/simulations/V631/arp631_CMIP6/ARMCU/REF/Output/netcdf/Out_klevel.nc ] || echo "PROBLEM with run_MUSC_cases.py"

  fi

fi

#####################################################
# Back in directory where installation was launched
cd $DIR0
