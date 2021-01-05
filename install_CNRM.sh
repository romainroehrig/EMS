#!/bin/sh

set -evx

#####################################################
# User specific

# Directory where EMS is installed
REP_EMS=$HOME/Tools/EMS/V1

# Directory where MUSC will be run
REP_MUSC=$HOME/MUSC/V1

# Environment file to use
PROFILE=.bash_profile

#####################################################

DIR0=`pwd`

#####################################################
# Some tests to avoid overwriting

if [ -d $REP_EMS ]; then
  echo "REP_EMS="$REP_EMS
  echo "REP_EMS already exists. Please remove it or modify REP_EMS at the top of install_CNRM.sh"
  exit
fi

if [ -d $REP_MUSC ]; then
  echo "REP_MUSC="$REP_MUSC
  echo "REP_MUSC already exists. Please remove it or modify REP_MUSC at the top of install_CNRM.sh"
  exit
fi

#####################################################
# Download and install EMS in REP_EMS
[ -d $REP_EMS ] || mkdir -p $REP_EMS
cd $REP_EMS
git clone --depth 1 https://github.com/romainroehrig/EMS.git --branch master --single-branch .

# Modify your .bash_profile to initialize a few environment variables
cd ~/

# save bash_profile
cat $PROFILE > $PROFILE.EMS-saved_$(date +"%Y-%m-%d_at_%H-%M-%S")

# Modify it
sed -i "/^export REP_EMS=/ s/$/ #commented on $(date)/" $PROFILE
sed -i "s/^export REP_EMS=/#&/" $PROFILE
sed -i "/^export REP_MUSC=/ s/$/ #commented on $(date)/" $PROFILE
sed -i "s/^export REP_MUSC=/#&/" $PROFILE
sed -i "/^export PYTHONPATH=.:\$REP_EMS/ s/$/ #commented on $(date)/" $PROFILE
sed -i "s/^export PYTHONPATH=.:\$REP_EMS/#&/" $PROFILE

cat << EOF >> $PROFILE

# Modifications for Environment for MUSC simulations (EMS)
# included on $(date)
export REP_EMS=$REP_EMS
export REP_MUSC=$REP_MUSC
export PYTHONPATH=.:\$REP_EMS/CASES:\$REP_EMS/UTIL/python:\$REP_EMS/UTIL/install/:\$PYTHONPATH
EOF

. ~/$PROFILE

#####################################################
# Prepare what is needed to run MUSC simulations in REP_MUSC
[ -d $REP_MUSC ] || mkdir -p $REP_MUSC
cd $REP_MUSC
cp -r $REP_EMS/Examples/* .
ln -s $REP_EMS/main/MUSC.py

for ff in convertLFA2nc.py lfa2nc.py convert2p.py convert2z.py 
do

  ln -s $REP_EMS/UTIL/post/$ff $REP_MUSC/post/$ff

done

#####################################################
# Some Testing
testing="n"

install_arp631="n"
install_cy41="n"

test_arp631="n"

if [ $testing == "y" ]; then

  # ARPEGE-Climat 6.3.1
  if [ $install_arp631 == 'y' ]; then

    [ -d /home/common/pack ] || mkdir -p /home/common/pack
    cd /home/common/pack

    if [ -d arp603_export.01.GFORTRAN610.cx ]; then
      echo "pack arp603_export.01.GFORTRAN610.cx already installed in /home/common/pack"
    else
      echo "pack arp603_export.01.GFORTRAN610.cx is installed in /home/common/pack"
      cp /cnrm/mosca/DATA/rootpack/arp603_export.01.GFORTRAN610.cx-29032018.tgz .
      tar zxvf arp603_export.01.GFORTRAN610.cx-29032018.tgz
      rm -f arp603_export.01.GFORTRAN610.cx-29032018.tgz
    fi

  fi

  # ARPEGE Cycle 41t1_op1.11_MUSC
  if [ $install_cy41 == 'y' ]; then

    [ -d $HOME/pack ] || mkdir $HOME/pack
    cd $HOME/pack

    if [ -d 41t1_op1.11_MUSC ]; then
      echo "pack 41t1_op1.11_MUSC already installed in /home/common/pack"
    else
      echo "pack 41t1_op1.11_MUSC is installed in $HOME/pack"
      cp /cnrm/amacs/USERS/roehrig/share/MUSC/pack/41t1_op1.11_MUSC.tar.gz
      tar zxvf 41t1_op1.11_MUSC.tar.gz
      rm -f 41t1_op1.11_MUSC.tar.gz
    fi

  fi

  # Testing ARPEGE-Climat 6.3.1
  if [ $test_arp631 == 'y']; then
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
