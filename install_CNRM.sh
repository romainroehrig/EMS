#!/bin/sh

set -evx

#####################################################
# User specific

REP_EMS=$HOME/Tools/EMS_test
REP_MUSC=$HOME/MUSC_test

#####################################################





#####################################################
# Some Test to avoid overwriting

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
[ -d $REP_EMS ] || mkdir $REP_EMS
cd $REP_EMS
git clone --single-branch https://github.com/romainroehrig/EMS.git .

# Modify your .bash_profile to initialize a few environment variables
cd ~/

# save bash_profile
cat .bash_profile > .bash_profile.EMS-saved_$(date +"%Y-%m-%d_at_%H:%M:%S")

# Modify it
sed -i "/^export REP_EMS=/ s/$/ #commented on $(date)/" .bash_profile
sed -i "s/^export REP_EMS=/#&/" .bash_profile
sed -i "/^export REP_MUSC=/ s/$/ #commented on $(date)/" .bash_profile
sed -i "s/^export REP_MUSC=/#&/" .bash_profile
sed -i "/^export PYTHONPATH=.:\$REP_EMS/ s/$/ #commented on $(date)/" .bash_profile
sed -i "s/^export PYTHONPATH=.:\$REP_EMS/#&/" .bash_profile

cat << EOF >> .bash_profile

# Modifications for Environment for MUSC simulations (EMS)
# included on $(date)
export REP_EMS=$REP_EMS
export REP_MUSC=$REP_MUSC
export PYTHONPATH=.:\$REP_EMS/CASES:\$REP_EMS/UTIL/python:\$REP_EMS/UTIL/install/:\$PYTHONPATH
EOF

. ~/.bash_profile

#####################################################
# Prepare what is needed to run MUSC simulations in REP_MUSC
[ -d $REP_MUSC ] || mkdir $REP_MUSC
cd $REP_MUSC
cp -r $REP_EMS/Examples/* .
ln -s $REP_EMS/main/install_ATM_cases.py install_ATM_cases.py
ln -s $REP_EMS/main/install_SFX_cases.py install_SFX_cases.py
ln -s $REP_EMS/main/run_MUSC_cases.py run_MUSC_cases.py

for ff in convertLFA2nc.py convertp_to_1hourly.py convertz_to_1hourly.py lfa2nc_part2.py convert2p.py convert2z.py convertk_to_1hourly.py convertp_to_3hourly.py convertz_to_daily.py convert2p.so convert2z.so convertk_to_daily.py convertp_to_daily.py lfa2nc_part1.py
do

  ln -s $REP_EMS/UTIL/post_DEPHY/$ff $REP_MUSC/post/$ff

done

#####################################################
# Some Testing

# get arp631 pack

cd /home/common/pack
if [ -d arp603_export.01.GFORTRAN610.cx ]; then
  echo "pack arp603_export.01.GFORTRAN610.cx already installed in /home/common/pack"
else
  echo "pack arp603_export.01.GFORTRAN610.cx is installed in /home/common/pack"
  cp /cnrm/mosca/DATA/rootpack/arp603_export.01.GFORTRAN610.cx-29032018.tgz .
  tar zxvf arp603_export.01.GFORTRAN610.cx-29032018.tgz
  rm -f arp603_export.01.GFORTRAN610.cx-29032018.tgz
fi

# get 41t1_op1.11_MUSC

install_cy41='n'

if [$install_cy41 == 'y' ]; then

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
# Testing for arp631

cd $REP_MUSC

install_ATM_cases.py AYOTTE 24SC
[ -f $REP_MUSC/ATM/ARPCLIMAT/AYOTTE/24SC/initfile_L91 ] || echo "PROBLEM with install_ATM_cases.py"

install_SFX_cases.py config/config_arp631_CMIP6.py AYOTTE 24SC
[ -f $REP_MUSC/SURFEX/arp631/CMIP6/AYOTTE/24SC/PGD.lfi ] || echo "PROBLEM with install_SFX_cases.py: PGD"
[ -f $REP_MUSC/SURFEX/arp631/CMIP6/AYOTTE/24SC/PREP.lfi ] || echo "PROBLEM with install_SFX_cases.py: PREP"

run_MUSC_cases.py config/config_arp631_CMIP6.py AYOTTE 24SC
[ -f $REP_MUSC/simulations/arp631/CMIP6/L91_300s/AYOTTE/24SC/Output/netcdf/Out_klevel.nc ] || echo "PROBLEM with run_MUSC_cases.py"

