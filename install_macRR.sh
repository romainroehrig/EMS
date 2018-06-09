#!/bin/sh

REP_EMS=$HOME/Tools/EMS
REP_MUSC=$HOME/MUSC

#####################################################
# Some Test to avoid overwriting

if [ -d $REP_EMS ]; then
  echo "REP_EMS="$REP_EMS
  echo "REP_EMS already exists. Please remove it or modify REP_EMS at the top of install.sh"
  exit
fi

if [ -d $REP_MUSC ]; then
  echo "REP_MUSC="$REP_MUSC
  echo "REP_MUSC already exists. Please remove it or modify REP_MUSC at the top of install.sh"
  exit
fi

#####################################################
# Download and install EMS in REP_EMS
[ -d $REP_EMS ] || mkdir $REP_EMS
cd $REP_EMS
git clone https://github.com/romainroehrig/EMS.git .

# Modify your .bash_profile to initialize a few environment variables
cd ~/

# save profile
cat .profile > .profile.EMS-saved_$(date +"%Y-%m-%d_at_%H:%M:%S")

# Modify it
sed -i '' "/^export REP_EMS=/ s/$/ #commented on $(date)/" .profile
sed -i '' "s/^export REP_EMS=/#&/" .profile
sed -i '' "/^export REP_MUSC=/ s/$/ #commented on $(date)/" .profile
sed -i '' "s/^export REP_MUSC=/#&/" .profile
sed -i '' "/^export PYTHONPATH=.:\$REP_EMS/ s/$/ #commented on $(date)/" .profile
sed -i '' "s/^export PYTHONPATH=.:\$REP_EMS/#&/" .profile

cat << EOF >> .profile

# Modifications for Environment for MUSC simulations (EMS)
# included on $(date)
export REP_EMS=$REP_EMS
export REP_MUSC=$REP_MUSC
export PYTHONPATH=.:\$REP_EMS/CASES:\$REP_EMS/UTIL/python:\$REP_EMS/UTIL/install/:\$PYTHONPATH
EOF

. ~/.profile

# Some compilation

# lfa python library
cd $REP_EMS/UTIL/python/lfa
./makelib.sh

# ascii2lfa binary
cd $REP_EMS/UTIL/Tools/ASCII2FA/src
cp /Users/romainroehrig/rootpack/arp603_export.01.MPIGNU640.x/lib/libxrd.local.a libxrd.a
cp /Users/romainroehrig/rootpack/arp603_export.01.MPIGNU640.x/lib/libxla.local.a libxla.a
cp /Users/romainroehrig/libraries/auxlibs/GNU/auxlibs/lib/libgribex.a libgribex.a
cp /Users/romainroehrig/libraries/auxlibs/GNU/auxlibs/lib/libmpidummy.a libmpidummy.a
make all
#make clean

# LFA tools
cd $REP_EMS//UTIL/Tools/LFA
./install

# a few python libraries
source activate myuvcdat

cd $REP_EMS/UTIL/Init_Forc/ARPCLIMAT
f2py -c interpvertp.F90 -m interpvertp
cd $REP_EMS/UTIL/post_DEPHY
f2py -c convert2p.F90 -m convert2p
f2py -c convert2z.F90 -m convert2z

source deactivaet myuvcdat

#####################################################
# Prepare what is needed to run MUSC simulations in REP_MUSC
[ -d $REP_MUSC ] || mkdir $REP_MUSC
cd $REP_MUSC
cp -r $REP_EMS/Examples/* .
ln -s $REP_EMS/main/install_ATM_cases.py install_ATM_cases.py
ln -s $REP_EMS/main/install_SFX_cases.py install_SFX_cases.py
ln -s $REP_EMS/main/run_MUSC_cases.py run_MUSC_cases.py

#####################################################
# Some Testing
cd $REP_MUSC

source activate myuvcdat

install_ATM_cases.py
[ -f $REP_MUSC/ATM/ARPCLIMAT/AYOTTE/A24SC/initfile_L91 ] || echo "PROBLEM with install_ATM_cases.py"

install_SFX_cases.py config/config_arp631_macRR_CMIP6.py
[ -f $REP_MUSC/SURFEX/arp631_macRR/CMIP6/AYOTTE/A24SC/PGD.lfi ] || echo "PROBLEM with install_SFX_cases.py: PGD"
[ -f $REP_MUSC/SURFEX/arp631_macRR/CMIP6/AYOTTE/A24SC/PREP.lfi ] || echo "PROBLEM with install_SFX_cases.py: PREP"

run_MUSC_cases.py config/config_arp631_macRR_CMIP6.py
[ -f $REP_MUSC/simulations/arp631_macRR/CMIP6/L91_300s/AYOTTE/A24SC/Output/netcdf/Out_klevel.nc ] || echo "PROBLEM with run_MUSC_cases.py"

