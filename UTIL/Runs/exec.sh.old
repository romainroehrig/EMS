#!/bin/sh

date

#------------------------------------------------------------
# 			INTIALISATION
#------------------------------------------------------------

DIR0=`pwd`

# Physique pronostique PRE62T127Cr2En0 - old
CONFIG=PRE62T127Cr2En0
levels=91
NAMELIST_ARPE=namarp_PRE62T127Cr2En0
NAMELIST_SURF=namsurf_PRE6.2T127L91Cr2
configpost=config_CINDY.py

TSTEP=900
NSTOP=d10


cycle=arp602_optim.02.GFORTRAN482.cx.RR

installpost=True
#installpost=False
runpost=True
#runpost=False

dirpost=/home/roehrig/MUSC/UTIL/post

INITFILE=$DIR0/../Init_Forc/ARPEGE/initfile_L${levels}

FORCING_FILES_DIR=$DIR0/../Init_Forc/ARPEGE/files_L${levels}_${TSTEP}s/

AROME_PREPSURF=$DIR0/../Init_Forc/SURFEX/PREP.lfi
AROME_PGD=$DIR0/../Init_Forc/SURFEX/PGD.lfi


echo 'CONFIG :' $CONFIG

run.sh $cycle $CONFIG $NAMELIST_ARPE $INITFILE $NAMELIST_SURF $AROME_PREPSURF $AROME_PGD $TSTEP $NSTOP $levels $installpost $runpost $dirpost $configpost $FORCING_FILES_DIR > run_${cycle}_${CONFIG}_L${levels}_${TSTEP}s.log 2>&1

mv run_${cycle}_${CONFIG}_L${levels}_${TSTEP}s.log logs/
echo log file: logs/run_${cycle}_${CONFIG}_L${levels}_${TSTEP}s.log


date
