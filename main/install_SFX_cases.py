#!/usr/bin/env python
# -*- coding:UTF-8 -*-

#
# Install and prepare SURFEX PGD and PREP files
# for the cases given in configsim.py
# and a given model pack (eg PGD/PREP binaries) and a given SURFEX namelist
# defined in a config.py file
# To be simply used as 'install_SFX_cases.py CONFIG_FILE' 
# eg install_SFX_cases config/config_arp631_CMI6.py
#

import os

REP_EMS = os.getenv('REP_EMS')
REP_MUSC = os.getenv('REP_MUSC')

import sys

sys.path.append('{0}/UTIL/install')

import argparse

import EMS_cases as CC
import install_MUSC

import configmain
import configsim

if not(configsim.lsurfex):
  print 'SURFEX is disabled in configsim.py'
  sys.exit()

# Definition of arguments
parser = argparse.ArgumentParser()
parser.add_argument("-case", help="case", type=str, required=True)
parser.add_argument("-subcase", help="subcase (default: REF)", type=str, default="REF")
parser.add_argument("-config", help="configuration file", type=str, required=True)

# Getting arguments
args = parser.parse_args()
case = args.case
subcase = args.subcase
config_file = args.config
print 'your configuration file is', config_file

# Importing config file
try:
    os.remove('config.py')
except:
    pass

os.symlink(config_file,'./config.py')

import config as configmod


# Installing SURFEX for case/subcase
repout = REP_MUSC + '/SURFEX/'

config = configmod.config 
cycle = configmod.cycle 
PGD = configmod.PGD 
PREP = configmod.PREP 
namSFXref = configmod.namSFXref 

loverwrite = configmain.loverwrite
lupdate = configmain.lupdate

data_input = CC.data_input

if not(case in CC.cases):
    print 'ERROR: case {0} is not known'.format(case)
    print 'ERROR: known cases:', CC.cases
    sys.exit()

if not(subcase in CC.subcases[case]):
    print 'ERROR: subcase {0} is not known for case {1}'.format(subcase,case)
    print 'ERROR: known subcases for case {0}:'.format(case), CC.subcases[case]

install_MUSC.install_SFX(configsim.model,case,subcase,data_input[case][subcase],repout,cycle,PGD,PREP,config,namSFXref,loverwrite=loverwrite,lupdate=lupdate)

os.remove('config.py')      
os.remove('config.pyc')      
