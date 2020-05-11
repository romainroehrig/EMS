#!/usr/bin/env python
# -*- coding:UTF-8 -*-

#
# Install and run a MUSC simulation
# for a given configuration defined in configsim.py, configpost.py
# and in config/config_PACK_SIMU.py
# To be simply used as 'run_MUSC_cases.py -case CASE [-subcase SUBCASE] -config CONFIG_FILE' 
# eg run_MUSC_cases.py -case ARMCU -subcase REF -config config/config_arp631_CMIP6.py
# subcase is not required. In this case, default is REF.
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
import configpost

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

nlev = configsim.nlev
timestep = configsim.timestep
model = configsim.model

loverwrite = configmain.loverwrite
lupdate = configmain.lupdate

lsurfex = configsim.lsurfex

subcases = CC.subcases
data_input = CC.data_input

if not(case in CC.cases):
    print 'ERROR: case {0} is not known'.format(case)
    print 'ERROR: known cases:', CC.cases
    sys.exit()

if not(subcase in CC.subcases[case]):
    print 'ERROR: subcase {0} is not known for case {1}'.format(subcase,case)
    print 'ERROR: known subcases for case {0}:'.format(case), CC.subcases[case]

config = {}

config['cycle'] = configmod.cycle 
config['MASTER'] = configmod.MASTER 
config['ecoclimap'] = configmain.ecoclimap

config['levels'] = nlev
config['TSTEP'] = timestep

config['lsurfex'] = lsurfex

config['name'] = configmod.config 
config['namATMref'] = configmod.namATMref
if lsurfex:
  config['namSFXref'] = configmod.namSFXref 

configOut = {}

configOut['dirpost'] = configpost.dirpost
configOut['variablesDict'] = configpost.variablesDict

repout = REP_MUSC + '/simulations/'

if configpost.caseDependent:
    configOut['configpost'] = 'config_' + case + '.py'      
else:
    configOut['configpost'] = configpost.defaultConfigPost      
      
config['initfile'] = REP_MUSC + '/ATM/'+model+'/' + case + '/' + subcase + '/initfile_L' + str(config['levels'])
if model == 'ARPCLIMAT':
    config['forcingfiles'] = REP_MUSC + '/ATM/'+model+'/' + case + '/' + subcase + '/files_L' + str(config['levels']) + '_' + str(config['TSTEP']) + 's/'
if lsurfex:
    config['PGDfile'] = REP_MUSC + '/SURFEX/' + config['cycle'] + '/' + config['name'] + '/' + case + '/' + subcase + '/PGD.lfi'
    config['PREPfile'] = REP_MUSC + '/SURFEX/' + config['cycle'] + '/' + config['name'] + '/' + case + '/' + subcase + '/PREP.lfi'

install_MUSC.install_Run(model,case,subcase,data_input[case][subcase],repout,config,configOut,loverwrite=loverwrite,lupdate=lupdate)

os.remove('config.py')
os.remove('config.pyc')
