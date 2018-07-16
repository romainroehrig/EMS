#!/usr/bin/env python
# -*- coding:UTF-8 -*-

#
# Install and run a MUSC simulation
# for the cases defined in configsim.py
# a a given configuration defined in configsim.py, configpost.py
# an in config/config_PACK_SIMU.py
# To be simply used as 'run_MUSC_cases.py CONFIG_FILE' 
# eg run_MUSC_cases.py config/config_arp631_CMIP6.py
# You can also choose the case and subcase you want to run with arguments:
# eg run_MUSC_cases.py config/config_arp631_CMIP6.py AYOTTE A24SC
#

import sys, os

REP_EMS = os.getenv('REP_EMS')
REP_MUSC = os.getenv('REP_MUSC')

import configmain


argv = sys.argv
try:
    config_file = argv[1]
    print 'your configuration file is', config_file
except:
    print "ERROR: you must give a configuration file, eg install_SFX_cases config/config_arp631_CMI6.py"
    sys.exit()

# In case a case/subcase is provided in argument
lasked = False
try:
    case = argv[2]
    SUB = argv[3]
    lasked = True
except:
    pass

try:
    os.remove('config.py')
except:
    pass

os.symlink(config_file,'./config.py')

import config as configmod
import configsim
import configpost

import EMS_cases as CC
import install_MUSC

nlev = configsim.nlev
timestep = configsim.timestep
model = configsim.model

if configsim.allcases:
  cases = CC.cases
else:
  cases = configsim.cases

if lasked:
  cases = [case,]

loverwrite = configmain.loverwrite
lupdate = configmain.lupdate

lsurfex = configsim.lsurfex

subcases = CC.subcases
data_input = CC.data_input

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

for case in cases:
  print case
  if configpost.caseDependent:
    configOut['configpost'] = 'config_' + case + '.py'      
  else:
    configOut['configpost'] = configpost.defaultConfigPost      
  if subcases.has_key(case):
    SS = subcases[case]
    if lasked: SS = [SUB,]
    for subcase in SS:
      print subcase
      config['initfile'] = REP_MUSC + '/ATM/'+model+'/' + case + '/' + subcase + '/initfile_L' + str(config['levels'])
      if model == 'ARPCLIMAT':
        config['forcingfiles'] = REP_MUSC + '/ATM/'+model+'/' + case + '/' + subcase + '/files_L' + str(config['levels']) + '_' + str(config['TSTEP']) + 's/'
      if lsurfex:
        config['PGDfile'] = REP_MUSC + '/SURFEX/' + config['cycle'] + '/' + config['name'] + '/' + case + '/' + subcase + '/PGD.lfi'
        config['PREPfile'] = REP_MUSC + '/SURFEX/' + config['cycle'] + '/' + config['name'] + '/' + case + '/' + subcase + '/PREP.lfi'
      install_MUSC.install_Run(model,case,data_input[case][subcase],repout,config,configOut,subcase=subcase,loverwrite=loverwrite,lupdate=lupdate)
  else:
    config['initfile'] = REP_MUSC + '/ATM/'+model+'/' + case + '/initfile_L' + str(config['levels'])
    if model == 'ARPCLIMAT':
      config['forcingfiles']= REP_MUSC + '/ATM/'+model+'/' + case + '/files_L' + str(config['levels']) + '_' + str(config['TSTEP']) + 's/'
    if lsurfex:
      config['PGDfile'] = REP_MUSC + '/SURFEX/' + config['cycle'] + '/' + config['name'] + '/' + case + '/PGD.lfi'
      config['PREPfile'] = REP_MUSC + '/SURFEX/' + config['cycle'] + '/' + config['name'] + '/' + case + '/PREP.lfi'      
    install_MUSC.install_Run(model,case,data_input[case],repout,config,configOut,loverwrite=loverwrite,lupdate=lupdate)

os.remove('config.py')
os.remove('config.pyc')
