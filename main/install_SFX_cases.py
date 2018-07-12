#!/usr/bin/env cdat
# -*- coding:UTF-8 -*-

#
# Install and prepare SURFEX PGD and PREP files
# for the cases given in configsim.py
# and a given model pack (eg PGD/PREP binaries) and a given SURFEX namelist
# defined in a config.py file
# To be simply used as 'install_SFX_cases.py CONFIG_FILE' 
# eg install_SFX_cases config/config_arp631_CMI6.py
#

import sys, os

REP_EMS = os.getenv('REP_EMS')
REP_MUSC = os.getenv('REP_MUSC')

import configmain
import configsim

if not(configsim.lsurfex):
  print 'SURFEX is disabled in configsim.py'
  sys.exit()

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

import EMS_cases as CC
import install_MUSC

repout = REP_MUSC + '/SURFEX/'

config = configmod.config 
cycle = configmod.cycle 
PGD = configmod.PGD 
PREP = configmod.PREP 
namSFXref = configmod.namSFXref 

if configsim.allcases:
  cases = CC.cases
else:
  cases = configsim.cases

if lasked:
  cases = [case,]

loverwrite = configmain.loverwrite
lupdate = configmain.lupdate

subcases = CC.subcases
data_input = CC.data_input

for case in cases:
  print case    
  if subcases.has_key(case):
    SS = subcases[case]
    if lasked: SS = [SUB,]      
    for subcase in SS:	
      print subcase
      install_MUSC.install_SFX(configsim.model,case,data_input[case][subcase],repout,cycle,PGD,PREP,config,namSFXref,subcase=subcase,loverwrite=loverwrite,lupdate=lupdate)
  else:
    install_MUSC.install_SFX(configsim.model,case,data_input[case],repout,cycle,PGD,PREP,config,namSFXref,loverwrite=loverwrite,lupdate=lupdate)
      

os.remove('config.py')      
os.remove('config.pyc')      
