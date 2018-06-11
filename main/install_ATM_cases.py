#!/usr/bin/env cdat
# -*- coding:UTF-8 -*-

#
# Install and prepare Atmospheric initial file (restart) and forcing files
# for a given case, a given vertical discretization and a given timestep
# defined in configsim.py
# To be simply used as 'install_ATM_cases.py'
#

import sys, os

REP_EMS = os.getenv('REP_EMS')
REP_MUSC = os.getenv('REP_MUSC')

import configmain
import configsim

import EMS_cases as CC
import install_MUSC


model = configsim.model
if model not in ['AROME','ARPCLIMAT']:
  print 'Model unexpected:', configsim.model
  sys.exit()


repout = REP_MUSC + '/ATM/' + model + '/'

nlev = configsim.nlev
timestep = configsim.timestep
if model == 'AROME':
    timestep = None


if configsim.allcases:
  cases = CC.cases
else:
  cases = configsim.cases

loverwrite = configmain.loverwrite
lupdate = configmain.lupdate

subcases = CC.subcases
data_input = CC.data_input

for case in cases:
  print case    
  if subcases.has_key(case):
    for subcase in subcases[case]:	
      print subcase
      install_MUSC.install_ATM(model,case,data_input[case][subcase],repout,nlev,timestep,subcase=subcase,loverwrite=loverwrite,lupdate=lupdate)
  else:
    install_MUSC.install_ATM(model,case,data_input[case],repout,nlev,timestep,loverwrite=loverwrite,lupdate=lupdate)
      
