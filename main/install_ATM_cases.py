#!/usr/bin/env python
# -*- coding:UTF-8 -*-

#
# Install and prepare Atmospheric initial file (restart) and forcing files
# for a given case, a given vertical discretization and a given timestep
# defined in configsim.py
# To be simply used as 'install_ATM_cases.py'
#

import os
REP_EMS = os.getenv('REP_EMS')
REP_MUSC = os.getenv('REP_MUSC')

import sys

sys.path.append('{0}/UTIL/install')

import argparse

import configmain
import configsim

import EMS_cases as CC
import install_MUSC

# Definition of arguments
parser = argparse.ArgumentParser()
parser.add_argument("-case", help="case", type=str, required=True)
parser.add_argument("-subcase", help="subcase (default: REF)", type=str, default="REF")

# Getting arguments
args = parser.parse_args()
case = args.case
subcase = args.subcase

model = configsim.model
if model not in ['ARPCLIMAT']:
  print 'Model unexpected:', configsim.model
  sys.exit()

# Installing ARPEGE initial and forcing files
repout = REP_MUSC + '/ATM/' + model + '/'

nlev = configsim.nlev
timestep = configsim.timestep

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
    sys.exit()

install_MUSC.install_ATM(model,case,subcase,data_input[case][subcase],repout,nlev,timestep,loverwrite=loverwrite,lupdate=lupdate)
