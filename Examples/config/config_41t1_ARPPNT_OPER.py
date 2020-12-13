#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
REP_EMS = os.getenv('REP_EMS')
REP_MUSC = os.getenv('REP_MUSC')

############# Begin editing

GROUP = '41t1'
# EXPID will be taken from this file name following the convention config_EXPID.py (__name__ = config_EXPID) 
EXPID = __name__[7:]

# Binaries
MASTER = '/cnrm/amacs/USERS/roehrig/share/MUSC/41t1_op1.11.GFORTRAN610.x.EB.MUSC/bin/MASTERODB'

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/ARPPNT/namarp_41t1_ARPEGE_OPER')

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L105.dta')
timestep = 300

# Postprocessing
dirpost = os.path.join(REP_MUSC,'post')
variablesDict = 'variables.py'
defaultConfigPost = 'config_default.py'
caseDependent = True
lfaformat = 8

# EMS configuration
model = 'ARPPNT'
lsurfex = False
loverwrite = True
lupdate_ATM = True 
lupdate_RUN = True

# ecoclimap data
ecoclimap = os.path.join(REP_EMS, 'UTIL', 'ecoclimap_cnrm_cm6.02')

############# End editing
