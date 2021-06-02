#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '/Users/romainroehrig/MUSC/riette'

############# Begin editing

GROUP = '46t1'
# EXPID will be taken from this file name following the convention config_EXPID.py (__name__ = config_EXPID) 
EXPID = __name__[7:]

# Binaries
MASTER = '/Users/romainroehrig/rootpack/46_t1.01.MPIGNU640.x/bin/MASTERODB'
ASCII2FA = '/Users/romainroehrig/rootpack/46_t1.01.MPIGNU640.x/bin/ASCII2FA'

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/AROME/namarp_46t1_AROME_OPER')

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L60_AROME.dta')
timestep = 50

# Postprocessing
dirpost = os.path.join(REP_MUSC,'post')
variablesDict = 'variables.py'
defaultConfigPost = 'config_default.py'
caseDependent = True

# EMS configuration
model = 'AROME46t1'
lsurfex = False
loverwrite = True
lupdate_ATM = True 
lupdate_RUN = True

# ecoclimap data
#ecoclimap

############# End editing
