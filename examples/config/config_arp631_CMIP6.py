#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '/Users/romainroehrig/MUSC/riette'

############# Begin editing

GROUP = 'V631'
# EXPID will be taken from this file name following the convention config_EXPID.py (__name__ = config_EXPID) 
EXPID = __name__[7:]

# Binaries
MASTER = '/Users/romainroehrig/rootpack/arp603_export.01.MPIGNU640.x/bin//MASTERODB'
PGD = '/Users/romainroehrig/rootpack/arp603_export.01.MPIGNU640.x/bin/PGD'
PREP = '/Users/romainroehrig/rootpack/arp603_export.01.MPIGNU640.x/bin/PREP'
#ASCII2FA

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/ARPCLIMAT/nam.atm.tl127l91r.CMIP6.v631')
SFXNAM = os.path.join(REP_MUSC, 'namelist/SURFEX/nam.sfx.tl127.CMIP6.v631')

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L91.dta')
timestep = 900

# Postprocessing
dirpost = os.path.join(REP_MUSC,'post')
variablesDict = 'variables.py'
defaultConfigPost = 'config_default.py'
caseDependent = True

# EMS configuration
model = 'ARPCLIMAT'
lsurfex = True
loverwrite = True
lupdate_ATM = True 
lupdate_SFX = True 
lupdate_RUN = True

# ecoclimap data
#ecoclimap

############# End editing