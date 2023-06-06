#!/usr/bin/env python3
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '__REP_MUSC__'

############# Begin editing

GROUP = 'V650'
# EXPID will be taken from this file name following the convention config_EXPID.py

# Binaries
bindir = '/cnrm/amacs/USERS/roehrig/share/EMS/pack/arp650_climat.00.MPIGNU930.cx/bin'
MASTER = os.path.join(bindir, 'MASTER')
PGD = os.path.join(bindir, 'PGD')
PREP = os.path.join(bindir, 'PREP')
ASCII2FA = os.path.join(bindir, 'ASCII2FA')

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/ARPCLIMAT/nam.atm.tl127l91r.P7.RRTM.v650')
SFXNAM = os.path.join(REP_MUSC, 'namelist/SURFEX/nam.sfx.tl127.CMIP6.v642')

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L91.dta')
timestep = 300

# Postprocessing
dirpost = os.path.join(REP_MUSC,'post')
variablesDict = 'variables.py'
defaultConfigPost = 'config_default.py'
caseDependent = True

# EMS configuration
model = 'ARPCLIMAT'
#lascii_forc
lsurfex = True
#sfxfmt
loverwrite = True
lupdate_ATM = True
lupdate_SFX = True
lupdate_RUN = True

# ecoclimap data
#ecoclimap

############# End editing
