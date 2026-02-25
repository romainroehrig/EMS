#!/usr/bin/env python3
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '__REP_MUSC__'

############# Begin editing

GROUP = 'V73'
# EXPID will be taken from this file name following the convention config_EXPID.py

# Binaries
bindir = '/home/gmgec/mrgc/roehrig/pack/cy49t1_clim1.03.IMPIIFC2302DP.y.musc/bin'
MASTER = os.path.join(bindir, 'MASTERODB')
PGD = os.path.join(bindir, 'PGD')
PREP = os.path.join(bindir, 'PREP')
ASCII2FA = os.path.join(bindir, 'ASCII2FA')

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/ARPCLIMAT/nam.atm.AGCM.tl127l91r.cy49t1_clim1.03')
SFXNAM_prep = os.path.join(REP_MUSC, 'namelist/SURFEX/nam.sfx.AGCM.tl127r.cy49t1_clim1.03')
SFXNAM_run = SFXNAM_prep

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L91.dta')
timestep = 900

# Postprocessing
dirpost = os.path.join(REP_MUSC,'post.dephycf')
variablesDict = 'variables.py'
defaultConfigPost = 'config_default.py'
caseDependent = True

# EMS configuration
model = 'ARPPNT'
cycle = 49
lforc_ascii = False
lsurfex = True
surfex_version = 9
sfxfmt = 'FA'
loverwrite = True
lupdate_ATM = True
lupdate_SFX = True
lupdate_RUN = True

# ecoclimap data
#ecoclimap

# rrtm data
rrtm = '/scratch/work/roehrig/atm/rrtm/ecrad.cy49t1.tgz'

############# End editing
