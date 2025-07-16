#!/usr/bin/env python3
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '__REP_MUSC__'

############# Begin editing

GROUP = '49t1_clim1.01'
# EXPID will be taken from this file name following the convention config_EXPID.py

# Binaries
bindir = '/Users/romain/pack/49t1_clim1.01.MPIGNU1230.y.musc.v3/bin'
MASTER = os.path.join(bindir, 'MASTERODB')
PGD = os.path.join(bindir, 'PGD')
PREP = os.path.join(bindir, 'PREP')
ASCII2FA = os.path.join(bindir, 'ASCII2FA')

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/ARPPNT/L1_FCST_HYD_SL2_VFD_ARPPHY1D.nam.sorted.v1')
SFXNAM_prep = os.path.join(REP_MUSC, 'namelist/SURFEX/cy49t1_op0.04.nam-namel_prep_sfx')
SFXNAM_run = os.path.join(REP_MUSC, 'namelist/SURFEX/cy49t1_op0.04.nam-namel_previ_surfex')

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L105.dta')
timestep = 300

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
sfxfmt = 'FA'
loverwrite = True
lupdate_ATM = True
lupdate_SFX = True
lupdate_RUN = True

# ecoclimap data
#ecoclimap

# rrtm data
rrtm = '/scratch/work/roehrig/atm/rrtm/rrtm.const.04.tgz'

############# End editing
