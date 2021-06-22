#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '__REP_MUSC__'

############# Begin editing

GROUP = '46t1_op1.01'
# EXPID will be taken from this file name following the convention config_EXPID.py (__name__ = config_EXPID) 
EXPID = __name__[7:]

# Binaries
bindir = '/cnrm/amacs/USERS/roehrig/share/EMS/pack/46t1_op1.01.GFORTRAN610ECCOD.xfftw.musc/bin'
MASTER = os.path.join(bindir, 'MASTERODB')
PGD = os.path.join(bindir, 'PGD')
PREP = os.path.join(bindir, 'PREP')
ASCII2FA = os.path.join(bindir, 'ASCII2FA')

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/AROME/al46t1_arome-op1.01.nam-namel_previ_dyn_prod')
SFXNAM_prep = os.path.join(REP_MUSC, 'namelist/SURFEX/al46t1_arome-op1.01.nam-namel_prep')
SFXNAM_run = os.path.join(REP_MUSC, 'namelist/SURFEX/al46t1_arome-op1.01.nam-namel_previ_surfex_prod')

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L90_AROME.dta')
timestep = 50

# Postprocessing
dirpost = os.path.join(REP_MUSC,'post')
variablesDict = 'variables.py'
defaultConfigPost = 'config_default.py'
caseDependent = True

# EMS configuration
model = 'AROME46t1'
lforc_ascii = False
lsurfex = True
sfxfmt = 'FA'
loverwrite = True
lupdate_ATM = True
lupdate_SFX = True
lupdate_RUN = True

# ecoclimap data
#ecoclimap

############# End editing
