#!/usr/bin/env python3
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '__REP_MUSC__'

############# Begin editing

GROUP = '46t1_op1.11'
# EXPID will be taken from this file name following the convention config_EXPID.py

# Binaries
bindir = '/cnrm/amacs/USERS/roehrig/share/EMS/pack/46t1_op1.11.MPIGFORTRAN920DBL.xfftw.musc/bin'
MASTER = os.path.join(bindir, 'MASTERODB')
#PGD
#PREP
ASCII2FA = os.path.join(bindir, 'ASCII2FA')

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/AROME/al46t1_arome-op1.11.nam-namel_previ_dyn_prod')
#SFXNAM_prep
#SFXNAM_run

# Model configuration
vert_grid = os.path.join(REP_MUSC, 'grid/L90_AROME.dta')
timestep = 50

# Postprocessing
dirpost = os.path.join(REP_MUSC,'post')
variablesDict = 'variables.py'
defaultConfigPost = 'config_default.py'
caseDependent = True

# EMS configuration
model = 'AROME'
lforc_ascii = False
lsurfex = False
#sfxfmt
loverwrite = True
lupdate_ATM = True
#lupdate_SFX
lupdate_RUN = True

# ecoclimap data
#ecoclimap

############# End editing
