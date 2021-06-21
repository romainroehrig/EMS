#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '__REP_MUSC__'

############# Begin editing

GROUP = '46t1'
# EXPID will be taken from this file name following the convention config_EXPID.py (__name__ = config_EXPID) 
EXPID = __name__[7:]

# Binaries
bindir = '/cnrm/amacs/USERS/roehrig/share/EMS/pack/46_t1.01.GFORTRAN610ECCOD.xfftw.musc/bin'
MASTER = os.path.join(bindir, 'MASTERODB')
PGD = os.path.join(bindir, 'PGD')
PREP = os.path.join(bindir, 'PREP')
ASCII2FA = os.path.join(bindir, 'ASCII2FA')

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/AROME/namarp_46t1_AROME_OPER')
SFXNAM = os.path.join(REP_MUSC, 'namelist/SURFEX/nam.sfx.46t1.test')

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
