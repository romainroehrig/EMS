#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
REP_MUSC = '/home/roehrig/MUSC/riette'

############# Begin editing

GROUP = '46t1'
# EXPID will be taken from this file name following the convention config_EXPID.py (__name__ = config_EXPID) 
EXPID = __name__[7:]

# Binaries
MASTER = '/home/roehrig/pack/46_t1.01.GFORTRAN610ECCOD.xfftw.test/bin/MASTERODB'
PGD = '/home/roehrig/pack/46_t1.01.GFORTRAN610ECCOD.xfftw.sfx/bin/PGD'
PREP = '/home/roehrig/pack/46_t1.01.GFORTRAN610ECCOD.xfftw.sfx/bin/PREP'
ASCII2FA = '/home/roehrig/pack/46_t1.01.GFORTRAN610ECCOD.xfftw.ascii2fa/bin/ASCII2FA'

# Namelists
ATMNAM = os.path.join(REP_MUSC, 'namelist/AROME/namarp_46t1_AROME_OPER')
SFXNAM = '/home/roehrig/pack/46_t1.01.GFORTRAN610ECCOD.xfftw.sfx/test/namsurf' #os.path.join(REP_MUSC, 'namelist/SURFEX/namarp_46t1_AROME_OPER')

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
lupdate_RUN = True

# ecoclimap data
#ecoclimap

############# End editing
