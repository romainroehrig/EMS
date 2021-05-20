#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
DEPHY common format for MUSC model
------------------------------------------------------------------

``dephycf_musc`` is a package designed to deal with the DEPHY common format for the MUSC model

"""

import os

__all__ = []

__version__ = '0.0.1'

__license__ = 'CeCILL-C'

__authors__ = ['Romain Roehrig']

__contributors__ = ['Sébastien Riette']

_dirEMS = os.path.dirname(os.path.abspath(__file__))

# COMPONENTS (modules) #
#from .musc_namel_run_atm import musc_namel_run_atm
#from .musc_namel_sfx import musc_namel_sfx
from .prep_init_forc_atm_ARPCLIMAT import prep_init_forc_atm
from .prep_nam_atm_ARPCLIMAT import prep_nam_atm
from .prep_nam_sfx import prep_nam_sfx
from .lfa import lfareader
from .lfa2nc import lfa2nc
