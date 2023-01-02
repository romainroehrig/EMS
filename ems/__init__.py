#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
Environment for MUSC Simulations
------------------------------------------------------------------

``dephycf`` is a package designed within the DEPHY framework, 
which proposes a netCDF SCM/LES input format.

"""

import os

__all__ = []

__version__ = '1.0.0'

__license__ = 'CeCILL-C'

__authors__ = ['Romain Roehrig']

__contributors__ = ['Sébastien Riette']

_dirEMS = os.path.dirname(os.path.abspath(__file__))

# COMPONENTS (modules) #
from .prep_init_forc_atm import prep_init_forc_atm
from .prep_nam_atm import prep_nam_atm
from .prep_nam_sfx import prep_nam_sfx
from .lfa import lfareader
from .lfa2nc import lfa2nc
