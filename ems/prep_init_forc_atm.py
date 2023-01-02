#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
Atmospheric namelist
"""

import os
import importlib
import logging
logger = logging.getLogger(__name__)

def prep_init_forc_atm(model, timestep, vertical_grid, **kwargs):
    """
    Takes a MUSC atmospheric namelist and modify it according to the case to simulate
    and described in ncfile
    :param model: name identifying the model and its version
    :param timestep: timestep
    :param vertical_grid: file name describing vertical grid
    :param **kwargs: other arguments needed by the chosen implementation
    :return: TO BE CLARIFIED
    """
    packages = {}
    feature = 'prep_init_forc_atm'
    #List of implementations
    implemented = [m[:-3] for m in os.listdir(os.path.dirname(os.path.abspath(__file__)))
                   if m.startswith(feature + '_') and m.endswith('.py')]
    #Module import
    packages[feature] = [importlib.import_module('..' + p, __name__) for p in implemented]
    #Look for the right implementation
    for name,pkg in zip(implemented, packages[feature]):
        if name == feature + '_' + model \
          or (name == feature + '_GMAP' and model in ['AROME','ARPPNT']):
            return pkg.prep_init_forc_atm(timestep, vertical_grid, **kwargs)
    # No suitable implementation is found
    msg = 'Suitable implementation of {0} not found for model {1}'.format(feature, model)
    logging.error(msg)
    raise ValueError(msg)
