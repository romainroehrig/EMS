#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import os
import shutil

import logging
logging.basicConfig(format='%(asctime)s - %(name)30s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

try:
    from ems.prep_init_forc_atm import prep_init_forc_atm
except ImportError:
    #python2
    logger.error("This script is available only if package ems can be imported in python.")
    raise
except ModuleNotFoundError:
    #python3
    logger.error("This script is available only if package ems can be imported in python.")
    raise

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description="Create nam1d needed by acadfa to create ARPIFS restart")
    parser.add_argument('-m', '--model', dest='model', required=True,
                        help="String representing the model name and version")
    parser.add_argument('-t', '--timestep', dest='timestep', type=int, required=True,
                        help='Timestep to use for the simulation')
    parser.add_argument('-nc', dest='ncfile', required=True,
                        help='netcdf file describing the case to simulate')
    parser.add_argument('-vgrid', dest='vertical_grid', type=str, required=True,
                        help='vertical grid description file')
    parser.add_argument('-o', '--namout', dest='namout', type=str, required=False, default='nam1D',
                        help='output nam1D filename (default: nam1D)')
    parser.add_argument('--ascii', dest='ascii_true', action='store_true', default=False,
                        help="Forcing written in ASCII files instead of in restart file")
    parser.add_argument('--save-init', dest='save_init_true', action='store_true', default=False,
                        help="Saving initial variables in netCDF file (default: False)")
    parser.add_argument('--save-forc', dest='save_forc_true', action='store_true', default=False,
                        help="Saving initial variables in netCDF file (default: False)")
    parser.add_argument('--diags', dest='diags_true', action='store_true', default=False,
                        help="Perform some diagnostics (default: False)")
    args = parser.parse_args()

    dirdiags = None
    if args.diags_true: 
        dirdiags = './images'
        if os.path.exists(dirdiags):
            shutil.rmtree(dirdiags)
        os.mkdir(dirdiags)

    dirforc = None
    if args.ascii_true:
        dirforc = './forcing'
        if os.path.exists(dirforc):
            shutil.rmtree(dirforc)
        os.mkdir(dirforc)
    
    prep_init_forc_atm(args.model, args.timestep, args.vertical_grid, ncfile=args.ncfile, nam1d=args.namout,
                 lforc_ascii=args.ascii_true,
                 dirforc=dirforc, dirdiags=dirdiags,
                 save_init=args.save_init_true, file_init='init.nc',
                 save_forc=args.save_forc_true, file_forc='forc.nc')
