#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import logging
logging.basicConfig(format='%(asctime)s - %(name)30s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

try:
    from ems.prep_nam_atm import prep_nam_atm
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
    parser = argparse.ArgumentParser(description="Modify an ARPIFS namelist to simulate a case described " + \
                                                 "by a netcdf file using the DEPHY common format.")
    parser.add_argument('-m', '--model', dest='model', required=True,
                        help="String representing the model name and version")
    parser.add_argument('-t', '--timestep', dest='timestep', required=True,
                        help='Timestep to use for the simulation')
    surfex = parser.add_mutually_exclusive_group(required=False)
    surfex.add_argument('--surfex', dest='surfex_true', action='store_true', default=True,
                        help="to enable surfex usage (default)")
    surfex.add_argument('--nosurfex', dest='surfex_false', action='store_true', default=False,
                        help="to disable surfex usage")
    parser.add_argument('-nc', dest='ncfile', required=True,
                        help='netcdf file describing the case to simulate')
    parser.add_argument('namin', metavar='namin', type=str,
                        help='input ARPIFS namelist')
    parser.add_argument('-o', '--namout', dest='namout', type=str, required=False, default='namarp',
                        help='output namelist')
    args = parser.parse_args()
    
    prep_nam_atm(args.model, args.ncfile, args.namin, args.timestep, lsurfex=args.surfex_true, namout=args.namout)
