#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import logging
logging.basicConfig(format='%(asctime)s - %(name)30s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

try:
    from ems.prep_nam_sfx import prep_nam_sfx
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
    parser = argparse.ArgumentParser(description="Modify a SURFEX namelist to simulate a case described " + \
                                                 "by a netcdf file using the DEPHY common format.")
    parser.add_argument('-nc', dest='ncfile', required=True,
                        help='netcdf file describing the case to simulate')
    sfxfmt = parser.add_mutually_exclusive_group(required=False)
    sfxfmt.add_argument('-lfi', dest='lfi_true', action='store_true', default=True,
                        help="Use LFI format for SURFEX files (default)")
    sfxfmt.add_argument('-fa', dest='fa_true', action='store_true', default=False,
                        help="Use FA format for SURFEX files")
    parser.add_argument('namin', metavar='namin', type=str,
                        help='input SURFEX namelist')
    parser.add_argument('-o', '--namout', dest='namout', type=str, required=False, default='namsurf',
                        help='name of the output namelist')
    args = parser.parse_args()

    if args.lfi_true: sfxfmt = 'LFI'
    if args.fa_true: sfxfmt = 'FA'
    
    prep_nam_sfx(args.ncfile, args.namin, namout=args.namout, sfxfmt=sfxfmt)
