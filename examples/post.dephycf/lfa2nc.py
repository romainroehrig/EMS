#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

from ems.lfa2nc_dephycf import lfa2nc

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Converts LFA into netcdf')
    parser.add_argument("-f", type=str, dest='conf_file', default=None,
                        help='File containing the netcdf variable to produce (one by line)')
    parser.add_argument("-l", type=str, dest='solib', default=None,
                        help="Path to a shared lib with wlfa* entries, or None to use epygram. " + \
                        "The share lib can be compiled with the EMS source code, or compiled " + \
                        "with gmkpack (-p libs4py) or taken in the epygram directory.")
    parser.add_argument("-d", type=str, dest='dirlfa', help="directory containing the LFA files")
    parser.add_argument('-o', type=str, dext='ncout', help='netcdf file to produce')
    parser.add_argument('-v', action='store_true', dest='verbose', help='Add verbosity')
    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=getattr(logging, 'DEBUG', None))
    lfa2nc(args.dirlfa, args.ncout, args.conf_file, solib=args.solib)
