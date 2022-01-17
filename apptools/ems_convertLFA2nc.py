#!/usr/bin/env python
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import os
import logging
logging.basicConfig(format='%(asctime)s - %(name)20s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

import netCDF4 as nc

import ems
from ems.lfa2nc import lfa2nc

import variables as VV
import config

solib = os.path.join(ems._dirEMS, '../aux/lfa4py/lfa4py.so')

if not(os.path.exists('netcdf')):
    os.makedirs('netcdf')
else:
    os.system('rm -f netcdf/*')


# Dict for variable attributes
#varatts = {}
#for var in VV.variables.keys():
#    varatts[var] = dict(varname=VV.varnames[var], 
#                        name=VV.names[var] if var in VV.names.keys() else VV.varnames[var], 
#                        units=VV.units[var] if var in VV.units.keys() else '-',
#                        coef=VV.coefs[var] if var in VV.coefs.keys() else 1)

# From lfa to netcdf
fin = 'netcdf/Out_klevel.nc'
lfa2nc('LFA', fin,
       tosave=None if config.saveall else config.var2save,
       varatts=VV.variables,
       solib=solib)

# Operations on model levels
if config.convertk1h:
    fout = 'netcdf/Out_1hourly_klevel.nc'
    os.system('cdo hourmean {0} {1}'.format(fin,fout))

if config.convertk3h:
    fout = 'netcdf/Out_3hourly_klevel.nc'
    f = nc.Dataset(fin)
    time = f['time'][:]
    f.close()
    timestep = time[1]-time[0]
    nt = int(3.*3600./timestep)
    os.system('cdo timselmean,{0} {1} {2}'.format(nt,fin,fout))

if config.convertkday:
    fout = 'netcdf/Out_daily_klevel.nc'
    os.system('cdo daymean {0} {1}'.format(fin,fout))

# Interpolation on pressure levels + time averages
if config.convert2p:
    os.system('./ems_convert2p.py')

    fin = 'netcdf/Out_plevel.nc'
    if config.convertp1h:
        fout = 'netcdf/Out_1hourly_plevel.nc'
        os.system('cdo hourmean {0} {1}'.format(fin,fout))

    if config.convertp3h:
        fout = 'netcdf/Out_3hourly_plevel.nc'
        f = nc.Dataset(fin)
        time = f['time'][:]
        f.close()
        timestep = time[1]-time[0]
        nt = int(3.*3600./timestep)
        os.system('cdo timselmean,{0} {1} {2}'.format(nt,fin,fout))

    if config.convertpday:
        fout = 'netcdf/Out_daily_plevel.nc'
        os.system('cdo daymean {0} {1}'.format(fin,fout))

# Interpolation en altitude levels + time averages
if config.convert2z:
    os.system('./ems_convert2z.py')
    fin = 'netcdf/Out_zlevel.nc'
    if config.convertz1h:
        fout = 'netcdf/Out_1hourly_zlevel.nc'
        os.system('cdo hourmean {0} {1}'.format(fin,fout))

    if config.convertz3h:
        fout = 'netcdf/Out_3hourly_zlevel.nc'
        f = nc.Dataset(fin)
        time = f['time'][:]
        f.close()
        timestep = time[1]-time[0]
        nt = int(3.*3600./timestep)
        os.system('cdo timselmean,{0} {1} {2}'.format(nt,fin,fout))

    if config.convertzday:
        fout = 'netcdf/Out_daily_zlevel.nc'
        os.system('cdo daymean {0} {1}'.format(fin,fout))

