#!/usr/bin/env python
# -*- coding:UTF-8 -*-
import os, sys

import netCDF4 as nc

import argparse

import config

# Definition of arguments
parser = argparse.ArgumentParser()
parser.add_argument("-format", help="format of LFA files", type=int, required=True)

# Getting arguments
args = parser.parse_args()
lfaformat = args.format

if not(os.path.exists('netcdf')):
    os.makedirs('netcdf')
else:
    os.system('rm -f netcdf/*')

# From lfa to netcdf
os.system('python lfa2nc.py -format {0}'.format(lfaformat))
os.system('mv Out_klevel.nc netcdf/')

fin = 'netcdf/Out_klevel.nc'

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
    os.system('python convert2p.py')

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
    os.system('python convert2z.py')
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

