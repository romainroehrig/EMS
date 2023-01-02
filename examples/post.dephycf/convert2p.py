#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import netCDF4 as nc

import numpy as np
from scipy import interpolate

import config

levout = config.levout
levout = np.array(levout,dtype=np.double)
nlevout = levout.shape[0]

missing_value = 1.e20

fin = nc.Dataset('netcdf/Out_klevel.nc')

dimensions = fin.dimensions.keys()
variables = fin.variables.keys() 

pres = fin['ph'][:]/100.
presf = fin['pf'][:]/100.
nt,nlev = presf.shape
nt,nlev1 = pres.shape

time = fin['time'][:]

fulltime = np.transpose(np.tile(time,(nlev,1)))
fulltime1 = np.transpose(np.tile(time,(nlev1,1)))
fulltimep = np.transpose(np.tile(time,(nlevout,1)))

full_levout = np.tile(levout,(nt,1))

fout = nc.Dataset('netcdf/Out_plevel.nc','w',format='NETCDF3_CLASSIC')
dim_tmp = {}
dims_tmp = {}
for dim in dimensions:
    if fin.dimensions[dim].isunlimited():
        dim_tmp[dim] = fout.createDimension(dim,None)
    else:
        dim_tmp[dim] = fout.createDimension(dim,fin.dimensions[dim].size)
    dims_tmp[dim] = fout.createVariable(dim,type(np.array(fin[dim])[0]),(dim,))
    for att in fin[dim].ncattrs():
        dims_tmp[dim].setncattr(att,fin[dim].getncattr(att))
    dims_tmp[dim][:] = fin[dim][:]

dim = 'levp'
dim_tmp[dim] = fout.createDimension(dim,nlevout)
dims_tmp[dim] = fout.createVariable(dim,'f8',(dim,))
dims_tmp[dim].setncattr('units','hPa')
dims_tmp[dim].setncattr('long_name','pressure_level')
dims_tmp[dim].setncattr('axis','Z')
dims_tmp[dim][:] = levout[:]

var_tmp = {}
for var in variables:
    if var not in dimensions:
        print var
        if len(fin[var].dimensions) == 2 and fin[var].dimensions[1] == 'levf':

            var_tmp[var] = fout.createVariable(var,'f4',('time','levp'),fill_value=missing_value)
            for it in range(0,nt):
                finterp = interpolate.interp1d(presf[it,:],fin[var][it,:],bounds_error=False,fill_value=missing_value)
                var_tmp[var][it,:] = finterp(levout)

        elif len(fin[var].dimensions) == 2 and fin[var].dimensions[1] == 'levh':

            var_tmp[var] = fout.createVariable(var,'f4',('time','levp'),fill_value=missing_value)
            for it in range(0,nt):
                finterp = interpolate.interp1d(pres[it,:],fin[var][it,:],bounds_error=False,fill_value=missing_value)
                var_tmp[var][it,:] = finterp(levout)

        else:

            var_tmp[var] = fout.createVariable(var,'f4',fin[var].dimensions,fill_value=missing_value)
            var_tmp[var][:] = fin[var][:]

        for att in fin[var].ncattrs():
            var_tmp[var].setncattr(att,fin[var].getncattr(att))

fout.close()

fin.close()
