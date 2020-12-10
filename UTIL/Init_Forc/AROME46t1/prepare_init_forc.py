#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
repEMS = os.getenv('REP_EMS')

import sys
sys.path.append('{0}/DEPHY-SCM/utils/'.format(repEMS))

import math
import numpy as np

import netCDF4 as nc

from Axis import Axis
from Variable import Variable, interpol
from Case import Case

import config

lverbose = False
lplot = True

if lplot:
    rep_images = {}
    rep_images['orig'] = './images/orig/'
    rep_images['MUSC'] = './images/MUSC/'
    rep_images['comp'] = './images/comp/'

    for v in rep_images.keys():
        if not(os.path.exists(rep_images[v])):
            os.makedirs(rep_images[v])

lforc = True #config.lforc
lnam1D = True #config.lnam1D

lsave_init = True
lsave_forc = True

nlev_out = config.nlev

#---------------------------------------------------------------
# Reading case information
#---------------------------------------------------------------

case = Case('{0}/{1}'.format(config.case,config.subcase))
case.read('data_input.nc')

if lverbose:
    case.info()

attributes = case.attributes

lat = float(case.lat)
lon = float(case.lon)

zorog = case.attributes['zorog']

startDate = case.startDate
year = int(startDate[0:4])
month = int(startDate[4:6])
day = int(startDate[6:8])
hour = int(startDate[8:10])
minute = int(startDate[10:12])
second = int(startDate[12:14])

nt,nlat,nlon = case.variables['ps_forc'].data.shape

#---------------------------------------------------------------
# Half-level pressure
#---------------------------------------------------------------

vah = np.zeros((nlev_out+1),dtype=np.float)
vbh = np.zeros((nlev_out+1),dtype=np.float)

f = open('L' + str(nlev_out) + '.dta')

for ilev in range(0,nlev_out+1):
    line = f.readline().split()
    vah[ilev] = float(line[0])
    vbh[ilev] = float(line[1])

f.close()

pph = np.zeros((nt,nlev_out+1),dtype=np.float)

for ilev in range(0,nlev_out+1):
    pph[:,ilev] = vah[ilev] + vbh[ilev]*case.variables['ps_forc'].data[:,0,0]	  

#---------------------------------------------------------------
# Full-level pressure
#---------------------------------------------------------------

ppf = np.zeros((nt,nlev_out),dtype=np.float)

pph = np.where(pph < 0.1,0.1,pph)

for ilev in range(0,nlev_out):
    # For reproductibility with CNRM machines
    for it in range(0,nt):
        ppf[it,ilev] = (pph[it,ilev+1]*math.log(pph[it,ilev+1])-pph[it,ilev]*math.log(pph[it,ilev]))/(pph[it,ilev+1]-pph[it,ilev]) - 1.

ppf = np.exp(ppf)	  

#---------------------------------------------------------------
# Interpolation verticale vers les niveaux modeles (pph)
#---------------------------------------------------------------

#case.variables['pressure'].info()

levin = Axis('lev',case.variables['pressure'].data[0,:,0,0],name='pressure',units='Pa')
levout = Axis('lev',ppf[0,:],name='pressure',units='Pa')

datain = {}
dataout = {}
for var in ['u','v','temp','theta','qv','ql','qi','tke']:
    datain[var] = Variable(var,
            data=case.variables[var].data,
            units=case.variables[var].units,
            name=case.variables[var].name,
            level=levin,
            time=case.variables[var].time,
            lat=case.variables[var].lat,
            lon=case.variables[var].lon,
            plotcoef=case.variables[var].plotcoef,
            plotunits=case.variables[var].plotunits)
    if lverbose:
        datain[var].info()

    dataout[var] = interpol(datain[var],levout=levout)
    if lverbose:
        dataout[var].info()

    if lplot:
        dataout[var].plotcoef = case.variables[var].plotcoef
        dataout[var].plotunits = case.variables[var].plotunits
        dataout[var].plot(rep_images=rep_images['comp'],timeunits='hours',levunits='hPa',var2=datain[var],label="MUSC",label2="Orig")

#---------------------------------------------------------------
# Preparing forcings
#---------------------------------------------------------------

dataout_forc = {}

timein = case.variables['pressure_forc'].time
nt_f = timein.length

def prep_forcing(var,lwrite=True):
    din = Variable(var,
            data=case.variables[var].data,
            units=case.variables[var].units,
            name=case.variables[var].name,
            level=levin,
            time=case.variables[var].time,
            lat=case.variables[var].lat,
            lon=case.variables[var].lon,
            plotcoef=case.variables[var].plotcoef,
            plotunits=case.variables[var].plotunits)
    if lverbose:
        din.info()

    dout = interpol(din,levout=levout)
    if lverbose:
        dout.info()

    if lplot:
        din.plot(rep_images=rep_images['orig'], timeunits='hours',levunits='hPa')
        dout.plotcoef = case.variables[var].plotcoef
        dout.plotunits = case.variables[var].plotunits
        dout.plot(rep_images=rep_images['MUSC'],timeunits='hours',levunits='hPa')

    return din, dout


#---------------------------------------------------------------
# Writing nam1D
#---------------------------------------------------------------

if lnam1D:

    # Computing number of forcing fields
    nb_f = 0
    if case.attributes['adv_temp'] == 1 or case.attributes['rad_temp'] == 1:
        nb_f += 1
        if case.attributes['adv_temp'] == 1:
            var = 'temp_adv'
            datain[var], dataout_forc[var] = prep_forcing(var) 
        else:
            dataout_forc['temp_adv'] = 0
        if case.attributes['rad_temp'] == 1:
            var = 'temp_rad'
            datain[var], dataout_forc[var] = prep_forcing(var)              
            dataout_forc['temp_adv'] += dataout[var]

    if case.attributes['adv_qv'] == 1:
        nb_f += 1
        var = 'qv_adv'
        datain[var], dataout_forc[var] = prep_forcing(var)          

#    if case.attributes['adv_u'] == 1:
#        nb_f += 1
#        var = 'u_adv'
#        datain[var], dataout_forc[var] = prep_forcing(var)          
#    if case.attributes['adv_v'] == 1:
#        nb_f += 1
#        var = 'v_adv'
#        datain[var], dataout_forc[var] = prep_forcing(var)        

    if case.attributes['forc_omega'] == 1 or case.attributes['forc_w'] == 1:
        nb_f += 1
        if case.attributes['forc_w'] == 1:
            var = 'w'
            datain[var], dataout_forc[var] = prep_forcing(var)
        elif case.attributes['forc_omega'] == 1:
            var = 'omega'
            datain[var], dataout_forc[var] = prep_forcing(var)            

    if case.attributes['forc_geo']:
        nb_f += 2
        for var in ['ug','vg']:
            datain[var], dataout_forc[var] = prep_forcing(var)        

    # Computing number of surface forcing fields
    nb_fs = 0

    if case.attributes['surfaceForcing'] == 'surfaceFlux':
        nb_fs += 2

    if case.attributes['surfaceForcingWind'] == 'ustar':
        nb_fs += 1

    if nt_f <= 1:
        dt = 0.
    else:  
        dt = timein.data[1]-timein.data[0]

    g = open('nam1D_L' + str(nlev_out),'w')

    print >>g, '&NAM1D'
    print >>g, '  LMAP    = .FALSE.,'
    print >>g, '  IFLEV   = {0},'.format(int(nlev_out))
    print >>g, '  ZDELY   = 250000.,'
    print >>g, '  LNHDYN  = .FALSE.,'
    print >>g, '  LALAPHYS= .TRUE.,'
    print >>g, '  LREASUR = .TRUE.,'
    print >>g, '  NFORC   = {0},'.format(nb_f*nt_f)
    print >>g, '  NFORCS  = {0},'.format(nb_fs*nt_f)
    print >>g, '  LQCGRP  = .FALSE.,'
    print >>g, '  LQIGRP  = .FALSE.,'
    print >>g, '  LQRGRP  = .FALSE.,'
    print >>g, '  LQSGRP  = .FALSE.,'
    print >>g, '  LQGGRP  = .FALSE.,'
    print >>g, '  LCFGRP  = .FALSE.,'
    print >>g, '  LSRCGRP = .FALSE.,'
    print >>g, '  LTKEGRP = .FALSE.,'
    print >>g, '  IYEAR   = {0},'.format(int(year)) 
    print >>g, '  IMONTH  = {0},'.format(int(month)) 
    print >>g, '  IDAY    = {0},'.format(int(day)) 
    print >>g, '  IHH     = {0},'.format(int(hour)) 
    print >>g, '  IMIN    = {0},'.format(int(minute))
    print >>g, '/'

    print >>g, 'ETA'

    print >>g, 'vah'
    for ilev in range(0,nlev_out+1):
        print >>g, vah[ilev]	

    print >>g, 'vbh'
    for ilev in range(0,nlev_out+1):
        print >>g, vbh[ilev]	

    print >>g, 'ATMOSPHERE'
    print >>g, 'zorog'
    print >>g, zorog
    print >>g, 'ps (Pa)'
    print >>g, np.log(case.variables['ps'].data[0,0,0])

    print >>g, 'U'
    for ilev in range(0,nlev_out):
        print >>g, dataout['u'].data[0,ilev,0,0]

    print >>g, 'V'
    for ilev in range(0,nlev_out):
        print >>g, dataout['v'].data[0,ilev,0,0]

    print >>g, 'T'
    for ilev in range(0,nlev_out):
        print >>g, dataout['temp'].data[0,ilev,0,0]

    print >>g, 'QV'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['qv'].data[0,ilev,0,0])

    print >>g, 'FORCING'

    if case.attributes['adv_temp'] == 1:
        for it in range(0,nt_f): 
            print >>g,' T ADV ',int(it*dt) ,'s'
            for ilev in range(0,nlev_out):
                print >>g, dataout_forc['temp_adv'].data[it,ilev,0,0]

    if case.attributes['adv_qv'] == 1:
        for it in range(0,nt_f): 
            print >>g,' Qv ADV ',int(it*dt) ,'s'
            for ilev in range(0,nlev_out):
                print >>g, dataout_forc['qv_adv'].data[it,ilev,0,0]

    if case.attributes['forc_geo'] == 1:
        for it in range(0,nt_f): 
            print >>g,' PFUG ',int(it*dt) ,'s'
            for ilev in range(0,nlev_out):
                print >>g, dataout_forc['ug'].data[it,ilev,0,0]
        for it in range(0,nt_f): 
            print >>g,' PFVG ',int(it*dt) ,'s'
            for ilev in range(0,nlev_out):
                print >>g, dataout_forc['vg'].data[it,ilev,0,0]

    if case.attributes['forc_w'] == 1:
        for it in range(0,nt_f): 
            print >>g,' W ',int(it*dt) ,'s'
            for ilev in range(0,nlev_out):
                print >>g, dataout_forc['w'].data[it,ilev,0,0]

    if case.attributes['surfaceForcing'] == 'surfaceFlux':
        print >>g, 'SURF.FORC'
        for it in range(0,nt_f): 
            print >>g,' FCS ',int(it*dt) ,'s'
            print >>g, case.variables['sfc_sens_flx'].data[it,0,0]
        for it in range(0,nt_f): 
            print >>g,' FLE ',int(it*dt) ,'s'
            print >>g, case.variables['sfc_lat_flx'].data[it,0,0]

        if case.attributes['surfaceForcingWind'] == 'ustar':
            for it in range(0,nt_f): 
                print >>g,' USTAR ',int(it*dt) ,'s'
                print >>g, case.variables['ustar'].data[it,0,0]


    for var in config.variablesAux.keys():
        print >>g, var
        if var == 'SURFZ0.FOIS.G' and attributes['z0']>0. :
            print >>g, str(9.80665*float(attributes['z0']))
        elif var == 'SURFGZ0.THERM'  and attributes['z0']>0. :
            print >>g, str(9.80665*float(attributes['z0'])/10.)
        else:
            print >>g, ' ' + str(config.variablesAux[var])

    print >>g, 'STOP'

    g.close()

#---------------------------------------------------------------
# Saving initial state and forcing in netCDF files
#---------------------------------------------------------------

if lsave_init:
    fileout = 'init_L'+ str(nlev_out) + '.nc'
    g = nc.Dataset(fileout,'w',format='NETCDF3_CLASSIC')
    for var in dataout.keys():
        dataout[var].write(g)

    g.close()

if lsave_forc:
    fileout = 'forc_L'+ str(nlev_out) + '.nc'
    g = nc.Dataset(fileout,'w',format='NETCDF3_CLASSIC')
    for var in dataout.keys():
        dataout[var].write(g)

    g.close()

