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
# Writing nam1D
#---------------------------------------------------------------

if lnam1D:

    g = open('nam1D_L' + str(nlev_out),'w')

    print >>g, '&NAM1D'
    print >>g, '  LMAP    = .FALSE.,'
    print >>g, '  IFLEV   = {0},'.format(int(nlev_out))
    print >>g, '  ZDELY   = 250000.,'
    print >>g, '  LNHDYN  = .FALSE.,'
    print >>g, '  LALAPHYS= .TRUE.,'
    print >>g, '  LREASUR = .TRUE.,'
    print >>g, '  NFORC   = 0,'
    print >>g, '  LQCGRP  = .TRUE.,'
    print >>g, '  LQIGRP  = .TRUE.,'
    print >>g, '  LQRGRP  = .FALSE.,'
    print >>g, '  LQSGRP  = .FALSE.,'
    print >>g, '  LQGGRP  = .FALSE.,'
    print >>g, '  LCFGRP  = .FALSE.,'
    print >>g, '  LSRCGRP = .FALSE.,'
    print >>g, '  LTKEGRP = .TRUE.,'
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
    print >>g, case.variables['ps'].data[0,0,0]

    print >>g, 'U'
    for ilev in range(0,nlev_out):
        print >>g, dataout['u'].data[0,ilev,0,0]

    print >>g, 'V'
    for ilev in range(0,nlev_out):
        print >>g, dataout['v'].data[0,ilev,0,0]

    print >>g, 'T'
    for ilev in range(0,nlev_out):
        print >>g, dataout['temp'].data[0,ilev,0,0]
# Pas completement satisfaisant a ce stade...
#    if pph[0,ilev] >= 10000.:
#      print >>g, data_out['theta'][0,ilev]*(pph[0,ilev]/100000.)**(2./7.)
#    else:
#      # Pour eviter des plantages en haute atmosphere
#      print >>g, data_out['temp'][0,ilev]

    print >>g, 'QV'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['qv'].data[0,ilev,0,0])

    print >>g, 'CLOUD_WATER'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['ql'].data[0,ilev,0,0])

    print >>g, 'ICE_CRYSTAL'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['qi'].data[0,ilev,0,0])

    print >>g, 'TKE'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['tke'].data[0,ilev,0,0])


    print >>g, 'FORCING'

    for var in config.variablesAux.keys():
        print >>g, var
        print >>g, ' ' + str(config.variablesAux[var])

    print >>g, 'STOP'

    g.close()

#---------------------------------------------------------------
# Saving initial state in netCDF file
#---------------------------------------------------------------

    if lsave_init:
        fileout = 'init_L'+ str(nlev_out) + '.nc'
        g = nc.Dataset(fileout,'w',format='NETCDF3_CLASSIC')
        for var in dataout.keys():
            dataout[var].write(g)

        g.close()

#---------------------------------------------------------------
# Ecriture des forcages ARPEGE-Climat
#---------------------------------------------------------------

dataout = {}

if lforc:

    dirout = 'files_L' + str(config.nlev) + '_' + str(int(config.dt)) + 's/' 

    names = {}
    names['ps_forc'] = 'Ps'

    names['ug'] = 'ug'
    names['vg'] = 'vg'

    names['w'] = 'W'
    names['omega'] = 'Omega'

    names['u_adv'] = 'du'
    names['v_adv'] = 'dv'
    names['temp_adv'] = 'dT'
    names['qv_adv'] = 'dq'

    names['u_nudging'] = 'u'
    names['v_nudging'] = 'v'
    names['temp_nudging'] = 'T'
    names['qv_nudging'] = 'q'    

    timein = case.variables['pressure_forc'].time
    tmin = timein.data[0]
    tmax = timein.data[-1]
    timeout = np.arange(tmin,tmax+config.dt,config.dt,dtype=np.float64)
    nt_out, = timeout.shape
    timeout = Axis('time',timeout,name='time',units=case.tunits)

    var = 'ps_forc'
    dataout[var] = interpol(case.variables[var],timeout=timeout)
    for ii in range(0,nt_out):
        g = open('{0}/{1}_forcing_{2:0>5}.txt'.format(dirout,names[var],ii),'w')
        print >>g,  dataout[var].data[ii,0,0]
        g.close()

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

        dout = interpol(din,levout=levout,timeout=timeout)
        if lverbose:
            dout.info()

        if lplot:
            din.plot(rep_images=rep_images['orig'], timeunits='hours',levunits='hPa')
            dout.plotcoef = case.variables[var].plotcoef
            dout.plotunits = case.variables[var].plotunits
            dout.plot(rep_images=rep_images['MUSC'],timeunits='hours',levunits='hPa')

        if lwrite:
            for ii in range(0,nt_out):
                g = open('{0}/{1}_forcing_{2:0>5}.txt'.format(dirout,names[var],ii),'w')
                for ilev in range(0,nlev_out):
                    print >>g,  dout.data[ii,ilev,0,0]

                g.close()

        return din, dout


    if attributes['forc_geo']:
        for var in ['ug','vg']:
            datain[var], dataout[var] = prep_forcing(var)

    if attributes['forc_omega']:
        var = 'omega'
        datain[var], dataout[var] = prep_forcing(var)  

    if attributes['forc_w']:
        var = 'w'
        datain[var], dataout[var] = prep_forcing(var)         

    for var in ['qv']: #['u','v','qv']:
        attloc = 'adv_{0}'.format(var)
        varloc = '{0}_adv'.format(var)
        if attributes[attloc]:
            datain[varloc], dataout[varloc] = prep_forcing(varloc)

    if attributes['adv_temp'] and (attributes['rad_temp'] == 1): 
        var1 = 'temp_adv'
        datain[var1], dataout[var1] = prep_forcing(var1,lwrite=False)
        var2 = 'temp_rad'
        datain[var2], dataout[var2] = prep_forcing(var2,lwrite=False)
        for ii in range(0,nt_out):
            g = open('{0}/{1}_forcing_{2:0>5}.txt'.format(dirout,names[var1],ii),'w')
            for ilev in range(0,nlev_out):
                print >>g,  dataout[var1].data[ii,ilev,0,0]+dataout[var2].data[ii,ilev,0,0]

            g.close()      
    elif attributes['adv_temp']: #attributes['rad_temp'] in [0,'adv']
        var = 'temp_adv'
        datain[var], dataout[var] = prep_forcing(var)

    for var in ['u','v','temp','qv']:
        attloc = 'nudging_{0}'.format(var)
        varloc = '{0}_nudging'.format(var)
        if attributes[attloc]:
            datain[var], dataout[var] = prep_forcing(var)

    if lsave_forc:
        fileout = 'forc_L'+ str(nlev_out) + '.nc'
        g = nc.Dataset(fileout,'w',format='NETCDF3_CLASSIC')
        for var in dataout.keys():
            dataout[var].write(g)

        g.close()

