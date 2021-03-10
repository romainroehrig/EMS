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

vert_grid = config.vert_grid

#---------------------------------------------------------------
# Reading case information
#---------------------------------------------------------------

case = Case('{0}/{1}'.format(config.case,config.subcase))
case.read('data_input.nc')

if lverbose:
    case.info()

attributes = case.attributes

lat = case.variables['lat'].data[0]
lon = case.variables['lon'].data[0]

zorog = case.variables['orog'].data[0]

startDate = case.start_date
year = startDate.year
month = startDate.month
day = startDate.day
hour = startDate.hour
minute = startDate.minute
second = startDate.second

nt, = case.variables['ps_forc'].data.shape

#---------------------------------------------------------------
# Half-level pressure
#---------------------------------------------------------------

f = open('{0}.dta'.format(vert_grid))
lines = f.readlines()
nlev_out = len(lines)-1

vah = np.zeros((nlev_out+1),dtype=np.float)
vbh = np.zeros((nlev_out+1),dtype=np.float)

for ilev in range(0,nlev_out+1):
    line = lines[ilev].split()
    vah[ilev] = float(line[0])
    vbh[ilev] = float(line[1])

f.close()

pph = np.zeros((nt,nlev_out+1),dtype=np.float)

for ilev in range(0,nlev_out+1):
    pph[:,ilev] = vah[ilev] + vbh[ilev]*case.variables['ps_forc'].data[:]	  

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

levin = Axis('lev',case.variables['pa'].data[0,:],name='pa',units='Pa')
levout = Axis('lev',ppf[0,:],name='pa',units='Pa')

datain = {}
dataout = {}
for var in ['ua','va','ta','theta','qv','ql','qi','tke']:
    datain[var] = Variable(var,
            data=case.variables[var].data,
            units=case.variables[var].units,
            name=case.variables[var].name,
            level=levin,
            time=case.variables[var].time,
            pressure=case.variables['pa'].data,
            plotcoef=case.variables[var].plotcoef,
            plotunits=case.variables[var].plotunits)
    if lverbose:
        datain[var].info()

    dataout[var] = datain[var].interpol_vert(pressure=levout.data)
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

    g = open('nam1D_{0}'.format(vert_grid),'w')

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
    print >>g, case.variables['ps'].data[0]

    print >>g, 'U'
    for ilev in range(0,nlev_out):
        print >>g, dataout['ua'].data[0,ilev]

    print >>g, 'V'
    for ilev in range(0,nlev_out):
        print >>g, dataout['va'].data[0,ilev]

    print >>g, 'T'
    for ilev in range(0,nlev_out):
        print >>g, dataout['ta'].data[0,ilev]
# Pas completement satisfaisant a ce stade...
#    if pph[0,ilev] >= 10000.:
#      print >>g, data_out['theta'][0,ilev]*(pph[0,ilev]/100000.)**(2./7.)
#    else:
#      # Pour eviter des plantages en haute atmosphere
#      print >>g, data_out['temp'][0,ilev]

    print >>g, 'QV'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['qv'].data[0,ilev])

    print >>g, 'CLOUD_WATER'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['ql'].data[0,ilev])

    print >>g, 'ICE_CRYSTAL'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['qi'].data[0,ilev])

    print >>g, 'TKE'
    for ilev in range(0,nlev_out):
        print >>g, max(0.,dataout['tke'].data[0,ilev])


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
        fileout = 'init_{0}.nc'.format(vert_grid)
        g = nc.Dataset(fileout,'w',format='NETCDF3_CLASSIC')
        for var in dataout.keys():
            dataout[var].write(g)

        g.close()

#---------------------------------------------------------------
# Ecriture des forcages ARPEGE-Climat
#---------------------------------------------------------------

dataout = {}

if lforc:

    dirout = 'files_{0}_{1}s/'.format(vert_grid,int(config.dt))

    names = {}
    names['ps_forc'] = 'Ps'

    names['ug'] = 'ug'
    names['vg'] = 'vg'

    names['wa'] = 'W'
    names['wap'] = 'Omega'

    names['tnua_adv'] = 'du'
    names['tnva_adv'] = 'dv'
    names['tnta_adv'] = 'dT'
    names['tnqv_adv'] = 'dq'

    names['ua_nud'] = 'u'
    names['va_nud'] = 'v'
    names['ta_nud'] = 'T'
    names['qv_nud'] = 'q'    

    timein = case.variables['pa_forc'].time
    tmin = timein.data[0]
    tmax = timein.data[-1]
    timeout = np.arange(tmin,tmax+config.dt,config.dt,dtype=np.float64)
    nt_out, = timeout.shape
    timeout = Axis('time',timeout,name='time',units=case.tunits)

    var = 'ps_forc'
    dataout[var] = case.variables[var].interpol_time(time=timeout)
    for ii in range(0,nt_out):
        g = open('{0}/{1}_forcing_{2:0>5}.txt'.format(dirout,names[var],ii),'w')
        print >>g,  dataout[var].data[ii]
        g.close()

    def prep_forcing(var,lwrite=True):
        din = Variable(var,
                data=case.variables[var].data,
                units=case.variables[var].units,
                name=case.variables[var].name,
                level=levin,
                time=case.variables[var].time,
                pressure=case.variables['pa_forc'],
                plotcoef=case.variables[var].plotcoef,
                plotunits=case.variables[var].plotunits)
        if lverbose:
            din.info()

        dout = din.interpol_time(time=timeout)
        dout = dout.interpol_vert(pressure=levout.data)
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
                    print >>g,  dout.data[ii,ilev]

                g.close()

        return din, dout


    if attributes['forc_geo']:
        for var in ['ug','vg']:
            datain[var], dataout[var] = prep_forcing(var)

    if attributes['forc_wap']:
        var = 'wap'
        datain[var], dataout[var] = prep_forcing(var)  

    if attributes['forc_wa']:
        var = 'wa'
        datain[var], dataout[var] = prep_forcing(var)         

    for var in ['qv']: #['u','v','qv']:
        attloc = 'adv_{0}'.format(var)
        varloc = 'tn{0}_adv'.format(var)
        if attributes[attloc]:
            datain[varloc], dataout[varloc] = prep_forcing(varloc)

    if attributes['adv_ta'] and (attributes['radiation'] == 'tend'): 
        var1 = 'tnta_adv'
        datain[var1], dataout[var1] = prep_forcing(var1,lwrite=False)
        var2 = 'tnta_rad'
        datain[var2], dataout[var2] = prep_forcing(var2,lwrite=False)
        for ii in range(0,nt_out):
            g = open('{0}/{1}_forcing_{2:0>5}.txt'.format(dirout,names[var1],ii),'w')
            for ilev in range(0,nlev_out):
                print >>g,  dataout[var1].data[ii,ilev]+dataout[var2].data[ii,ilev]

            g.close()      
    elif attributes['adv_ta']: #attributes['rad_temp'] in [0,'adv']
        var = 'tnta_adv'
        datain[var], dataout[var] = prep_forcing(var)

    for var in ['ua','va','ta','qv']:
        attloc = 'nudging_{0}'.format(var)
        varloc = '{0}_nud'.format(var)
        if attributes[attloc]:
            datain[varloc], dataout[varloc] = prep_forcing(varloc)

    if lsave_forc:
        fileout = 'forc_{0}.nc'.format(vert_grid)
        g = nc.Dataset(fileout,'w',format='NETCDF3_CLASSIC')
        for var in dataout.keys():
            dataout[var].write(g)

        g.close()

