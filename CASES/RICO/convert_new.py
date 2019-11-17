import cdms2 
import MV2
import math
import os

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

datain = {}

f = cdms2.open('rico_driver_RR.nc')
for var in ['u','v','temp','tke','qv','height','ug','vg','ts','ps','tadvh','qadvh','pressure','omega']:
  datain[var] = f(var)
  try:
    del(datain[var].positive)
  except:
    pass


f.close()

nt, nlev, nlat, nlon = datain['ug'].shape
time = datain['ug'].getAxis(0)
lev = datain['ug'].getAxis(1)
lev.id = 'lev'
lat = datain['ug'].getAxis(2)
lon = datain['ug'].getAxis(3)

t0 = MV2.array([0,],typecode=MV2.float)
t0 = cdms2.createAxis(t0)
t0.id = 't0'
t0.units = 'seconds since 2004-12-16 00:00:0.0'
t0.calendar = 'gregorian'

time[:] = time[:] - time[0]
time.units = 'seconds since 2004-12-16 00:00:0.0'
#del(time.realtopology)

title = {}
title['u'] = 'Zonal wind'
title['v'] = 'Meridional wind'
title['temp'] = 'Temperature'
title['qv'] = 'Specific humidity'
title['tke'] = 'Turbulent kinetic energy'
title['pressure'] = 'Pressure'
title['height'] = 'Height above the surface'

dataout = {}
for var in ['u','v','temp','qv','tke','pressure','height']:
  dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
  dataout[var][0,:,0,0] = datain[var][0,:,0,0]
  dataout[var].id = var
  dataout[var].title = title[var] #datain[var].title
  dataout[var].units = datain[var].units
  dataout[var].setAxis(0,t0)
  dataout[var].setAxis(1,lev)
  dataout[var].setAxis(2,lat)
  dataout[var].setAxis(3,lon)

var = 'ql'
dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
dataout[var].id = var
dataout[var].title = 'Liquid water content'
dataout[var].units = 'kg kg-1'
dataout[var].setAxis(0,t0)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)

var = 'qi'
dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
dataout[var].id = var
dataout[var].title = 'Ice water content'
dataout[var].units = 'kg kg-1'
dataout[var].setAxis(0,t0)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)

var = 'tke'
dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
dataout[var].id = var
dataout[var].title = 'Turbulent kinetic energy'
dataout[var].units = 'm2 s-2'
dataout[var].setAxis(0,t0)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)

title['tadvh'] = 'Horizontal advection of temperature'

for var in ['ug','vg','ts','ps','tadvh','omega']:
    dataout[var] = datain[var]
    dataout[var].setAxis(0,time)
    if title.has_key(var):
        dataout[var].title = title[var]
    if var in ['ug','vg','tadvh']:
        dataout[var].setAxis(1,lev)
    try:
      del(dataout[var].positive)
    except:
      pass

var = 'qvadvh'
dataout[var] = datain['qadvh']*1.
dataout[var].id = var
dataout[var].title = 'Horizontal advection of specific humidity advection'
dataout[var].units = 'kg kg-1 s-1'
dataout[var].setAxis(0,time)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)
 
var = 'pressure_forc'
dataout[var] =  MV2.zeros((nt,nlev,nlat,nlon),typecode=MV2.float)
for it in range(0,nt):
    dataout[var][it,:,0,0] = datain['pressure'][0,:,0,0]
dataout[var].id = var
dataout[var].title = 'Forcing pressure'
dataout[var].units = 'Pa'
dataout[var].setAxis(0,time)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)


g = cdms2.open('rico_driver_RR_new.nc','w')

for var in ['pressure','height','u','v','temp','qv','ql','qi','tke','pressure_forc','ug','vg','tadvh','qvadvh','omega','ts','ps']: #dataout.keys():
    g.write(dataout[var])

g.comment = "Forcing and initial conditions for RICO case" 
g.reference = "http://projects.knmi.nl/rico/setup1d_composite.html" 
g.author = "R. Roehrig" 
g.modifications = "2019-10-01: R. Roehrig - new DEPHY format" 
g.case = "RICO/REF" 
g.startDate = "20041216000000"
g.endDate = "20041219000000"
g.tadv = 0 
g.tadvh = 1 
g.tadvv = 0 
g.trad = "adv"
g.qvadv = 0 
g.qvadvh = 1 
g.qvadvv = 0 
g.forc_omega = 1
g.forc_w = 0 
g.forc_geo = 1 
g.nudging_u = 0 
g.nudging_v = 0 
g.nudging_t = 0 
g.nudging_q = 0 
g.zorog = 0. 
g.z0 = 0.1 
g.surfaceType = "ocean" 
g.surfaceForcing = "ts" 
g.surfaceForcingWind = "z0" 

g.close()

os.system('nc4tonc3 -o {0} {1}'.format('rico_driver_RR_new.nc','rico_driver_RR_new3.nc'))
