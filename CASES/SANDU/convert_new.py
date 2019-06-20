import cdms2 
import MV2
import math

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

datain = {}

f = cdms2.open('Composite_REF_driver_RR.nc')
for var in ['u','v','temp','qv','ts','ps','w','pressure','zf']:
  datain[var] = f(var)

f.close()

nt, nlev, nlat, nlon = datain['temp'].shape
time = datain['temp'].getAxis(0)
lev = datain['temp'].getAxis(1)
lat = datain['temp'].getAxis(2)
lon = datain['temp'].getAxis(3)

t0 = MV2.array([0,],typecode=MV2.float)
t0 = cdms2.createAxis(t0)
t0.id = 't0'
t0.units = 'seconds since 2007-7-15 00:00:0.0'
t0.calendar = 'gregorian'

time[:] = time[:] - time[0]
time.units = 'seconds since 2007-7-15 00:00:0.0'

dataout = {}
for var in ['u','v','temp','qv','pressure']:
  dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
  dataout[var][0,:,0,0] = datain[var][0,:,0,0]
  dataout[var].id = var
  dataout[var].title = datain[var].long_name
  dataout[var].units = datain[var].units
  dataout[var].setAxis(0,t0)
  dataout[var].setAxis(1,lev)
  dataout[var].setAxis(2,lat)
  dataout[var].setAxis(3,lon)

var = 'height'
dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
dataout[var][0,:,0,0] = datain['zf'][0,:,0,0]
dataout[var].id = var
dataout[var].title = 'Height above the surface'
dataout[var].units = 'm'
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



for var in ['ts','ps','w']:
    dataout[var] = datain[var]
    dataout[var].setAxis(0,time)
    try:
      del(dataout[var].positive)
    except:
      pass

var = 'u_nudg'
dataout[var] = datain['u']*1.
dataout[var].id = var
dataout[var].title = 'Zonal wind for nudging'
dataout[var].units = 'm s-1'
dataout[var].setAxis(0,time)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)

var = 'v_nudg'
dataout[var] = datain['v']*1.
dataout[var].id = var
dataout[var].title = 'Meridional wind for nudging'
dataout[var].units = 'm s-1'
dataout[var].setAxis(0,time)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)

var = 'temp_nudg'
dataout[var] = datain['temp']*1.
dataout[var].id = var
dataout[var].title = 'Temperature for nudging'
dataout[var].units = 'K'
dataout[var].setAxis(0,time)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)

var = 'qv_nudg'
dataout[var] = datain['qv']*1.
dataout[var].id = var
dataout[var].title = 'Specific humidity for nudging'
dataout[var].units = 'kg kg-1'
dataout[var].setAxis(0,time)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)


 
var = 'pressure_forc'
dataout[var] = datain['pressure']*1.
dataout[var].id = var
dataout[var].title = 'Pressure'
dataout[var].units = 'Pa'
dataout[var].setAxis(0,time)
dataout[var].setAxis(1,lev)
dataout[var].setAxis(2,lat)
dataout[var].setAxis(3,lon)


g = cdms2.open('Composite_REF_driver_RR_new.nc','w')

for var in dataout.keys():
    g.write(dataout[var])

g.comment = "Forcing and initial conditions for I. SANDU Composite cases - REF" 
g.reference = "??" 
g.author = "R. Roehrig" 
g.modifications = "2019-06-20: R. Roehrig - new DEPHY format" 
g.case = "SANDU/REF" 
g.startDate = "20070715000000" 
g.endDate = "20070718000000" 
g.tadv = 0 
g.tadvh = 0 
g.tadvv = 0 
g.trad = 0
g.qvadv = 0 
g.qvadvh = 0 
g.qvadvv = 0 
g.forc_omega = 0 
g.forc_w = 1 
g.forc_geo = 0 
g.nudging_u = 3600 
g.nudging_v = 3600
g.nudging_t = 3600
g.nudging_qv = 3600
g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 70000
g.p_nudging_qv = 70000
g.zorog = 0. 
g.z0 = 0.01
g.surfaceType = "ocean" 
g.surfaceForcing = "ts" 

g.close()
