import cdms2 
import MV2
import math

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

datain = {}

f = cdms2.open('ARMCu_driver_RR.nc')
for var in ['u','v','temp','qv','ug','vg','sfc_sens_flx','sfc_lat_flx','ts','ps','tadv','qadv','pressure']:
  datain[var] = f(var)

f.close()

nt, nlev, nlat, nlon = datain['temp'].shape
time = datain['temp'].getAxis(0)
lev = datain['temp'].getAxis(1)
lev.id = 'lev'
lat = datain['temp'].getAxis(2)
lon = datain['temp'].getAxis(3)

t0 = MV2.array([0,],typecode=MV2.float)
t0 = cdms2.createAxis(t0)
t0.id = 't0'
t0.units = 'seconds since 1997-06-21 11:30:0.0'
t0.calendar = 'gregorian'

time[:] = time[:] - time[0]
time.units = 'seconds since 1997-06-21 11:30:0.0'
del(time.realtopology)

dataout = {}
for var in ['u','v','temp','qv','pressure']:
  dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
  dataout[var][0,:,0,0] = datain[var][0,:,0,0]
  dataout[var].id = var
  dataout[var].title = datain[var].title
  dataout[var].units = datain[var].units
  dataout[var].setAxis(0,t0)
  dataout[var].setAxis(1,lev)
  dataout[var].setAxis(2,lat)
  dataout[var].setAxis(3,lon)

var = 'height'
dataout[var] = MV2.zeros((1,nlev,nlat,nlon),typecode=MV2.float)
dataout[var][0,:,0,0] = lev[:]
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



for var in ['ug','vg','sfc_sens_flx','sfc_lat_flx','ts','ps','tadv']:
    dataout[var] = datain[var]
    dataout[var].setAxis(0,time)
    try:
      del(dataout[var].positive)
    except:
      pass

var = 'qvadv'
dataout[var] = datain['qadv']*1.
dataout[var].id = var
dataout[var].title = 'Specific humidity advection'
dataout[var].units = 'kg kg-1 s-1'
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


g = cdms2.open('ARMCu_driver_RR_new.nc','w')

for var in dataout.keys():
    g.write(dataout[var])

g.comment = "Forcing and initial conditions for ARMCu case" 
g.reference = "http://projects.knmi.nl/eurocs/ARM/case_ARM_html" 
g.author = "M.-P. Lefebvre, R. Roehrig" 
g.modifications = "2017-04-28: R. Roehrig - compute tadv + add ps + update forcing information\n\
        2019-03-21: R. Roehrig - new DEPHY format" 
g.case = "ARMCU/REF" 
g.startDate = "19970621113000" 
g.endDate = "19970622023000" 
g.tadv = 1 
g.tadvh = 0 
g.tadvv = 0 
g.trad = "adv"
g.qvadv = 1 
g.qvadvh = 0 
g.qvadvv = 0 
g.forc_omega = 0 
g.forc_w = 0 
g.forc_geo = 1 
g.nudging_u = 0 
g.nudging_v = 0 
g.nudging_t = 0 
g.nudging_q = 0 
g.zorog = 0. 
g.z0 = 0.035 
g.surfaceType = "land" 
g.surfaceForcing = "surfaceFlux" 
g.surfaceForcingWind = "z0" 

g.close()
