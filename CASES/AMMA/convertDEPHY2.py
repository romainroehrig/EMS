# -*- coding:UTF-8 -*-

import cdms2 
import MV2
import cdtime

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

f = cdms2.open('AMMA_20060710_setupI.nc')

data = {}
for var in f.listvariables():
  data[var] = f(var)

time = data['temp'].getTime()
lev = data['temp'].getLevel()
lat = data['temp'].getLatitude()
lon = data['temp'].getLongitude()

varnames = {}
for var in ['temp','qv','u','v','tadvh','qadvh','w','ps','pressure','uadvh','vadvh','sfc_sens_flx','sfc_lat_flx']:
  varnames[var] = var

varnames['pressure'] = 'pp'
varnames['tadvh'] = 'hT'
varnames['qadvh'] = 'hq'
varnames['uadvh'] = 'hu'
varnames['vadvh'] = 'hv'
varnames['ps'] = 'pps'
varnames['sfc_sens_flx'] = 'hfss'
varnames['sfc_lat_flx'] = 'hfls'

names = {}
names['temp'] = 'Temperature'
names['qv'] = 'Specific Humidity'
names['u'] = 'Zonal Wind'
names['v'] = 'Meridional Wind'
names['tadvh'] = 'Horizontal Temperature Advection'
names['qadvh'] = 'Horizontal Specific Humidity Advection'
names['uadvh'] = 'Horizontal Temperature Advection'
names['vadvh'] = 'Horizontal Specific Humidity Advection'
names['w'] = 'Vertical Velocity'
names['ps'] = 'Surface Pressure'
names['pressure'] = 'Pressure'
names['sfc_sens_flx'] = 'Surface sensible heat flux (positive downward)'
names['sfc_lat_flx'] = 'Surface latent heat flux (positive downward)'
names['ts'] = 'Surface temperature'

g = cdms2.open('AMMA_20060710_setupI_driver_RR.nc','w')
for var in varnames.keys():
  if len(data[varnames[var]].shape) == 4:	
    data[varnames[var]].setAxis(1,lev)
    data[varnames[var]].setAxis(2,lat)
    data[varnames[var]].setAxis(3,lon)
  elif len(data[varnames[var]].shape) == 3:
    data[varnames[var]].setAxis(1,lat)
    data[varnames[var]].setAxis(2,lon)

  if var in ['pressure','ps']:
    data[varnames[var]] = data[varnames[var]]*100.
    data[varnames[var]].units = 'Pa'

  if var in ['hfls','hfss']:
    data[varnames[var]] = data[varnames[var]]*-1.

  data[varnames[var]].id = var		  
  data[varnames[var]].title = names[var]
  data[varnames[var]].positive = ''
  g.write(data[varnames[var]])

var = 'ts'
data[var] = data['hfss']*0. 
f = cdms2.open('nimmetS1.b1.20060710.000000.cdf')
time = data[var].getAxis(0)
nt,nlat,nlon = data[var].shape
for it in range(0,nt-1):
    tt = cdtime.reltime(time[it],time.units)
    data[var][it,0,0] = f('temp_mean', time = tt) + 273.15
tt = cdtime.comptime(2006,7,10,0,0,0)
data[var][nt-1,0,0] = f('temp_mean', time = tt) + 273.15
f.close()
data[var].id = var
data[var].title = names[var]
data[var].positive = ''
g.write(data[var])

g.comment = 'Forcing and initial conditions for 10 July 2006 AMMA Case - Setup I'
g.reference =\
""" Couvreux F, C. Rio, F. Guichard, M. Lothon, G. Canut, D. Bouniol, A. Gounou, 2012 : Initiation of daytime local convection in a semi-arid region analyzed with Large-Eddy Simulations and AMMA observations. Quarterly Journal of the Royal Meteorological Society, 138, 56-71.

Couvreux F, R Roehrig, C Rio, MP Lefebvre, M Caian, T Komori, S Derbyshire, F Guichard, F Favot, F D’andrea, P Bechtold, P Gentine, 2015 : Daytime moist convection over the semi-arid Tropics : impact of parametrizations used in CMIP5 and other models. Quarterly Journal of the Royal Meteorological Society, 141, 2220-2236."""
g.author = 'R. Roehrig'


g.case = '10 July 2006 AMMA Case - Setup I'
g.startDate = '20060710060000'
g.endDate = '20060711000000'

g.qadvh = 1
g.tadvh = 1
g.tadvv = 0
g.qadvv = 0
g.uadvh = 0
g.vadvh = 0
g.uadvv = 0
g.vadvv = 0

g.trad = 'adv'

g.forc_omega = 0
g.forc_w = 1

g.forc_geo = 0

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0
g.nudging_q = 0

g.zorog = 0.
g.z0 = 0.01
g.ustar = 0.
g.surfaceType = 'land'
g.surfaceForcing = 'surfaceFlux'
g.surfaceForcingWind = 'z0'

g.close()

f.close()


