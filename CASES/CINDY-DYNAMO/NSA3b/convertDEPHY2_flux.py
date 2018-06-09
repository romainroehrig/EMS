import cdms2 
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

f = cdms2.open('cindy_nsa3b_extended.nc')

data = {}
for var in f.listvariables():
  data[var] = f(var)

time = data['temp'].getTime()
lev = data['temp'].getLevel()
lat = data['temp'].getLatitude()
lon = data['temp'].getLongitude()

varnames = {}
for var in ['temp','qv','u','v','tadvh','qadvh','omega','ps','pressure','ts','sfc_lat_flx','sfc_sens_flx']:
  varnames[var] = var

varnames['pressure'] = 'pp'
varnames['tadvh'] = 'hT'
varnames['qadvh'] = 'hq'
varnames['ps'] = 'pps'
varnames['sfc_lat_flx'] = 'flat'
varnames['sfc_sens_flx'] = 'sens'

names = {}
names['temp'] = 'Temperature'
names['qv'] = 'Specific Humidity'
names['u'] = 'Zonal Wind'
names['v'] = 'Meridional Wind'
names['tadvh'] = 'Horizontal Temperature Advection'
names['qadvh'] = 'Horizontal Specific Humidity Advection'
names['omega'] = 'Vertical Pressure Velocity'
names['ps'] = 'Surface Pressure'
names['pressure'] = 'Pressure'
names['ts'] = 'Surface Temperature'
names['sfc_lat_flx'] = 'Surface latent heat flux'
names['sfc_sens_flx'] = 'Surface sensible heat flux'


g = cdms2.open('cindy-dynamo-NSA3bflux_driver_RR.nc','w')
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
     		  
  data[varnames[var]].id = var		  
  data[varnames[var]].title = names[var]
  data[varnames[var]].positive = ''
  g.write(data[varnames[var]])

g.comment = 'Forcing and initial conditions for CINDY-DYNAMO NSA3b case - flux forcing'
g.reference = 'http://johnson.atmos.colostate.edu/dynamo/products/array_averages/index.html'
g.author = 'R. Roehrig'


g.case = 'CINDY-DYNAMO - NSA3b - flux forcing'
g.startDate = '20111001000000'
g.endDate = '20111231210000'

g.qadvh = 1
g.tadvh = 1
g.tadvv = 0
g.qadvv = 0
g.trad = 0

g.forc_omega = 1
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 10800
g.nudging_v = 10800
g.nudging_t = 10800
g.nudging_q = 10800

g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 10000
g.p_nudging_q = 10000


g.zorog = 0.
g.z0 = 0.01
g.ustar = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'surfaceFlux'

g.close()

f.close()



