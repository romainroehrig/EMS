import cdms2 
import MV2
import numpy as np

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

names = {}
units = {}

var = 'temp'
names[var] = 'Temperature'
units[var] = 'K'

var = 'pressure'
names[var] = 'Air Pressure'
units[var] = 'Pa'


var = 'zf'
names[var] = 'Height'
units[var] = 'm'

var = 'qv'
names[var] = 'Specific Humidity'
units[var] = 'kg kg-1'

var = 'u'
names[var] = 'Zonal Wind'
units[var] = 'm s-1'

var = 'v'
names[var] = 'Meridional Wind'
units[var] = 'm s-1'

var = 'w'
names[var] = 'Vertical Velocity'
units[var] = 'm s-1'

var = 'omega'
names[var] = 'Pressure Vertical Velocity'
units[var] = 'Pa s-1'

var = 'ps'
names[var] = 'Surface pressure'
units[var] = 'Pa'

var = 'ts'
names[var] = 'Surface Temperature'
units[var] = 'K'




f = cdms2.open('composite_slow_init.nc')

lat = MV2.array([25.,],typecode=MV2.float32)
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.array([-125.,],typecode=MV2.float32)
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

t0= MV2.array([0,],typecode=MV2.float32)
t0= cdms2.createAxis(t0)
#t0.designateTime()
t0.id = 't0'
t0.calendar = 'gregorian'
t0.units = "seconds since 2007-7-15 00:00:00"


tsec = f('tsec')

time = cdms2.createAxis(tsec)
time.designateTime()
time.id = 'time'
time.units = "seconds since 2007-7-15 00:00:00"
time.calendar = 'gregorian'

nt, = time.shape

data = {}

data['temp'] = f('T')

lev = data['temp'].getAxis(0)
lev.designateLevel()
lev.id = 'lev'
lev.units = 'Pa'
lev.long_name = 'Pressure'
nlev, = lev.shape

data['pressure'] = data['temp'].getAxis(0)[:]
data['zf'] = f('height')
data['qv'] = f('qt')
data['u'] = f('u')
data['v'] = f('v')
data['w'] = f('w')
data['omega'] = f('omega')

data['ps'] = MV2.array(np.tile(f('Ps'),nt))

data['ts'] = f('Tg')

for var in ['temp','pressure','zf','qv','u','v','w','omega']:
    data[var] = MV2.array(np.tile(data[var],(nt,1)))
    data[var] = MV2.reshape(data[var],(nt,nlev,1,1))
    data[var].setAxis(0,time)
    data[var].setAxis(1,lev)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
    data[var].id = var
    data[var].long_name = names[var]
    data[var].units = units[var]

for var in ['ts','ps']:
    data[var] = MV2.reshape(data[var],(nt,1,1))
    data[var].setAxis(0,time)
    data[var].setAxis(1,lat)
    data[var].setAxis(2,lon)
    data[var].id = var
    data[var].long_name = names[var]
    data[var].units = units[var]


g = cdms2.open('Composite_SLOW_driver_RR.nc','w')

for var in data.keys():
  g.write(data[var])

g.comment = 'Forcing and initial conditions for I. SANDU Composite cases - SLOW'
g.reference = '??'
g.author = 'R. Roehrig'


g.case = 'SANDU/SLOW'
g.startDate = '20070715000000'
g.endDate = '20070718000000'

g.qadvh = 0
g.tadvh = 0
g.tadvv = 0
g.qadvv = 0
g.trad = 0 #1 ????

g.forc_omega = 0
g.forc_w = 1

g.forc_geo = 0

g.nudging_u = 3600
g.nudging_v = 3600
g.nudging_t = 3600
g.nudging_q = 3600

g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 70000
g.p_nudging_q = 70000

g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'
g.z0 = 0.01

g.close()
