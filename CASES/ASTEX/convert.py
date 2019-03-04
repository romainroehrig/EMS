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

var = 'theta'
names[var] = 'Potential temperature'
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

var = 'ql'
names[var] = 'Liquid water content'
units[var] = 'kg kg-1'

var = 'qt'
names[var] = 'Total water content'
units[var] = 'kg kg-1'

var = 'u'
names[var] = 'Zonal Wind'
units[var] = 'm s-1'

var = 'v'
names[var] = 'Meridional Wind'
units[var] = 'm s-1'

var = 'tke'
names[var] = 'Turbulent Kinetic Energy'
units[var] = 'm2 s-2'

var = 'ug'
names[var] = 'Geostrophic Zonal Wind'
units[var] = 'm s-1'

var = 'vg'
names[var] = 'Geostrophic Meridional Wind'
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




f = cdms2.open('astex_input_v5.nc')

lat = MV2.array([34.,],typecode=MV2.float32)
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.array([-25.,],typecode=MV2.float32)
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

t0= MV2.array([0,],typecode=MV2.float32)
t0= cdms2.createAxis(t0)
#t0.designateTime()
t0.id = 't0'
t0.calendar = 'gregorian'
t0.units = "seconds since 1992-06-13 00:00:00"


tsec = f('tsec')

time = cdms2.createAxis(tsec)
time.designateTime()
time.id = 'time'
time.units = "seconds since 1992-06-13 00:00:00"
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
data['theta'] = f('theta')
data['tke'] = f('tke')
data['zf'] = f('height')
data['qv'] = f('qv')
data['ql'] = f('ql')
data['qt'] = f('qt')
data['u'] = f('u')
data['v'] = f('v')
data['ug'] = f('ug')
data['vg'] = f('vg')
data['w'] = f('w')

#data['ps'] = MV2.array(np.tile(f('Ps'),nt))

data['ts'] = f('Tg')
data['ps'] = f('Ps')

for var in ['temp','pressure','zf','qv','ql','qt','tke','u','v']:
#    data[var] = MV2.array(np.tile(data[var],(nt,1)))
#    data[var] = MV2.reshape(data[var],(nt,nlev,1,1))
    data[var] = MV2.reshape(data[var],(1,nlev,1,1))
    data[var].setAxis(0,t0)
    data[var].setAxis(1,lev)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
    data[var].id = var
    data[var].long_name = names[var]
    data[var].units = units[var]

for var in ['ug','vg']:
    data[var] = MV2.transpose(MV2.array(np.tile(data[var],(nlev,1))))
    data[var] = MV2.reshape(data[var],(nt,nlev,1,1))
    data[var].setAxis(0,time)
    data[var].setAxis(1,lev)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
    data[var].id = var
    data[var].long_name = names[var]
    data[var].units = units[var]

for var in ['w']:
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


g = cdms2.open('ASTEX_GASS-EUCLIPSE_driver_RR.nc','w')

for var in data.keys():
  g.write(data[var])

g.comment = 'Forcing and initial conditions for GASS/EUCLIPSE ASTEX cases'
g.reference = '??'
g.author = 'R. Roehrig'


g.case = 'ASTEX/EUCLIPSE'
g.startDate = '19920613000000'
g.endDate = '19920614160000'

g.qadvh = 0
g.tadvh = 0
g.tadvv = 0
g.qadvv = 0
g.trad = 0 #1 ????

g.forc_omega = 0
g.forc_w = 1

g.forc_geo = 1

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0
g.nudging_q = 0

#g.p_nudging_u = 110000
#g.p_nudging_v = 110000
#g.p_nudging_t = 70000
#g.p_nudging_q = 70000

g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'
g.z0 = 0.01

g.close()
