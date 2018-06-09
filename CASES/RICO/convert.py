import cdms2 
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

f = cdms2.open('rico_driver_MPL.nc')

data = {}
for var in f.listvariables():
  data[var] = f(var)

time = data['tadvh'].getTime()

lev = data['tadvh'].getAxis(1)
lev.designateLevel()

lat = data['tadvh'].getAxis(2)
lat.units = 'degrees_north'
lat.designateLatitude()

lon = data['tadvh'].getAxis(3)
lon.units = 'degrees_east'
lon.designateLongitude()




t0= MV2.array(range(0,1),typecode=MV2.float)
t0= cdms2.createAxis(t0)
#t0.designateTime()
t0.id = 't0'
t0.calendar = 'gregorian'
t0.units = "seconds since 2004-12-16 00:00:00"

data['w'].id = 'omega' 
data['w'].title = 'Vertical Pressure Velocity'

var = 'qadvh'
tmp = data[var]/1000.
for att in data[var].listattributes():
  tmp.setattribute(att, data[var].getattribute(att))

tmp.id = var
tmp.setAxis(0,time)
tmp.setAxis(1,lev)
tmp.setAxis(2,lat)
tmp.setAxis(3,lon)
data[var] = tmp


for var in ['temp','qv','u','v','height','pressure']:
  nlev,nlat,nlon = data[var].shape	
  tmp = data[var].reshape((1,nlev,nlat,nlon))
  for att in data[var].listattributes():
    tmp.setattribute(att, data[var].getattribute(att))

  tmp.id = var
  tmp.setAxis(0,t0)
  tmp.setAxis(1,lev)
  tmp.setAxis(2,lat)
  tmp.setAxis(3,lon)
  data[var] = tmp

g = cdms2.open('rico_driver_RR.nc','w')
for var in f.listvariables():
  if len(data[var].shape) == 4:	
    data[var].setAxis(1,lev)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
  elif len(data[var].shape) == 3:
    if data[var].shape == (nlev,nlat,nlon):
      data[var].setAxis(0,lev)
      data[var].setAxis(1,lat)
      data[var].setAxis(2,lon)
    else:
      data[var].setAxis(1,lat)
      data[var].setAxis(2,lon)
     		  
  g.write(data[var])

g.comment = 'Forcing and initial conditions for RICO case'
g.reference = 'http://projects.knmi.nl/rico/setup1d_composite.html'
g.author = 'R. Roehrig'


g.case = 'RICO'
g.startDate = '20041216000000'
g.endDate =   '20041218210000'

g.qadvh = 1
g.tadvh = 1
g.tadvv = 0
g.qadvv = 0
g.trad = 0 #1 ????

g.forc_omega = 1
g.forc_w = 0

g.forc_geo = 1

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0
g.nudging_q = 0

g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'
g.z0 = 0.1

g.close()

f.close()

