import cdms2, MV2
import cdtime
import math

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

var3d = ['theta','temp','qv','rv','u','v','zf','pressure']
var2d = ['ps','ts']

variables = var3d+var2d

lat = [0.1,]
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon =[80.5,]
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

nt = 24
nlev = 52


data = {}
for var in var3d:
  data[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float)

for var in var2d:
  data[var] = MV2.zeros((nt,1,1),typecode=MV2.float)

data['ps'][:,0,0] = 101039

time = MV2.array(range(0,nt*3600,3600),typecode=MV2.float)
time = cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.units = "second since 2011-11-13 02:00:0.0"

f = open('z_u_v')
lines = f.readlines()
f.close()
for i in range(1,nlev):
    tmp = lines[i-1].split()
    data['zf'][:,i,0,0] = float(tmp[0])
    data['u'][:,i,0,0] = float(tmp[1])
    data['v'][:,i,0,0] = float(tmp[2])

data['theta'][:,0,0,0] = 301.10
data['rv'][:,0,0,0] = 1.83e-2

f = open('z_th_rv')
lines = f.readlines()
f.close()
for i in range(1,nlev):
    tmp = lines[i-1].split()
    data['theta'][:,i,0,0] = float(tmp[1])
    data['rv'][:,i,0,0] = float(tmp[2])

data['qv'] = data['rv']/(1. + data['rv'])

level = cdms2.createAxis(data['zf'][0,:,0,0])
level.designateLevel()
level.id = 'level'
level.name = 'altitude'
level.units = 'm'

f = open('ts')
lines = f.readlines()
f.close()
for i in range(0,nt):
    tmp = lines[i].split()[0]
    data['ts'][i,0,0] = float(tmp.split('=')[1])

kappa = 2./7. #0.286 #2./7.
p0 = 100000.
g = 9.80665 #9.81
R = 287.0596736665907 #287 

integ = 0.

data['pressure'][:,0,0,0] = data['pressure'][:,0,0,0] + data['ps'][0,0,0]

for ilev in range(1,nlev):
    dz = data['zf'][0,ilev,0,0]- data['zf'][0,ilev-1,0,0]    
#    print 'dz =', dz
    integ = integ + (g/(R*data['theta'][0,ilev-1,0,0])+g/(R*data['theta'][0,ilev,0,0]))/2*dz
#  print 'integ =', integ
    tmp = data['ps'][0,0,0]**kappa-p0**kappa*kappa*integ
    data['pressure'][0,ilev,0,0] = math.exp(math.log(tmp)/kappa)

for ilev in range(0,nlev):
    data['temp'][:,ilev,0,0] = data['theta'][:,ilev,0,0]*(data['pressure'][0,ilev,0,0]/p0)**kappa


####
for var in var3d:
    data[var].id = var
    data[var].setAxis(0,time)
    data[var].setAxis(1,level)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)

for var in var2d:
    data[var].id = var
    data[var].setAxis(0,time)
    data[var].setAxis(1,lat)
    data[var].setAxis(2,lon)


g = cdms2.open('CINDY-DYNAMO_Revelle-PE-13Nov_driver_RR.nc','w')

for var in variables:
  print var
  g.write(data[var])

g.comment = 'Initial conditions for CINDY-DYNAMO Revelle souding, no forcing'
g.reference = 'TBD'
g.author = 'R. Roehrig'


g.case = 'CINDY-DYNAMO - Revelle PE turb+rad'
g.startDate = '20111113020000'
g.endDate = '20111114010000'

g.qadvh = 0
g.tadvh = 0
g.tadvv = 0
g.qadvv = 0
g.trad = 0

g.forc_omega = 0
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0
g.nudging_q = 0


g.zorog = 0.
g.z0 = 0.1
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'


g.close()
f.close()


