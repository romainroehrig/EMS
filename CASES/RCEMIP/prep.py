import MV2, cdms2
import cdtime

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value) 
cdms2.setNetcdfDeflateLevelFlag(value)

q0 = {}
q0[295] = 12.00/1000.
q0[300] = 18.65/1000.
q0[305] = 24.00/1000.

ps = 101480

def q(z,SST=300,qt=1.e-14,zq1=4000.,zq2=7500.,zt=15000.):
  tmp = q0[SST]*MV2.exp(-z/zq1)*MV2.exp(-z*z/(zq2*zq2))
  if isinstance(tmp,float):
    if z > zt:
        tmp = qt
  else:
    tmp = MV2.where(z > zt, qt, tmp)
  return tmp

def Tv(z,SST=300.,zt=15000.,gamma=0.0067):
  Tv0 = SST*(1+0.608*q(0,SST=SST))
  tmp = Tv0 - gamma*z
  Tvt = Tv0 - gamma*zt
  if isinstance(tmp,float):
    if z > zt:
        tmp = Tvt
  else:
    tmp = MV2.where(z > zt, Tvt, tmp)
  return tmp

def T(z,SST=300):
  tmp = Tv(z,SST=SST)/(1.+0.608*q(z,SST=SST))
  return tmp

def p(z,SST=300,zt=15000.,p0=ps,gamma=0.0067,g=9.79764,Rd=287.04):
  Tv0 = Tv(0,SST=SST)
  Tvt = Tv(zt,SST=SST)
  tmp1 = p0*MV2.exp(g/(Rd*gamma)*MV2.log((Tv0-gamma*z)/Tv0))
  pt = p0*(Tvt/Tv0)**(g/(Rd*gamma))
  tmp2 = pt*MV2.exp(-g*(z-zt)/(Rd*Tvt))
  if isinstance(tmp1,float):
    if z > zt:
        tmp = tmp2
    else:
        tmp = tmp1
  else:  
    tmp = MV2.where(z > zt, tmp2, tmp1)
  return tmp

lat = MV2.array([0.,],typecode=MV2.float32)
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.array([0.,],typecode=MV2.float32)
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

level = MV2.array(range(0,80001,10),typecode=MV2.float)
level = cdms2.createAxis(level)
level.designateLevel()
level.id = 'level'
level.long_name = 'altitude'
level.units = 'm'

nlev, = level.shape

tmin = cdtime.comptime(1979,1,1,0,0,0)
tmax = tmin.add(1000,cdtime.Day)
#tmax = cdtime.comptime(1979,1,2,0,0,0)
units = 'seconds since 1979-01-01 00:00:0.0'

time = MV2.array(range(0,2),typecode=MV2.float)
time[1] = tmax.torel(units).value
time = cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.units = units
time.calendar = 'gregorian'

nt, = time.shape

var3D = ['qv','temp','pressure','u','v']
var2D = ['ts','ps']

for SST in [295,300,305]:
  print SST
   
  data = {}
  for var in var3D:
    data[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float)

  for it in range(0,nt):
    data['qv'][it,:,0,0] = q(level[:],SST=SST)
    data['temp'][it,:,0,0] = T(level[:],SST=SST)
    data['pressure'][it,:,0,0] = p(level[:],SST=SST)

  for var in var3D:
    data[var].id = var
    data[var].setAxis(0,time)
    data[var].setAxis(1,level)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)

  for var in var2D:
    data[var] = MV2.zeros((nt,1,1),typecode=MV2.float)

  data['ts'][:,0,0] = SST
  data['ps'][:,0,0] = ps

  for var in var2D:
    data[var].id = var
    data[var].setAxis(0,time)
    data[var].setAxis(1,lat)
    data[var].setAxis(2,lon)


  g = cdms2.open('RECMIP_SST{0}.nc'.format(SST),'w')

  for var in var2D + var3D:
    g.write(data[var])

  g.comment = 'Initial conditions for RCE-MIP simulation SST = {0}K'.format(SST)
  g.reference = 'Wing et al. (2018, GMD)'
  g.author = 'R. Roehrig'


  g.case = 'RCEMIP/SST{0}'.format(SST)
  g.startDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmin.year,month=tmin.month,day=tmin.day)
  g.endDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmax.year,month=tmax.month,day=tmax.day)

  g.RCE = 1
  g.zangle = 42.05 # degrees
  g.I0 = 551.58
  g.alb = 0.07
  g.CO2 = 348.  # ppmv
  g.CH4 = 1650. # ppbv
  g.N2O = 306. # ppbv
  g.CFC11 = 0. # pptv
  g.CFC12 = 0. # pptv
  g.CCN = 1.e8 # m-3

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
  g.surfaceType = 'ocean'
  g.surfaceForcing = 'ts'
  g.z0 = 0.1

  g.close()
