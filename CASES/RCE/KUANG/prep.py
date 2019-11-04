import MV2, cdms2
import cdtime

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value) 
cdms2.setNetcdfDeflateLevelFlag(value)

def trad(p):
    nlev, = p.shape
    tmp = MV2.zeros(nlev,typecode=MV2.float)
    for ilev in range(0,nlev):
        if p[ilev] >= 20000.:
            tmp[ilev] = 0.
        elif p[ilev] <= 10000.:
            tmp[ilev] = -1.5/86400.
        else:
            tmp[ilev] = (20000.-p[ilev])/(20000.-10000.)*(-1.5/86400.)

    return tmp

ps = 101480

datain = {}

t0 = cdtime.comptime(1979,1,1,0,0,0)
tmin = t0.add(1000-50,cdtime.Day)
tmax = t0.add(1000,cdtime.Day)

#f = cdms2.open('/home/roehrig/MUSC/simulations//arp631.RCE/CMIP6/L91_900s/RCEMIP/SST300/Output/netcdf/Out_klevel.nc')
f = cdms2.open('/home/roehrig/MUSC/simulations//arp631.RCE/CMIP6/L91_900s/RCEMIP//SST301.15/Output/netcdf/Out_klevel.nc')
for var in ['pf','temp','qv','ql','qi','tke']:
    datain[var] = MV2.average(f(var,time = (tmin,tmax)),axis=0)
f.close()


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

level = cdms2.createAxis(datain['pf'])
level.designateLevel()
level.id = 'level'
level.long_name = 'pressure'
level.units = 'Pa'

nlev, = level.shape

tmin = cdtime.comptime(1979,1,1,0,0,0)
tmax = tmin.add(600,cdtime.Day)
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

var3D = ['qv','temp','pressure','u','v','ql','qi','tke','trad']
var2D = ['ts','ps']

SST = 28.

   
data = {}
for var in var3D:
  data[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float)
  if var in ['u','v']:
      pass
  elif var == 'pressure':
      for it in range(0,nt):
          data[var][it,:,0,0] = datain['pf'][:]
  elif var == 'tke':
      for it in range(0,nt):
          data[var][it,:,0,0] = datain['tke'][1:]
  elif var == 'trad':
      for it in range(0,nt):
          data['trad'][it,:,0,0] = trad(datain['pf'])
  else:
      for it in range(0,nt):
          data[var][it,:,0,0] = datain[var][:]

for var in var3D:
  data[var].id = var
  data[var].setAxis(0,time)
  data[var].setAxis(1,level)
  data[var].setAxis(2,lat)
  data[var].setAxis(3,lon)

for var in var2D:
  data[var] = MV2.zeros((nt,1,1),typecode=MV2.float)

data['ts'][:,0,0] = 273.15+SST
data['ps'][:,0,0] = ps

for var in var2D:
  data[var].id = var
  data[var].setAxis(0,time)
  data[var].setAxis(1,lat)
  data[var].setAxis(2,lon)


g = cdms2.open('KUANG_SST{0}.nc'.format(int(SST)),'w')

for var in var2D + var3D:
    g.write(data[var])

g.comment = 'Initial conditions for RCE simulation SST = {0}C; Initial state from the last 20 days of RCEMIP/SST={1}K'.format(SST,SST+273.15)
g.reference = 'Herman and Kuang (2013, JAMES)'
g.author = 'R. Roehrig'


g.case = 'RCE/KUANG_SST{0}'.format(SST)
g.startDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmin.year,month=tmin.month,day=tmin.day)
g.endDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmax.year,month=tmax.month,day=tmax.day)

g.RCE = 1

g.qadvh = 0
g.tadvh = 0
g.tadvv = 0
g.qadvv = 0
g.trad = 1 

g.forc_omega = 0
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0.5*86400
g.nudging_q = 0.5*86400

g.p_nudging_t = 10000.
g.p_nudging_q = 10000.

g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'
g.minSurfaceWind = 5.
#g.z0 = 0.1

g.close()
