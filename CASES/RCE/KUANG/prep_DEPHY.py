import MV2, cdms2
import cdtime
import sys

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value) 
cdms2.setNetcdfDeflateLevelFlag(value)

def trad(p):
    nlev, = p.shape
    tmp = MV2.zeros(nlev,typecode=MV2.float)
    for ilev in range(0,nlev):
        if p[ilev] >= 20000.:
            tmp[ilev] = -1.5/86400.
        elif p[ilev] <= 10000.:
            tmp[ilev] = 0.
        else:
            tmp[ilev] = (p[ilev]-10000.)/(20000.-10000.)*(-1.5/86400.)

    return tmp

ps = 101480

datain = {}

t0 = cdtime.comptime(1979,1,1,0,0,0)
tmin = t0.add(1000-50,cdtime.Day)
tmax = t0.add(1000,cdtime.Day)

#f = cdms2.open('/home/roehrig/MUSC/simulations//arp631.RCE/CMIP6/L91_900s/RCEMIP/SST300/Output/netcdf/Out_klevel.nc')
f = cdms2.open('/home/roehrig/MUSC/simulations//arp631.RCE/CMIP6/L91_900s/RCEMIP//SST301.15/Output/netcdf/Out_klevel.nc')
for var in ['pf','zf','temp','qv','ql','qi','tke']:
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

time0 = MV2.array([0],typecode=MV2.float)
time0 = cdms2.createAxis(time0)
time0.id = 't0'
time0.units = units
time0.calendar = 'gregorian'


time = MV2.array(range(0,2),typecode=MV2.float)
time[1] = tmax.torel(units).value
time = cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.units = units
time.calendar = 'gregorian'

nt, = time.shape

var3Dini = ['qv','temp','pressure','height','u','v','ql','qi','tke']
var3Dforc = ['pressure_forc','trad','temp_nudg','qv_nudg']
var2D = ['ts','ps']

titles = {}

titles['qv'] = 'Specific humidity'
titles['temp'] = 'Temperature'
titles['pressure'] = 'Pressure'
titles['height'] = 'Height above the surface'
titles['u'] = 'Zonal wind'
titles['v'] = 'Meridional wind'
titles['ql'] = 'Liquid water content'
titles['qi'] = 'Ice water content'
titles['tke'] = 'Turbulent kinetic energy'
titles['pressure_forc'] = 'Pressure'
titles['trad'] = 'Temperature radiative tendency'
titles['temp_nudg'] = 'Nudging temperature'
titles['qv_nudg'] = 'Nudging specific humidity'
titles['ts'] = 'Surface temperature'
titles['ps'] = 'Surface pressure'

units = {}
units['qv'] = 'kg kg-1'
units['temp'] = 'K'
units['pressure'] = 'Pa'
units['height'] = 'm'
units['u'] = 'm s-1'
units['v'] = 'm s-1'
units['ql'] = 'kg kg-1'
units['qi'] = 'kg kg-1'
units['tke'] = 'm2 s-2'
units['pressure_forc'] = 'Pa'
units['trad'] = 'K s-1'
units['temp_nudg'] = 'K'
units['qv_nudg'] = 'kg kg-1'
units['ts'] = 'K'
units['ps'] = 'Pa'

SST = 28.

   
data = {}
for var in var3Dini:
  data[var] = MV2.zeros((1,nlev,1,1),typecode=MV2.float)
  if var in ['u','v']:
      pass
  elif var == 'pressure':
      data[var][0,:,0,0] = datain['pf'][:]
  elif var == 'height':
      data[var][0,:,0,0] = datain['zf'][:]          
  elif var == 'tke':
      data[var][0,:,0,0] = datain['tke'][1:]
  else:
      data[var][0,:,0,0] = datain[var][:]

  data[var].id = var
  data[var].setAxis(0,time0)
  data[var].setAxis(1,level)
  data[var].setAxis(2,lat)
  data[var].setAxis(3,lon)
  data[var].title = titles[var]
  data[var].units = units[var]

for var in var3Dforc:
  data[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float)
  if var == 'pressure_forc':
      for it in range(0,nt):
          data[var][it,:,0,0] = datain['pf'][:]  
  elif var == 'trad':
      for it in range(0,nt):
          data['trad'][it,:,0,0] = trad(datain['pf'])
  elif var[-5:] == '_nudg':
      for it in range(0,nt):
          data[var][it,:,0,0] = datain[var[:-5]][:]
  else:
      print 'case unexepected for forcing:', var
      sys.exit()

  data[var].id = var
  data[var].setAxis(0,time)
  data[var].setAxis(1,level)
  data[var].setAxis(2,lat)
  data[var].setAxis(3,lon)
  data[var].title = titles[var]
  data[var].units = units[var]

for var in var2D:
  data[var] = MV2.zeros((nt,1,1),typecode=MV2.float)

data['ts'][:,0,0] = 273.15+SST
data['ps'][:,0,0] = ps

for var in var2D:
  data[var].id = var
  data[var].setAxis(0,time)
  data[var].setAxis(1,lat)
  data[var].setAxis(2,lon)
  data[var].title = titles[var]
  data[var].units = units[var]


g = cdms2.open('KUANG_SST{0}_DEPHY.nc'.format(int(SST)),'w')

for var in var3Dini + var3Dforc + var2D:
    g.write(data[var])

g.comment = 'Initial conditions for RCE simulation SST = {0}C; Initial state from the last 20 days of RCEMIP/SST={1}K'.format(SST,SST+273.15)
g.reference = 'Herman and Kuang (2013, JAMES)'
g.author = 'R. Roehrig'


g.case = 'RCE/KUANG_SST{0}'.format(SST)
g.startDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmin.year,month=tmin.month,day=tmin.day)
g.endDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmax.year,month=tmax.month,day=tmax.day)

g.RCE = 1

g.tadvh = 0
g.tadvv = 0
g.trad = 1 

g.qvadvh = 0
g.qvadvv = 0

g.forc_omega = 0
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0.5*86400
g.nudging_qv = 0.5*86400

g.p_nudging_t = 10000.
g.p_nudging_qv = 10000.

g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'
g.minSurfaceWind = 5.
#g.z0 = 0.1

g.close()
