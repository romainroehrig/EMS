import MV2, cdms2
import cdtime
import math
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

#ampl = 0.5
#ampl_name = '05'
ampl = 0.2
ampl_name = '02'

def perturb(p,lev=1):
    nlev, = p.shape
    tmp = MV2.zeros(nlev,typecode=MV2.float)
    for ilev in range(0,nlev):
        tmp[ilev] = 0.5*(int(ilev == lev) + math.exp(-((p[ilev]-p[lev])/7500.)**2))

    tmp = tmp*ampl/86400.
    return tmp

ps = 101480

datain = {}

t0 = cdtime.comptime(1979,1,1,0,0,0)
dt = 900.
tt = t0.add(200-dt/2./86400,cdtime.Day)

#f = cdms2.open('/home/roehrig/MUSC/simulations//arp631.RCE/CMIP6/L91_900s/RCE//KUANG_SST28/Output/netcdf/Out_klevel.nc')
#f = cdms2.open('/home/roehrig/MUSC/simulations/arp631.KUANG/CMIP6.IDEAL/L91_900s/RCE/KUANG_SST28_DEPHY/Output/netcdf/Out_klevel.nc')
f = cdms2.open('/home/roehrig/MUSC/simulations/arp641.KUANG/CMIP6.IDEAL/L91_900s/RCE/KUANG_SST28_DEPHY/Output/netcdf/Out_klevel.nc')
for var in ['pf','zf','u','v','temp','qv','ql','qi','tke']:
    #if var == 'tke':
    if var in ['v','ql','qi','tke']:
        datain[var] = datain['pf']*0.
    elif var == 'u':
        datain[var] = datain['pf']*0.+4.8
    else:
        datain[var] = MV2.average(f(var,time = tt),axis=0)
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

t0 = cdtime.comptime(1979,1,1,0,0,0)
tmin = t0.add(200,cdtime.Day)
#tmax = tmin.add(400,cdtime.Day)
tmax = tmin.add(1000,cdtime.Day)
#tmax = tmin.add(2000,cdtime.Day)
#tmax = tmin.add(1,cdtime.Day)
#tmax = cdtime.comptime(1979,1,2,0,0,0)
units = 'seconds since 1979-01-01 00:00:0.0'

time0 = MV2.array([0],typecode=MV2.float)
time0[0] = tmin.torel(units).value
time0 = cdms2.createAxis(time0)
time0.id = 't0'
time0.units = units
time0.calendar = 'gregorian'


time = MV2.array(range(0,2),typecode=MV2.float)
time[0] = tmin.torel(units).value
time[1] = tmax.torel(units).value
time = cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.units = units
time.calendar = 'gregorian'

nt, = time.shape

var3Dini = ['qv','temp','pressure','height','u','v','ql','qi','tke']
var3Dforc = ['pressure_forc','temp_rad','temp_nudging','qv_nudging','temp_adv','u_nudging','v_nudging']
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
titles['temp_rad'] = 'Temperature radiative tendency'
titles['temp_adv'] = 'Temperature advection'
titles['u_nudging'] = 'Nudging zonal wind'
titles['v_nudging'] = 'Nudging zonal wind'
titles['temp_nudging'] = 'Nudging temperature'
titles['qv_nudging'] = 'Nudging specific humidity'
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
units['temp_rad'] = 'K s-1'
units['temp_adv'] = 'K s-1'
units['u_nudging'] = 'm s-1'
units['v_nudging'] = 'm s-1'
units['temp_nudging'] = 'K'
units['qv_nudging'] = 'kg kg-1'
units['ts'] = 'K'
units['ps'] = 'Pa'

SST = 28.

   
data = {}
for var in var3Dini:
  data[var] = MV2.zeros((1,nlev,1,1),typecode=MV2.float)
  if var in ['u','v']:
      data[var][0,:,0,0] = datain[var][:]
  elif var == 'pressure':
      data[var][0,:,0,0] = datain['pf'][:]
  elif var == 'height':
      data[var][0,:,0,0] = datain['zf'][:]          
#  elif var == 'tke':
#      data[var][0,:,0,0] = datain['tke'][1:]
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
  elif var == 'temp_rad':
      for it in range(0,nt):
          data['temp_rad'][it,:,0,0] = trad(datain['pf'])
  elif var == 'temp_adv':
      pass
#      for it in range(0,nt):
#          data['tadvh'][it,:,0,0] = perturb(datain['pf'])          
  elif var[-8:] == '_nudging':
      for it in range(0,nt):
          data[var][it,:,0,0] = datain[var[:-8]][:]
      if var[0:1] == 'u':
          data[var][:,:,0,0] = data[var][:,:,0,0]*0. + 4.8
      if var[0:1] == 'v':
          data[var][:,:,0,0] = data[var][:,:,0,0]*0.
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


for lev0 in range(1,91):

    var = 'temp_adv'
    data[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float)
    for it in range(0,nt):
        data[var][it,:,0,0] = perturb(datain['pf'],lev = lev0)          

    data[var].id = var
    data[var].setAxis(0,time)
    data[var].setAxis(1,level)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
    data[var].title = titles[var]
    data[var].units = units[var]

    g = cdms2.open('KUANG_SST{0}_TP{2}_l{1:0>2}_DEPHY.nc'.format(int(SST),int(lev0),ampl_name),'w')

    for var in var3Dini + var3Dforc + var2D:
        g.write(data[var])

    g.comment = 'Initial conditions for RCE simulation SST = {0}C with positive T perturbation at level {2} ({3:8.2f} hPa); Initial state from the last 20 days of RCEMIP/SST={1}K'.format(SST,SST+273.15,lev0,datain['pf'][lev0]/100.)
    g.reference = 'Herman and Kuang (2013, JAMES)'
    g.author = 'R. Roehrig'


    g.case = 'RCE/KUANG_SST{0}_TP{2}_l{1:0>2}'.format(SST,lev0,ampl_name)
    g.startDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmin.year,month=tmin.month,day=tmin.day)
    g.endDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmax.year,month=tmax.month,day=tmax.day)

    g.RCE = 1

    g.adv_temp = 1
    g.rad_temp = 1 

    g.adv_qv = 0

    g.forc_omega = 0
    g.forc_w = 0

    g.forc_geo = 0

    g.nudging_u = 3*3600
    g.nudging_v = 3*3600
    g.nudging_temp = 2*86400
    g.nudging_qv = 2*86400

    g.p_nudging_u = 110000.
    g.p_nudging_v = 110000.
    g.p_nudging_temp = 10000.
    g.p_nudging_qv = 10000.

    g.zorog = 0.
    g.surfaceType = 'ocean'
    g.surfaceForcing = 'ts'
    g.minSurfaceWind = 4.8

    g.close()

for lev0 in range(1,91):

    var = 'temp_adv'
    data[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float)
    for it in range(0,nt):
        data[var][it,:,0,0] = -1*perturb(datain['pf'],lev = lev0)          

    data[var].id = var
    data[var].setAxis(0,time)
    data[var].setAxis(1,level)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
    data[var].title = titles[var]
    data[var].units = units[var]

    g = cdms2.open('KUANG_SST{0}_TM{2}_l{1:0>2}_DEPHY.nc'.format(int(SST),int(lev0),ampl_name),'w')

    for var in var3Dini + var3Dforc + var2D:
        g.write(data[var])

    g.comment = 'Initial conditions for RCE simulation SST = {0}C with negative T perturbation at level {2} ({3:8.2f} hPa); Initial state from the last 20 days of RCEMIP/SST={1}K'.format(SST,SST+273.15,lev0,datain['pf'][lev0]/100.)
    g.reference = 'Herman and Kuang (2013, JAMES)'
    g.author = 'R. Roehrig'


    g.case = 'RCE/KUANG_SST{0}_TM{2}_l{1:0>2}'.format(SST,lev0,ampl_name)
    g.startDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmin.year,month=tmin.month,day=tmin.day)
    g.endDate = '{year:4d}{month:02d}{day:02d}000000'.format(year=tmax.year,month=tmax.month,day=tmax.day)

    g.RCE = 1

    g.adv_temp = 1
    g.rad_temp = 1 

    g.adv_qv = 0

    g.forc_omega = 0
    g.forc_w = 0

    g.forc_geo = 0

    g.nudging_u = 3*3600
    g.nudging_v = 3*3600
    g.nudging_temp = 2*86400
    g.nudging_qv = 2*86400

    g.p_nudging_u = 110000.
    g.p_nudging_v = 110000.
    g.p_nudging_temp = 10000.
    g.p_nudging_qv = 10000.

    g.zorog = 0.
    g.surfaceType = 'ocean'
    g.surfaceForcing = 'ts'
    g.minSurfaceWind = 4.8

    g.close()    
