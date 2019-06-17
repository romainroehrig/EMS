import cdms2
import MV2
import numpy as np
from scipy import interpolate

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

dico = {}
dico['u'] = 'uwind'
dico['v'] = 'vwind'
dico['qv'] = 'qvthermo'
dico['th'] = 'ththermo'
dico['thadvh'] = 'thadvhforcing'
dico['qadvh'] = 'qadvhforcing'
dico['w'] = 'wforcing'
dico['ug'] = 'ugforcing'
dico['vg'] = 'vgforcing'
dico['sfc_sens_flx'] = 'sfc_sens_flx'
dico['sfc_lat_flx'] = 'sfc_lat_flx'
dico['ps'] = 'ps'

time = range(0,21601,3600)
nt = len(time)
time = cdms2.createAxis(MV2.array(time,typecode=MV2.float32))
time.designateTime()
time.id = 'time'
time.units = 'seconds since 2002-06-14 06:00:00'
time.calendar = 'gregorian'

data = {}

f = cdms2.open('IHOP_LES_driver.nc')
for var in f.listvariables():
    data[var] = f(var)

for var in dico.keys():
  data[var] = data[dico[var]]


for var in ['u','v','th','qv','Pthermo']:
  nlev, = data[var].shape
  lev = data[var].getAxis(0)
  tmp = MV2.zeros((nt,nlev),typecode=MV2.float32)
  for i in range(0,nt):
      tmp[i,:] = data[var][:] 
 
  tmp.setAxis(0,time)
  tmp.setAxis(1,lev)
  data[var] = tmp*1.


for var in ['thadvh','qadvh','w','ug','vg']:
  nlev,nt0 = data[var].shape
  lev = data[var].getAxis(0)
  time0 = data[var].getAxis(1)
  tmp = MV2.zeros((nt0,nlev),typecode=MV2.float32)
  for i in range(0,nt0):
    tmp[i,:] = data[var][:,i]

  tmp.setAxis(0,time0)
  tmp.setAxis(1,lev)

  data[var] = tmp*1.

f.close()

#time = 0, 10800, 21600, 32400, 43200 ;

lat = MV2.zeros(1,typecode=MV2.float32) + 36.5
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.zeros(1,typecode=MV2.float32) - 100.61
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

lev = [0, 16, 51.4, 76, 86.9, 122.6, 124, 158.4, 174, 212.3, 225, 274, 284.7, 325, 357.7, 375, 425, 431.2, 475, 505.2, 525, 574, 579.9, 624, 655.1, 674, 725, 769, 774, 825, 874, 923.1, 925, 974, 1025, 1075, 1079.8, 1125, 1175, 1225, 1239, 1275, 1324, 1374, 1401, 1424, 1474, 1524, 1575, 1586.6, 1625, 1675, 1725, 1774, 1797.2, 1825, 1874, 1924, 1974, 2025, 2075, 2122.2, 2124, 2176, 2225, 2275, 2325, 2375, 2426, 2475, 2524, 2574.1, 2575, 2624, 2675, 2725, 2775, 2824, 2875, 2923, 2974, 3024, 3049.3, 3075, 3125, 3176, 3225, 3275, 3324, 3375, 3424, 3475, 3525, 3574, 3625, 3675, 3723, 3774, 3811.9, 3824, 3873, 3924, 3974, 4025, 4075, 4125, 4175, 4218, 4274.9, 4325.6, 4374.5, 4425.7, 4474.9, 4500, 4525.6, 4574.5, 4625, 4674.5, 4724.8, 4774.9, 4823.4, 4874.5, 4925.3, 4974.5, 6000, 10000, 15000]
nlev = len(lev)
lev = MV2.array(lev,typecode=MV2.float32)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'nlev'
lev.units = 'm'
lev.positive = 'up'


variables0D = [] #['orog']
variables2D = ['sfc_lat_flx','sfc_sens_flx','ps','ts']
variables3D = ['pressure','th','qv','temp','u','v','thadvh','qadvh','w','ug','vg']

variables = variables3D + variables2D + variables0D

units = {}
units['pressure'] = 'Pa'
units['th'] = 'K'
units['qv'] = 'kg kg-1'
units['temp'] = 'K'
units['u'] = 'm s-1'
units['v'] = 'm s-1'
units['ug'] = 'm s-1'
units['vg'] = 'm s-1'
units['sfc_lat_flx'] = 'W m-2'
units['sfc_sens_flx'] = 'W m-2'
units['orog'] = 'm'
units['ps'] = 'Pa'
units['ts'] = 'K'
units['w'] = 'm s-1'
units['qadvh'] = 'kg kg-1 s-1'
units['thadvh'] = 'K s-1'
units['tadvh'] = 'K s-1'
units['ustar'] = 'm s-1'

names = {}
names['pressure'] = 'Pressure'
names['th'] = 'Potential temperature'
names['qv'] = 'Specific humidity'
names['temp'] = 'Temperature'
names['u'] = 'Zonal wind'
names['v'] = 'Meridional wind'
names['ug'] = 'Geostrophic zonal wind'
names['vg'] = 'Geostrophic meridional wind'
names['sfc_lat_flx'] = 'Surface latent heat flux'
names['sfc_sens_flx'] = 'Surface sensible heat flux'
names['ustar'] = 'Surface ustar'
names['orog'] = 'Surface orography'
names['ps'] = 'Surface pressure'
names['ts'] = 'Surface temperature'
names['w'] = 'Vertical velocity'
names['qadvh'] = 'Specific Humidity horizontal advection'
names['thadvh'] = 'Potential temperature horizontal advection'
names['tadvh'] = 'Temperature horizontal advection'


datanew = {}
for var in variables0D:
  datanew[var] = MV2.zeros((1,1),typecode=MV2.float32)    
  datanew[var].setAxis(0,lat)
  datanew[var].setAxis(1,lon)
for var in variables2D:
  datanew[var] = MV2.zeros((nt,1,1),typecode=MV2.float32)    
  datanew[var].setAxis(0,time)
  datanew[var].setAxis(1,lat)
  datanew[var].setAxis(2,lon)
for var in variables3D:
  datanew[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float32)
  datanew[var].setAxis(0,time)
  datanew[var].setAxis(1,lev)
  datanew[var].setAxis(2,lat)
  datanew[var].setAxis(3,lon)

data['pressure'] = data['Pthermo']
data['temp'] = data['th']*1.
nt0,nlev0, = data['temp'].shape
for ilev in range(0,nlev0):
  for it in range(0,nt0):
    data['temp'][it,ilev] = data['th'][it,ilev]*(data['Pthermo'][it,ilev]/100000.)**(2./7.)  

#data['tadvh'] = data['thadvh']*1.
#nt0,nlev0, = data['tadvh'].shape
#for ilev in range(0,nlev0):
#  for it in range(0,nt0):
#    data['tadvh'][it,ilev] = data['thadvh'][it,ilev]*(data['Pthermo'][it,ilev]/100000.)**(2./7.)    

for var in variables3D:
  time0 = data[var].getAxis(0)
  lev0 = data[var].getAxis(1)
  nt0,nlev0 =data[var].shape
  new = MV2.zeros((nt,nlev0),typecode=MV2.float32)
  for ilev in range(nlev0):
    ff = interpolate.interp1d(np.array(time0[:]),np.array(data[var][:,ilev]))
    new[:,ilev] = ff(np.array(time[:]))
  new.setAxis(0,time)
  new.setAxis(1,lev0)
  data[var] = new

  levin = data[var].getAxis(1)
  nlevin, = levin.shape
  for ilev in range(0,nlev):
    lev0 = lev[ilev]
    ii = 0
    lflag = False
    for ilevin in range(0,nlevin-1):
      if  lev0 >= levin[ilevin] and lev0 < levin[ilevin+1]:
        ii = ilevin            
        lflag = True
    if not(lflag):
      if lev0 >= levin[nlevin-1]:
        ii = nlevin-2
      if lev0 <= levin[0]:
        ii = 0
    tmp = data[var][:,ii] + (data[var][:,ii+1]-data[var][:,ii])*(lev0 - levin[ii])/(levin[ii+1]-levin[ii])
#    for it1 in range(0,4):
#      for it2 in range(0,6):
#        datanew[var][it1*6+it2,ilev,0,0] = tmp[it1] + (tmp[it1+1]-tmp[it1])*it2/6. 
    datanew[var][:,ilev,0,0] = tmp[:]
    if var == 'qv' and not(lflag):
        datanew[var][:,ilev,0,0] = 0.


nt,nlev,x,y = datanew['temp'].shape
#10000 m
datanew['pressure'][:,-2,0,0] = datanew['pressure'][:,-2,0,0]*0. + 30000.
datanew['u'][:,-2,0,0] = datanew['u'][:,-2,0,0]*0.
datanew['v'][:,-2,0,0] = datanew['v'][:,-2,0,0]*0.

#15000 m
datanew['pressure'][:,-1,0,0] = datanew['pressure'][:,-1,0,0]*0. + 10000.
datanew['u'][:,-1,0,0] = datanew['u'][:,-1,0,0]*0.
datanew['v'][:,-1,0,0] = datanew['v'][:,-1,0,0]*0.

for ilev in range(0,nlev):
  for it in range(0,nt):
    datanew['temp'][it,ilev,0,0] = datanew['th'][it,ilev,0,0]*(datanew['pressure'][it,ilev,0,0]/100000.)**(2./7.) 

for var in ['sfc_lat_flx','sfc_sens_flx']:
    time0 = data[var].getAxis(0)
    ff = interpolate.interp1d(np.array(time0[:]),np.array(data[var][:]))
    datanew[var][0:nt,0,0] = ff(np.array(time[:]))


datanew['ts'] = datanew['sfc_lat_flx']*0. + 320.

datanew['ps'][:,0,0] = datanew['ps'][:,0,0] + data['ps']
#datanew['orog'][0,0] = datanew['orog'][0,0] + data['orog']

#datanew['ustar'] = datanew['ps']*0.

datanew['tadvh'] = datanew['thadvh']*1.
nt0,nlev0,ny,nx = datanew['tadvh'].shape
for ilev in range(0,nlev0):
  for it in range(0,nt0):
    datanew['tadvh'][it,ilev,0,0] = datanew['thadvh'][it,ilev,0,0]*(datanew['pressure'][it,ilev,0,0]/100000.)**(2./7.)

variables.append('tadvh')

#datanew['qv'][:,nlev-1,0,0] = datanew['qv'][:,nlev-1,0,0]*0.
#datanew['qv'][:,nlev-2,0,0] = datanew['qv'][:,nlev-2,0,0]*0.

g = cdms2.open('IHOP_driver_FC_RR.nc','w')

for var in variables:
  datanew[var].id = var
  if var == 'th':
    datanew[var].id = 'theta'
  datanew[var].long_name = names[var]    
  datanew[var].units = units[var]
  g.write(datanew[var])

g.description = "Subsidence, radiation included in temperature large-scale advection, moisture large-scale advection"
g.reference = '??'
g.author = "F Couvreux"
g.modifications = "2018-07-26: R. Roehrig put all fields on the same vertical and time axes"
g.case = "IHOP" 
g.startDate = "20020614060000" 
g.endDate =   "20020614120000" 
g.qadvh = 1
g.tadvh = 1
g.qadvv = 0 
g.tadvv = 0 
g.trad = 'adv' 
g.forc_omega = 0 
g.forc_w = 1
g.forc_geo = 1
g.nudging_u = 0 
g.nudging_v = 0
g.nudging_t = 0 
g.nudging_q = 0 
g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 110000
g.p_nudging_q = 110000
g.zorog = 0.
g.surfaceForcing = "surfaceFlux" 
g.surfaceForcingWind = "z0"
g.surfaceType = "land"
g.z0 = 0.1

g.close()

