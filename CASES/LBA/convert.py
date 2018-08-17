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
dico['sfc_sens_flx'] = 'sfc_sens_flx'
dico['sfc_lat_flx'] = 'sfc_lat_flx'
dico['ps'] = 'ps'

time = range(0,21601,300)
nt = len(time)
time = cdms2.createAxis(MV2.array(time,typecode=MV2.float32))
time.designateTime()
time.id = 'time'
time.units = 'seconds since 1999-02-23 07:30:00'
time.calendar = 'gregorian'

data = {}

f = cdms2.open('LBA_LES_driver.nc')
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


for var in ['thadvh']:
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
# Coordinates of Ji-Parana, Brazil
lat = MV2.zeros(1,typecode=MV2.float32) - 10.88
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.zeros(1,typecode=MV2.float32) + 61.95
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

lev = [0, 42.5, 130, 200.9, 456.3, 464, 573, 743, 1061.1, 1100, 1410.5, 1653, 1791.3, 2203.5, 2216, 2647, 2760, 3121.9, 3297, 3628.1, 3824, 4165.7, 4327, 4734.7, 4787, 5242, 5335, 5686, 5966.7, 6131, 6578, 6629.7, 6996, 7324.1, 7431, 7881, 8049.9, 8300, 8718, 8807, 9149, 9595.5, 9611, 10084, 10415.3, 10573, 11008, 11266.5, 11460, 11966, 12149.1, 12472, 12971, 13063, 13478, 13971, 14008.3, 14443, 14956, 14984.9, 15458, 15992.9, 16019, 16491, 16961, 17032.3, 17442, 17934, 18103, 18397, 18851, 19205.1, 19331, 19809, 20321, 20338.5, 20813, 21329, 21503.3, 22699.5, 30000]
nlev = len(lev)
lev = MV2.array(lev,typecode=MV2.float32)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'nlev'
lev.units = 'm'
lev.positive = 'up'


variables0D = [] #['orog']
variables2D = ['sfc_lat_flx','sfc_sens_flx','ps','ustar']
variables3D = ['pressure','th','qv','temp','u','v','thadvh']

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


for var in ['sfc_lat_flx','sfc_sens_flx']:
    time0 = data[var].getAxis(0)
    ff = interpolate.interp1d(np.array(time0[:]),np.array(data[var][:]))
    datanew[var][0:nt,0,0] = ff(np.array(time[:]))


datanew['ps'][:,0,0] = datanew['ps'][:,0,0] + data['ps']
#datanew['orog'][0,0] = datanew['orog'][0,0] + data['orog']

datanew['ustar'] = datanew['ps']*0.

datanew['tadvh'] = datanew['thadvh']*1.
nt0,nlev0,ny,nx = datanew['tadvh'].shape
for ilev in range(0,nlev0):
  for it in range(0,nt0):
    datanew['tadvh'][it,ilev,0,0] = datanew['thadvh'][it,ilev,0,0]*(datanew['pressure'][it,ilev,0,0]/100000.)**(2./7.)

variables.append('tadvh')

#datanew['qv'][:,nlev-1,0,0] = datanew['qv'][:,nlev-1,0,0]*0.
#datanew['qv'][:,nlev-2,0,0] = datanew['qv'][:,nlev-2,0,0]*0.

g = cdms2.open('LBA_driver_FC_RR.nc','w')

for var in variables:
  datanew[var].id = var
  if var == 'th':
    datanew[var].id = 'theta'
  datanew[var].long_name = names[var]    
  datanew[var].units = units[var]
  g.write(datanew[var])

g.description = "Radiation included in temperature large-scale advection, no surface friction (ustar=0)"
g.reference = '??'
g.author = "F Couvreux"
g.modifications = "2018-07-24: R. Roehrig put all fields on the same vertical and time axes"
g.case = "LBA" 
g.startDate = "19990223073000" 
g.endDate = "19990223133000" 
g.qadvh = 0
g.tadvh = 1
g.qadvv = 0 
g.tadvv = 0 
g.trad = 'adv' 
g.forc_omega = 0 
g.forc_w = 0 
g.forc_geo = 0
g.nudging_u = 3601 
g.nudging_v = 3601
g.nudging_t = 0 
g.nudging_q = 0 
g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 110000
g.p_nudging_q = 110000
g.zorog = 0.
g.surfaceForcing = "surfaceFlux" 
g.surfaceForcingWind = "ustar"
g.surfaceType = "ocean"

g.close()


variables0D = [] #['orog']
variables2D = ['sfc_lat_flx','sfc_sens_flx','ps']
variables3D = ['pressure','th','qv','temp','u','v','thadvh','tadvh']

variables = variables3D + variables2D + variables0D

g = cdms2.open('LBA_driver_MesoNH_RR.nc','w')

for var in variables:
  datanew[var].id = var
  if var == 'th':
    datanew[var].id = 'theta'
  datanew[var].long_name = names[var]    
  datanew[var].units = units[var]
  g.write(datanew[var])

g.description = "Radiation included in temperature large-scale advection, surface friction with z0=0.1"
g.reference = '??'
g.author = "F Couvreux"
g.modifications = "2018-07-24: R. Roehrig put all fields on the same vertical and time axes"
g.case = "LBA" 
g.startDate = "19990223073000" 
g.endDate = "19990223133000" 
g.qadvh = 0
g.tadvh = 1
g.qadvv = 0 
g.tadvv = 0 
g.trad = 'adv' 
g.forc_omega = 0 
g.forc_w = 0 
g.forc_geo = 0
g.nudging_u = 3601 
g.nudging_v = 3601
g.nudging_t = 0 
g.nudging_q = 0 
g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 110000
g.p_nudging_q = 110000
g.zorog = 0.
g.surfaceForcing = "surfaceFlux" 
g.surfaceForcingWind = "z0"
g.z0 = 0.1
g.surfaceType = "ocean"

g.close()
