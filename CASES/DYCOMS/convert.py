import cdms2
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

dico = {}
dico['u'] = 'uwind'
dico['v'] = 'vwind'
dico['qv'] = 'qvthermo'
dico['th'] = 'ththermo'
dico['ug'] = 'ugforcing'
dico['vg'] = 'vgforcing'
dico['qadvh'] = 'qadvhforcing'
dico['thadvh'] = 'thadvhforcing'
dico['w'] = 'wforcing'
dico['ps'] = 'ps'
dico['ts'] = 'Surface_temperature'

data = {}

f = cdms2.open('DYCOMS_LES_driver.nc')
for var in f.listvariables():
    data[var] = f(var)

for var in dico.keys():
  data[var] = data[dico[var]]


for var in ['u','v','th','qv','Pthermo']:
  nlev, = data[var].shape
  lev = data[var].getAxis(0)
  tmp = MV2.zeros((2,nlev),typecode=MV2.float32)
  for i in range(0,2):
      tmp[i,:] = data[var][:] 
 
  tmp.setAxis(1,lev)
  data[var] = tmp*1.


for var in ['ug','vg','qadvh','thadvh','w']:
  nlev,nt = data[var].shape
  lev = data[var].getAxis(0)
#  time = data[var].getAxis(1)
  tmp = MV2.zeros((2,nlev),typecode=MV2.float32)
  for i in range(0,2):
    tmp[i,:] = data[var][:,0]

#  tmp.setAxis(0,time)
  tmp.setAxis(1,lev)

  data[var] = tmp*1.

f.close()

time = [0, 43200]
nt = len(time)
time = cdms2.createAxis(MV2.array(time,typecode=MV2.float32))
time.designateTime()
time.id = 'time'
time.units = 'seconds since 1987-07-14 07:00:00'
time.calendar = 'gregorian'

lat = MV2.zeros(1,typecode=MV2.float32) + 33.3
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.zeros(1,typecode=MV2.float32) - 1119.5
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

lev = [0, 10, 100, 300, 500, 590, 600, 800, 839.95, 840.05, 840.14, 840.3, 845, 850, 900, 945, 1000, 1100, 1157, 1200, 1327, 1500, 2000, 3300, 3400, 10000]
nlev = len(lev)
lev = MV2.array(lev,typecode=MV2.float32)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'nlev'
lev.units = 'm'
lev.positive = 'up'


variables0D = [] #['orog']
variables2D = ['ts','ps']
variables3D = ['pressure','th','qv','temp','u','v','ug','vg','w']

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
names['ts'] = 'Surface temperarure'
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


datanew['ps'][:,0,0] = datanew['ps'][:,0,0] + data['ps']
datanew['ts'][:,0,0] = datanew['ts'][:,0,0] + data['ts']

datanew['qv'][:,nlev-1,0,0] = datanew['qv'][:,nlev-1,0,0]*0.
datanew['qv'][:,nlev-2,0,0] = datanew['qv'][:,nlev-2,0,0]*0.


#datanew['tadvh'] = datanew['thadvh']*1.
#nt0,nlev0,ny,nx = datanew['tadvh'].shape
#for ilev in range(0,nlev0):
#  for it in range(0,nt0):
#    datanew['tadvh'][it,ilev,0,0] = datanew['thadvh'][it,ilev,0,0]*(datanew['pressure'][it,ilev,0,0]/100000.)**(2./7.)
#
#variables.append('tadvh')

g = cdms2.open('DYCOMS_driver_FC_RR.nc','w')

for var in variables:
  datanew[var].id = var
  if var == 'th':
    datanew[var].id = 'theta'
  datanew[var].long_name = names[var]    
  datanew[var].units = units[var]
  g.write(datanew[var])

g.description = "subsidence and geostrophic wind"
g.reference = '??'
g.author = "F Couvreux"
g.modifications = "2018-06-13: R. Roehrig put all fields on the same vertical and time axes"
g.case = "DYCOMS" 
g.startDate = "19870714070000" 
g.endDate = "19870714190000" 
g.qadvh = 0
g.tadvh = 0 
g.qadvv = 0 
g.tadvv = 0 
g.trad = 0 
g.forc_omega = 0 
g.forc_w = 1 
g.forc_geo = 1 
g.nudging_u = 0 
g.nudging_v = 0 
g.nudging_t = 0 
g.nudging_q = 0 
g.zorog = 0.
g.z0 = 0.01
g.surfaceForcing = "ts" 
g.surfaceType = "ocean"

g.close()

