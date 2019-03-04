import cdms2
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)


data = {}

f = cdms2.open('AYOTTE_A05SC_driver_FC.nc')
for var in f.listvariables():
  data[var] = f(var)    

f.close()


time = cdms2.createAxis(MV2.array([0.,3600*6],typecode=MV2.float32))
time.designateTime()
time.id = 'time'
time.units = 'seconds since 2009-12-11 00:00:00'
time.calendar = 'gregorian'

lat = MV2.zeros(1,typecode=MV2.float32) + 45
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.zeros(1,typecode=MV2.float32) + 123.33
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

#lev = [0,5,130,400,415,425,434,444,454,463,473,475,482,492,500,501,511,520,530,540,549,559,568,578,587,597,600,606,616,625,635,644,654,664,673,683,692,702,711,721,730,740,750,759,769,778,788,797,807,816,826,836,845,855,1000,1750,2000,3000,4000]
lev = range(0,5000,10)
nlev = len(lev)
lev = MV2.array(lev,typecode=MV2.float32)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'nlev'
lev.units = 'm'
lev.positive = 'up'


variables0D = [] #['orog']
variables2D = ['sfc_lat_flx','sfc_sens_flx','ps','ustar']
variables3D = ['pressure','th','qv','temp','u','v','ug','vg']

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
names['orog'] = 'Surface orography'
names['ps'] = 'Surface pressure'
names['ustar'] = 'Surface ustar'


datanew = {}
for var in variables0D:
  datanew[var] = MV2.zeros((1,1),typecode=MV2.float32)    
  datanew[var].setAxis(0,lat)
  datanew[var].setAxis(1,lon)
for var in variables2D:
  datanew[var] = MV2.zeros((2,1,1),typecode=MV2.float32)    
  datanew[var].setAxis(0,time)
  datanew[var].setAxis(1,lat)
  datanew[var].setAxis(2,lon)
for var in variables3D:
  datanew[var] = MV2.zeros((2,nlev,1,1),typecode=MV2.float32)
  datanew[var].setAxis(0,time)
  datanew[var].setAxis(1,lev)
  datanew[var].setAxis(2,lat)
  datanew[var].setAxis(3,lon)

data['pressure'] = data['Pthermo']
data['temp'] = data['th']*1.
nlev0, = data['temp'].shape
for ilev in range(0,nlev0):
  data['temp'][ilev] = data['th'][ilev]*(data['Pthermo'][ilev]/100000.)**(2./7.)    

for var in variables3D:
  levin = data[var].getAxis(0)
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
      ii = nlevin-2
    tmp = data[var][ii] + (data[var][ii+1]-data[var][ii])*(lev0 - levin[ii])/(levin[ii+1]-levin[ii])
    datanew[var][:,ilev,0,0] = datanew[var][:,ilev,0,0] + tmp

for var in ['sfc_lat_flx','sfc_sens_flx']:
  datanew[var][:,0,0] = data[var][:]

datanew['ps'][:,0,0] = datanew['ps'][:,0,0] + data['ps']
#datanew['orog'][0,0] = datanew['orog'][0,0] + data['orog']
datanew['ustar'][:,0,0] = datanew['ustar'][:,0,0] + 0.623

for it in range(0,2):
  for ilev in range(0,nlev):
      datanew['temp'][it,ilev,0,0] = datanew['th'][it,ilev,0,0]*(datanew['pressure'][it,ilev,0,0]/100000.)**(2./7.) 

g = cdms2.open('AYOTTE_A05SC_driver_FC_RR.nc','w')

for var in variables:
  datanew[var].id = var
  if var == 'th':
    datanew[var].id = 'theta'
  datanew[var].long_name = names[var]    
  datanew[var].units = units[var]
  g.write(datanew[var])

g.description = "No subsidence/ascendance, No T & q large-scale advection, no radiation, geostrophic wind" 
g.reference = 'Ayotte et al (1996, BLM)'
g.author = "F Couvreux"
g.modifications = "2018-04-20: R. Roehrig put all fields on the same vertical and time axes"
g.case = "AYOTTE_05SC" 
g.startDate = "20091211000000" 
g.endDate = "20091211060000" 
g.qadvh = 0 
g.tadvh = 0 
g.qadvv = 0 
g.tadvv = 0 
g.trad = 0 
g.forc_omega = 0 
g.forc_w = 0 
g.forc_geo = 1 
g.nudging_u = 0 
g.nudging_v = 0 
g.nudging_t = 0 
g.nudging_q = 0 
g.zorog = 0.
g.z0 = 0.16
#g.ustar = 0.623
g.surfaceForcing = "surfaceFlux" 
g.surfaceForcingWind = "z0"
g.surfaceType = "ocean"

g.close()

