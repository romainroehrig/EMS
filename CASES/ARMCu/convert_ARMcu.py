import cdms2
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

variables = ['thadv','qadv','pressure_f','temp','qv','rh','theta','u','v','ug','vg','sfc_sens_flx','sfc_lat_flx','ts','omega','orog']

data = {}

f = cdms2.open('ARMcu.nc')

for var in variables:
  data[var] = f(var)

f.close()

nt,nlev,nlat,nlon = data['thadv'].shape
time = data['thadv'].getTime()
lat = data['thadv'].getLatitude()
lon = data['thadv'].getLongitude()

lev = data['thadv'].getAxis(1)
lev.designateLevel()

data['tadv'] = data['thadv']*(data['pressure_f']/100000.)**(2./7.)
data['tadv'].id = 'tadv'
data['tadv'].title = 'Large scale total adv. of temperature'
data['tadv'].units = 'K/s'

data['pressure_f'].id = 'pressure'

data['ps'] = MV2.zeros((nt,nlat,nlon),typecode=MV2.float32) + 97000.
data['ps'].setAxis(0,time)
data['ps'].setAxis(1,lat)
data['ps'].setAxis(2,lon)
data['ps'].id = 'ps'
data['ps'].title = 'Surface Pressure'
data['ps'].units = 'Pa'

variables.append('tadv')
variables.append('ps')

for var in variables:
  if len(data[var].shape) == 4:
    data[var].setAxis(1,lev)      

g = cdms2.open('ARMCu_driver_RR.nc','w')
for var in variables:
  g.write(data[var])

g.comment = "Forcing and initial conditions for ARMCu case" 
g.reference = "http://projects.knmi.nl/eurocs/ARM/case_ARM_html"
g.author = "M.-P. Lefebvre" 
g.modifications = "2017-04-28: R. Roehrig - compute tadv + add ps + update forcing information"
g.case = "ARMCu" 
g.startDate = "19970621113000" 
g.endDate = "19970622023000"
g.qadv = 1 
g.tadv = 1
g.qadvh = 0
g.tadvh = 0 
g.tadvv = 0 
g.qadvv = 0 
g.trad = 0 
g.forc_omega = 0 
g.forc_w = 0 
g.forc_geo = 1 
g.nudging_u = 0 
g.nudging_v = 0 
g.nudging_t = 0 
g.nudging_q = 0 
g.zorog = 0.
g.z0 = 0.01
g.ustar = 0.
g.surfaceType = "land" 
g.surfaceForcing = "surfaceFlux"

g.close()
