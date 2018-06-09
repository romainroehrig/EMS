import cdms2
import MV2

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value)
cdms2.setNetcdfDeflateLevelFlag(value)

data = {}
f = cdms2.open('BOMEX_driver_MPL.nc')

for var in f.listvariables():
  data[var] = f(var)

f.close()

lev = data['temp'].getAxis(1)
lev.designateLevel()
lev.id = 'lev'
nlev, = lev.shape

time = data['temp'].getAxis(0)
time[:] = MV2.array(range(0,48),typecode=MV2.float)*1800
time.designateTime()
time.id = 'time'

data['height'] = data['height_f']*1.
data['height'].id = 'height'
data['height'].units = 'm'
data['height'].title = 'Height'

data['pressure'] = data['pressure_f']*1.
data['pressure'].id = 'pressure'
data['pressure'].units = 'Pa'
data['pressure'].title = 'Pressure'

data['ug'] = -10 + 1.8e-3*data['height']
data['ug'].id = 'ug'
data['ug'].units = 'm/s'
data['ug'].title = 'Zonal geostrophic wind'

data['vg'] = data['ug']*0.
data['vg'].id = 'vg'
data['vg'].units = 'm/s'
data['vg'].title = 'Meridional geostrophic wind'

data['trad'] = data['tadv']*1.
data['trad'].id = 'trad'
data['trad'].units = 'K/s'
data['trad'].title = 'Radiative heating'

data['sfc_sens_flx'] = data['sfc_sens_flx']*0.+9.2
data['sfc_sens_flx'].id = 'sfc_sens_flx'
data['sfc_sens_flx'].units = 'W/m2'
data['sfc_sens_flx'].title = 'Surface sensible heat flux'

data['sfc_lat_flx'] = data['sfc_lat_flx']*0.+154.
data['sfc_lat_flx'].id = 'sfc_lat_flx'
data['sfc_lat_flx'].units = 'W/m2'
data['sfc_lat_flx'].title = 'Surface latent heat flux'

for ilev in range(0,nlev):
  if lev[ilev] <= 300:
      data['qadvh'][:,ilev,:,:] = -1.2e-8
  if lev[ilev] >= 500:
      data['qadvh'][:,ilev,:,:] = 0.
  if lev[ilev] > 300 and lev[ilev] < 500:
      data['qadvh'][:,ilev,:,:] = -1.2e-8*(lev[ilev]-500.)/(300.-500.)

data['thrad'] = data['trad']*1.
data['theta'] = data['temp']*1.
for ilev in range(0,nlev):
    data['thrad'][:,ilev,:,:] = data['trad'][:,ilev,:,:]*(100000./data['pressure'][:,ilev,:,:])**(2./7.)
    data['theta'][:,ilev,:,:] = data['temp'][:,ilev,:,:]*(100000./data['pressure'][:,ilev,:,:])**(2./7.)

data['thrad'].id = 'thrad'
data['theta'].id = 'theta'

g = cdms2.open('BOMEX_driver_MPL_RR.nc','w')

for var in ['height','pressure','ps','temp','theta','qv','u','v','w','trad','thrad','qadvh','ug','vg','sfc_sens_flx','sfc_lat_flx','ts']:
    if len(data[var].shape) == 4:
        data[var].setAxis(1,lev)
    data[var].setAxis(0,time)
    g.write(data[var])

g.comment = "Forcing and initial conditions for BOMEX case" ;
g.author = "MP Lefebvre (20170628), R. Roehrig (20180404)" ;
g.version = "20180404" ;
g.case = "BOMEX" ;
g.startDate = "19690624000000" ;
g.endDate = "19690624060000" ;
g.comment = """Forcing and initial confitions for BOMEX case
Large-scale subsidence w applied to qt, thetal, u, v
Radiative cooling dtheta/dt
Large-scale horizontal advection of qt
Sensible and latent heat fluxes imposed""" 
g.reference = "ftp://eos.atmos.washington.edu/pub/breth/papers/2003/BOMEX.pdf"
g.tadvh = 0
g.tadvv = 0
g.qadvh = 1
g.qadvv = 0
g.trad = 1
g.forc_w = 1
g.forc_omega = 0
g.forc_geo = 1
g.forc_ustar = 0
g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0
g.nudging_q = 0
g.zorog = 0.
g.z0 = 0.001019
#g.ustar = 0.28
g.surfaceType = "ocean"
g.surfaceForcing = "surfaceFlux"
g.surfaceForcingWind = "z0"

g.close()


