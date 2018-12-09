import cdms2 
import MV2
import vcs

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

x =vcs.init()


f = cdms2.open('FIRE_I_driver.nc')

data = {}
for var in f.listvariables():
  data[var] = f(var)

time = data['ps'].getTime()
time.calendar = 'gregorian'

lev = data['pressure'].getAxis(0)
nlev, = lev.shape
lev.designateLevel()

lat = data['ps'].getAxis(1)
nlat, = lat.shape
lat.units = 'degrees_north'
lat.designateLatitude()

lon = data['ps'].getAxis(2)
nlon, = lon.shape
lon.units = 'degrees_east'
lon.designateLongitude()

titles = {}
titles['ts'] = 'Surface temperature'
titles['rugos'] = 'Surface rugosity'
titles['ps'] = 'Surface pressure'
titles['fcorio'] = 'Coriolis parameter - 2*Omega*sin(phi)'
titles['height'] = 'Height'
titles['pressure'] = 'Pressure'
titles['temp'] = 'Temperature'
titles['theta'] = 'Potential temperature'
titles['thv'] = 'Virtual potential temperature'
titles['thl'] = 'Liquid potential temperature'
titles['ths'] = 'Theta_s'
titles['u'] = 'Zonal wind'
titles['v'] = 'Meridional wind'
titles['ug'] = 'Zonal geostrophic wind'
titles['vg'] = 'Meridional geostrophic wind'
titles['qt'] = 'Total specific humidity'
titles['qv'] = 'Specific humidity'
titles['ql'] = 'Specific liquid moisture'
titles['rh'] = 'Relative humidity'
titles['w'] = 'Vertical velocity'
titles['thladvh'] = 'Large scale horiz. adv. of thl'
titles['tadvh'] = 'Large scale horiz. adv. of temp'
titles['qtadvh'] = 'Large scale horiz. adv. of qt'


units = {}
units['ts'] = 'K'
units['rugos'] = 'm'
units['ps'] = 'Pa'
units['fcorio'] = 's-1'
units['height'] = 'm'
units['pressure'] = 'Pa'
units['temp'] = 'K'
units['theta'] = 'K'
units['thv'] = 'K'
units['thl'] = 'K'
units['ths'] = 'K'
units['u'] = 'm s-1'
units['v'] = 'm s-1'
units['ug'] = 'm s-1'
units['vg'] = 'm s-1'
units['qt'] = 'kg/kg'
units['qv'] = 'kg/kg'
units['ql'] = 'kg/kg'
units['rh'] = '-'
units['w'] = 'm s-1'
units['thladvh'] = 'K s-1'
units['tadvh'] = 'K s-1'
units['qtadvh'] = 'kg kg-1 s-1'

variables4D = ['height','pressure','temp','theta','thv','thl','ths','u','v','ug','vg','qt','qv','ql','rh','w','thladvh','tadvh','qtadvh']
variables3D = ['ts','ps','fcorio','rugos']

g = cdms2.open('Fire-I_driver_RR.nc','w')
for var in f.listvariables():
  if var in variables4D:
    data[var] = data[var].reshape((1,nlev,nlat,nlon))	  
    data[var].setAxis(0,time)	  
    data[var].setAxis(1,lev)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
    x.plot(data[var](squeeze=1),bg=1)
    x.png('images/' + var)
    x.clear()
  if var in variables3D:
    data[var] = data[var].reshape((1,nlat,nlon))	  
    data[var].setAxis(0,time)
    data[var].setAxis(1,lat)
    data[var].setAxis(2,lon)
    print var, ':', data[var](squeeze=1), units[var]

  data[var].id = var
  data[var].title = titles[var]
  data[var].units = units[var]
  data[var].positive = ''

  g.write(data[var])

g.comment = 'Forcing and initial conditions for FIRE-I case'
g.reference = ''
g.author = 'P. Marquet, R. Roehrig'
g.date = 'Date de creation : 20160304'


g.case = 'FIRE-I'
g.startDate = '19870714080000'
g.endDate = '19870715210000'

g.tadvh = 1
g.qadvh = 0
g.thladvh = 1
g.qtadvh = 1
g.tadvv = 0
g.qadvv = 0
g.thladvv = 0
g.qtadvv = 0
g.trad = 0

g.forc_omega = 0
g.forc_w = 1

g.forc_geo = 1

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0
g.nudging_q = 0

g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'
g.z0 = 0.0004

g.comment = """
  Forcing: Large scale subsidence in w
           Large scale forcing for theta_l in thladvh and tadvh
	   Large scale total water forcing in qtadvh
	   A Coriolis force with (ug,vg) and fcorio
	   The roughness length is in z0
---------------------------------------
Valeurs issues de : Duynkerke
van Zanten and van Dijk.
FIRE I observations for the model
inter comparison of EUROCS
Stratocumulus - V1.1 Nov.2000
---------------------------------------
Interpolations lineaires entre :
z m    Theta_l (K)  q_t  (g/kg)
    0  287.50       9.6
  595  287.50       9.6
  595  299.50       6.6
 1200  304.0375     4.785
 2095  310.75       2.1
 3153  317.29       1.481
 5900  326.75       0.379
 7600  329.28       0.098
 9670  336.61       0.005 <- 0.020
13000  348.4        0.001 <-
dessus    extr.lin   0.001 <-
---------------------------------------
mofifications au-dessus de 9670 m
pour eviter des nuages strato.
---------------------------------------
"""

g.close()

f.close()


