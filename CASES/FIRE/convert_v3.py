import cdms2 
import MV2
import cdtime
import math

import numpy as np
from scipy import interpolate

import vcs

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

x =vcs.init()

tunits = 'seconds since 1987-07-01 0:0:0.0'

tmin = cdtime.comptime(1987,7,14,8,0,0)
tmax = cdtime.comptime(1987,7,15,21,0,0)
#tmax = cdtime.comptime(1987,7,14,10,0,0)

time = MV2.array(range(0,2),typecode=MV2.float)
time[0] = tmin.torel(tunits).value
time[1] = tmax.torel(tunits).value
time = cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.units = tunits
time.calendar = 'gregorian'

lev = [0] + range(5,590,10) + range(590,611,1) + range(615,2000,5) + range(2000,20001,100)
lev = MV2.array(lev,typecode=MV2.float)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'lev'
lev.units = 'm'

lat = MV2.array(range(0,1),typecode=MV2.float) + 33.3
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.array(range(0,1),typecode=MV2.float) - 119.5
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

nt, = time.shape
nlev, = lev.shape
nlat, = lat.shape
nlon, = lon.shape

f = cdms2.open('../CINDY-DYNAMO/NSA3a/Standard_Atmosphere_zlevel.nc')
pstd = f('pp')
zstd = f('zz')*1000.
tstd = f('temp')
f.close()

ff = interpolate.interp1d(np.array(zstd),np.array(pstd))
pstd2 = ff(np.array(lev)) 

ff = interpolate.interp1d(np.array(zstd),np.array(tstd))
tstd2 = ff(np.array(lev))       

thstd2 = tstd2*0.
for ilev in range(0,nlev):
  thstd2[ilev] = tstd2[ilev]*(100000./pstd2[ilev])**(2./7)


titles = {}
titles['ts'] = 'Surface temperature'
titles['ps'] = 'Surface pressure'
titles['height'] = 'Height'
titles['pressure'] = 'Pressure'
titles['temp'] = 'Temperature'
titles['thl'] = 'Liquid potential temperature'
titles['u'] = 'Zonal wind'
titles['v'] = 'Meridional wind'
titles['ug'] = 'Zonal geostrophic wind'
titles['vg'] = 'Meridional geostrophic wind'
titles['qt'] = 'Total specific humidity'
titles['qv'] = 'Specific humidity'
titles['w'] = 'Vertical velocity'
titles['thladvh'] = 'Large scale horiz. adv. of thl'
titles['tadvh'] = 'Large scale horiz. adv. of temp'
titles['qtadvh'] = 'Large scale horiz. adv. of qt'


units = {}
units['ts'] = 'K'
units['ps'] = 'Pa'
units['height'] = 'm'
units['pressure'] = 'Pa'
units['temp'] = 'K'
units['thl'] = 'K'
units['u'] = 'm s-1'
units['v'] = 'm s-1'
units['ug'] = 'm s-1'
units['vg'] = 'm s-1'
units['qt'] = 'kg/kg'
units['qv'] = 'kg/kg'
units['w'] = 'm s-1'
units['thladvh'] = 'K s-1'
units['tadvh'] = 'K s-1'
units['qtadvh'] = 'kg kg-1 s-1'

variables4D = ['height','pressure','temp','thl','u','v','ug','vg','qt','qv','w','thladvh','tadvh','qtadvh']
#variables4D = ['height','pressure','temp','thl','u','v','ug','vg','qt','qv','w','thladvh','qtadvh']
variables3D = ['ts','ps']


data = {}

for var in variables4D:
    data[var] = MV2.zeros((nt,nlev,nlat,nlon),typecode=MV2.float)

for var in variables3D:
    data[var] = MV2.zeros((nt,nlat,nlon),typecode=MV2.float)

ps = 101250.

data['ts'] = data['ts'] + 289.
data['ps'] = data['ps'] + ps

alpha_geo = 305
data['u'] = data['u'] + 6*math.cos(math.pi/180.*alpha_geo)
data['ug'] = data['ug'] + 6*math.cos(math.pi/180.*alpha_geo)
data['v'] = data['v'] + 6*math.sin(math.pi/180.*alpha_geo)
data['vg'] = data['vg'] + 6*math.sin(math.pi/180.*alpha_geo)

for ilev in range(0,nlev):
    data['height'][:,ilev,:,:] = data['height'][:,ilev,:,:] + lev[ilev]
    if lev[ilev] <= 595:
        data['thl'][:,ilev,:,:] = data['thl'][:,ilev,:,:] + 287.5
        data['qt'][:,ilev,:,:] = data['qt'][:,ilev,:,:] + 0.0096
        data['thladvh'][:,ilev,:,:] = data['thladvh'][:,ilev,:,:] -7.5e-8*max(lev[ilev],500.)
        data['qtadvh'][:,ilev,:,:] = data['qtadvh'][:,ilev,:,:] +3.e-11*max(lev[ilev],500.)
        data['w'][:,ilev,:,:] = data['w'][:,ilev,:,:] - 1.e-5*lev[ilev]
    elif lev[ilev] <= 1200:
        data['thl'][:,ilev,:,:] = data['thl'][:,ilev,:,:] + 299.5 + 7.5e-3*(lev[ilev]-595)
        data['qt'][:,ilev,:,:] = data['qt'][:,ilev,:,:] + 0.0066 - 3.e-6*(lev[ilev]-595)
        data['thladvh'][:,ilev,:,:] = data['thladvh'][:,ilev,:,:] -7.5e-8*max(lev[ilev],500.)
        data['qtadvh'][:,ilev,:,:] = data['qtadvh'][:,ilev,:,:] +3.e-11*max(lev[ilev],500.)
        data['w'][:,ilev,:,:] = data['w'][:,ilev,:,:] - 1.e-5*lev[ilev]
    elif lev[ilev] <= 2000:
        data['thl'][:,ilev,:,:] = data['thl'][:,ilev,:,:] + 299.5 + 7.5e-3*(lev[ilev]-595)
        data['qt'][:,ilev,:,:] = data['qt'][:,ilev,:,:] + 0.0066 - 3.e-6*(lev[ilev]-595)
    else:
        data['thl'][:,ilev,:,:] = thstd2[ilev]+(299.5 + 7.5e-3*(2000-595)-thstd2[ilev])*math.exp(-(lev[ilev]-2000)/3000.)
        data['qt'][:,ilev,:,:] = 0. #(0.0066 - 3.e-6*(2000.-595.))*math.exp(-(lev[ilev]-2000)/3000.)

kappa = 2./7.
p0 = 100000.
g = 9.80665 
Rd = 287.
Rv = 462.

data['pressure'][:,0,:,:] = data['pressure'][:,0,:,:] + ps

for it in range(0,nt):
  for ilat in range(0,nlat):
    for ilon in range(0,nlon):
      integ = 0.
      for ilev in range(1,nlev):
        dz = lev[ilev]-lev[ilev-1]
        thl = (data['thl'][it,ilev-1,ilat,ilon]+data['thl'][it,ilev,ilat,ilon])/2.
        qt = (data['qt'][it,ilev-1,ilat,ilon]+data['qt'][it,ilev,ilat,ilon])/2.
        R = (1-qt)*Rd+qt*Rv
        integ = integ + g/(R*thl)*dz
        tmp = ps**kappa-p0**kappa*kappa*integ
        data['pressure'][it,ilev,ilat,ilon] = math.exp(math.log(tmp)/kappa)
 

for it in range(0,nt):
  for ilat in range(0,nlat):
    for ilon in range(0,nlon):
      for ilev in range(0,nlev):
        data['temp'][it,ilev,ilat,ilon] = data['thl'][it,ilev,ilat,ilon]*(data['pressure'][it,ilev,ilat,ilon]/100000.)**(2./7.)
        data['qv'][it,ilev,ilat,ilon] = data['qt'][it,ilev,ilat,ilon]
        data['tadvh'][it,ilev,ilat,ilon] = data['thladvh'][it,ilev,ilat,ilon]*(data['pressure'][it,ilev,ilat,ilon]/100000.)**(2./7.)

g = cdms2.open('Fire-I_driver_RR_v3.nc','w')
for var in variables4D + variables3D:
  if var in variables4D:
    data[var].setAxis(0,time)	  
    data[var].setAxis(1,lev)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
    x.plot(data[var][0,:,0,0],bg=1)
    x.png('images/' + var)
    x.clear()
  if var in variables3D:
    data[var].setAxis(0,time)
    data[var].setAxis(1,lat)
    data[var].setAxis(2,lon)
    print var, ':', data[var][0,0,0], units[var]

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


