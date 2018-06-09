import cdms2
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

nt = 728
dt = 3*3600

nlev = 41

lat = [0.1,]
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'

lon =[80.5,]
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'

time = MV2.zeros(nt,typecode=MV2.float32)
tunits = 'seconds since 2011-10-2 0:0:0.0'
time = cdms2.createAxis(time)
time.id = 'time'
time.designateTime()
time.units = tunits
time.calendar = 'gregorian'
for it in range(0,nt):
  time[it] = it*dt    

lev = MV2.array(range(0,nlev),typecode=MV2.float32)
lev = cdms2.createAxis(lev)
lev.id = 'lev'
lev.designateLevel()
lev.long_name = 'level number'
lev.units = '-'



f = cdms2.open('rev180varanaecmwfanatrmmC1.c1.20111001.000000.cdf')

data = {}
for var in f.listvariables():
  data[var] = f(var)

f.close()

datanew = {}

variables3D = ['u','v','T','q','omega','T_adv_h','q_adv_h','q1','q2']
variables2D = ['LH','SH','prec_srf','T_skin','q_srf','omega_srf','u_srf','v_srf','T_srf','p_srf_aver']

coefs = {}
for var in variables2D+variables3D:
    coefs[var] = 1

coefs['q'] = 1./1000.
coefs['omega'] = 100/3600.
coefs['T_adv_h'] = 1/3600.
coefs['q_adv_h'] = 1/3600./1000.
coefs['q1'] = 1/3600.*86400
coefs['q2'] = 1/3600.*86400
coefs['prec_srf'] = 24.
coefs['p_srf_aver'] = 100.

newvariables3D = ['u','v','temp','qv','omega','tadvh','qadvh','Q1','Q2']
newvariables2D = ['sfc_lat_flx','sfc_sens_flx','pr','ts','ps']

dico = {}
dico['u'] = 'u'
dico['v'] = 'v'
dico['temp'] = 'T'
dico['qv'] = 'q'
dico['omega'] = 'omega'
dico['tadvh'] = 'T_adv_h'
dico['qadvh'] = 'q_adv_h'
dico['Q1'] = 'q1'
dico['Q2'] = 'q2'
dico['ps'] = 'p_srf_aver'
dico['ts'] = 'T_skin'
dico['pr'] = 'prec_srf'
dico['sfc_lat_flx'] = 'LH'
dico['sfc_sens_flx'] = 'SH'

names = {}
names['temp'] = 'Temperature'
names['qv'] = 'Specific Humidity'
names['u'] = 'Zonal Wind'
names['v'] = 'Meridional Wind'
names['tadvh'] = 'Horizontal Temperature Advection'
names['qadvh'] = 'Horizontal Specific Humidity Advection'
names['omega'] = 'Vertical Pressure Velocity'
names['ps'] = 'Surface Pressure'
names['pressure'] = 'Pressure'
names['ts'] = 'Surface Temperature'
names['Q1'] = 'Q1'
names['Q2'] = 'Q2'
names['sfc_lat_flx'] = 'Latent heat flux'
names['sfc_sens_flx'] = 'Sensible heat flux'
names['pr'] = 'Precipitation'

units = {}
units['pressure'] = 'Pa'
units['u'    ] = 'm/s'
units['v'    ] = 'm/s'
units['omega'] = 'Pa/s'
units['temp' ] = 'K'
units['qv'   ] = 'kg/kg'
units['tadvh' ] = 'K/s'
units['qadvh' ] = 'kg/kg/s'
units['Q1'   ] = 'K/day'
units['Q2'   ] = 'K/day'
units['pr'] = 'mm/day'
units['sfc_lat_flx'] = 'W/m2'
units['sfc_sens_flx'] = 'W/m2'
units['ps'] = 'Pa'
units['ts'] = 'K'

tmp = data['u'].getLevel()
pres = MV2.zeros((nt,nlev,1,1),typecode=MV2.float32)
pres[:,0,0,0] = data['p_srf_aver'][:]*100.
for it in range(0,nt):
  pres[it,1:,0,0] = tmp[1:]*100.

pres.id = 'pressure'
pres.title = names['pressure']
pres.units = 'Pa'
pres.setAxis(0,time)
pres.setAxis(1,lev)
pres.setAxis(2,lat)
pres.setAxis(3,lon)

datanew['pressure'] = pres

for var in newvariables2D:
  tmp = MV2.reshape(data[dico[var]],(nt,1,1))*coefs[dico[var]]
  for att in tmp.listattributes():
      tmp.deleteattribute(att)
  tmp.setAxis(0,time)
  tmp.setAxis(1,lat)
  tmp.setAxis(2,lon)

  tmp.id = var
  tmp.title = names[var]
  tmp.units = units[var]

  if var == 'ts':
      tmp = tmp + 273.15

  datanew[var] = tmp



for var in newvariables3D:
  tmp = MV2.reshape(data[dico[var]],(nt,nlev,1,1))*coefs[dico[var]]
  for att in tmp.listattributes():
      tmp.deleteattribute(att)
  tmp.setAxis(0,time)
  tmp.setAxis(1,lev)
  tmp.setAxis(2,lat)
  tmp.setAxis(3,lon)

  tmp.id = var
  tmp.title = names[var]
  tmp.units = units[var]

  datanew[var] = tmp

datanew['u'][:,0,0,0] = data['u_srf'][:]
datanew['v'][:,0,0,0] = data['v_srf'][:] 
datanew['omega'][:,0,0,0] = data['omega_srf'][:]*coefs['omega']
datanew['temp'][:,0,0,0] = data['T_srf'][:] + 273.15
datanew['qv'][:,0,0,0] = data['q_srf'][:]

g = cdms2.open('CINDY-DYNAMO_Revelle-ARM-TRMM_driver_RR.nc','w')

for var in newvariables2D+newvariables3D+['pressure',]:
  g.write(datanew[var])


g.comment = 'Forcing and initial conditions for CINDY-DYNAMO Revelle ARM-TRMM case'
g.reference = 'TBD'
g.author = 'R. Roehrig'


g.case = 'CINDY-DYNAMO - Revelle ARM-TRMM'
g.startDate = '20111002000000'
g.endDate = '20111231210000'

g.qadvh = 1
g.tadvh = 1
g.tadvv = 0
g.qadvv = 0
g.trad = 0

g.forc_omega = 1
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 10800
g.nudging_v = 10800
g.nudging_t = 10800
g.nudging_q = 10800

g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 10000
g.p_nudging_q = 10000


g.zorog = 0.
g.z0 = 0.01
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'

g.close()

