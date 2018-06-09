import cdms2
import numpy
import MV2

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value)
cdms2.setNetcdfDeflateLevelFlag(value)

lat = -75.1
lon = 123.33

lat = cdms2.createAxis(MV2.array([lat,],typecode=MV2.float))
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = cdms2.createAxis(MV2.array([lon,],typecode=MV2.float))
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

variables = ['Ug', 'Vg','pf','height','theta','t','qv','u','v','hadvT','hadvQ','Tg','orog','psurf','z0m','emis','alb']

data = {}
attributes = {}

f = cdms2.open('GABLS4_SCM_LES_STAGE2.nc')
for var in variables:
    data[var] = f(var,squeeze=1)

attributes['startDate'] = '20091211000000'
attributes['endDate'] = '20091212120000'
attributes['author'] = 'E. Bazile, R. Roehrig'
attributes['surfaceForcing'] = 'ts'
attributes['surfaceType'] = 'land'
attributes['case'] = 'GABLS4'
attributes['description'] = 'No subsidence/ascendance, T & q Large-scale advection, geostrophic wind'
attributes['forc_geo'] = 1
attributes['zorog'] = 3223. #float(data['orog'])
attributes['z0'] = 0.001 #float(data['zo'])
attributes['ustar'] = 0 #float(data['ustar'])
attributes['alb'] = 0.81
attributes['emis'] = 0.98
attributes['trad'] = 0

attributes['qadvh'] = 1
attributes['tadvh'] = 1
attributes['tadvv'] = 0
attributes['qadvv'] = 0
attributes['uadvh'] = 0
attributes['vadvh'] = 0
attributes['uadvv'] = 0
attributes['vadvv'] = 0

attributes['forc_omega'] = 0
attributes['forc_w'] = 0
for s in ['u','v','t','q']:
    attributes['nudging_' + s] = 0

dico = {}
dico['u'] = 'u'
dico['v'] = 'v'
dico['ug'] = 'Ug'
dico['vg'] = 'Vg'
dico['ts'] = 'Tg'
dico['pressure'] = 'pf'
dico['qv'] = 'qv'
dico['temp'] = 't'
dico['ps'] = 'psurf'
dico['tadvh'] = 'hadvT'
dico['qadvh'] = 'hadvQ'

var2write = ['u','v','temp','qv','ug','vg','pressure','ps','ts','tadvh','qadvh']
var2write0 = ['ps']
var2write1 = ['ug','vg','tadvh','qadvh']

tmp = data[dico['ts']]
nt, = tmp.shape
time = tmp.getAxis(0)
time = cdms2.createAxis(time[:])
time.designateTime()
time.id = 'time'
time.units = 'seconds since 2009-12-11 00:00:0.0'

newdata = {}
for var in var2write:
  tmp = data[dico[var]]
  if var in ['ts']:
    tmp = MV2.reshape(tmp,(nt,1,1))
    tmp.setAxis(0,time)
    tmp.setAxis(1,lat)
    tmp.setAxis(2,lon)

    tmp.id = var
    tmp.long_name = data[dico[var]].long_name
    tmp.units = data[dico[var]].units

    newdata[var] = tmp
    
  elif not(var in var2write0) and not(var in var2write1):
    nlev, = tmp.shape
    lev = tmp.getAxis(0)
    lev = cdms2.createAxis(lev[:])
    lev.designateLevel()
    lev.id = 'lev'
    lev.units = 'm'
    lev.long_name = 'Altitude'
    tmp0 = MV2.zeros((nt,nlev,1,1),typecode=MV2.float32)
    for it in range(0,nt):
      tmp0[it,:,0,0] = tmp        
    tmp0.setAxis(0,time)
    tmp0.setAxis(1,lev)
    tmp0.setAxis(2,lat)
    tmp0.setAxis(3,lon)

    tmp0.id = var
    tmp0.long_name = data[dico[var]].long_name
    tmp0.units = data[dico[var]].units

    newdata[var] = tmp0
  elif var in var2write1:
    nt,nlev, = tmp.shape
    lev = tmp.getAxis(1)
    lev = cdms2.createAxis(lev[:])
    lev.designateLevel()
    lev.id = 'lev'
    lev.units = 'm'
    lev.long_name = 'Altitude'
    tmp0 = MV2.reshape(tmp,(nt,nlev,1,1))
    tmp0.setAxis(0,time)
    tmp0.setAxis(1,lev)
    tmp0.setAxis(2,lat)
    tmp0.setAxis(3,lon)

    tmp0.id = var
    tmp0.long_name = data[dico[var]].long_name
    tmp0.units = data[dico[var]].units

    newdata[var] = tmp0    
  else:
    tmp0 = MV2.zeros((nt,1,1),typecode=MV2.float32)
    for it in range(0,nt):
      tmp0[it,0,0] = tmp
    tmp0.setAxis(0,time)
    tmp0.setAxis(1,lat)
    tmp0.setAxis(2,lon)
    tmp0.id = var
    if var == 'ps':
      tmp0.long_name = 'surface pressure'
      tmp0.units = 'Pa'
    else:
      tmp0.long_name = data[dico[var]].long_name
      tmp0.units = data[dico[var]].units
    newdata[var] = tmp0


g = cdms2.open('GABLS4_SCM_LES_STAGE2_RR.nc','w')
for var in var2write:
    g.write(newdata[var])
for att in sorted(attributes.keys()):
    g.__setattr__(att,attributes[att])
g.close()
