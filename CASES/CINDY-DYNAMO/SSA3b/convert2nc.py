import MV2 
import cdms2
import cdtime
import math


cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

missing_value = -999.00

nt = 736
dt = 3*3600

nlev = 40

lat = [2.5,]
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'

lon =[77.5,]
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'

time = MV2.zeros(nt,typecode=MV2.float32)
tunits = 'seconds since 2011-10-1 0:0:0.0'
time = cdms2.createAxis(time)
time.id = 'time'
time.designateTime()
time.units = tunits
time.calendar = 'gregorian'

lev = MV2.array(range(0,nlev),typecode=MV2.float32)
lev = cdms2.createAxis(lev)
lev.id = 'lev'
lev.designateLevel()
lev.long_name = 'level number'
lev.units = '-'

variables3D = ['pp','zz','u','v','ug','vg','omega','temp','theta','qv','rv','rh','div','vort','advu','hu','vu','advv','hv','vv','advT','hT','vT','advq','hq','vq','advth','hth','vth','advr','hr','vr','Q1','Q2','qv2','rv2']

variables2D = ['pps','zzs','us','vs','omegas','ts','thetas','qvs','rvs','rhs','divs','vorts','advus','hus','vus','advvs','hvs','vvs','advTs','hTs','vTs','advqs','hqs','vqs','advths','hths','vths','advrs','hrs','vrs','Q1s','Q2s','flat','sens','po1','po2','Qrad','Qrad_CERES','pr','rvs2','qvs2']

units = {}

units['pp'   ] = 'hPa'
units['zz'   ] = 'm'
units['u'    ] = 'm/s'
units['v'    ] = 'm/s'
units['ug'    ] = 'm/s'
units['vg'    ] = 'm/s'
units['omega'] = 'Pa/s'
units['temp' ] = 'K'
units['theta'] = 'K'
units['qv'   ] = 'kg/kg'
units['qv2'  ] = 'kg/kg'
units['rv'   ] = 'kg/kg'
units['rv2'  ] = 'kg/kg'
units['rh'   ] = '%'
units['div'  ] = '1/s'
units['vort' ] = '1/s'
units['advu' ] = 'm/s2'
units['hu'   ] = 'm/s2'
units['vu'   ] = 'm/s2'
units['advv' ] = 'm/s2'
units['hv'   ] = 'm/s2'
units['vv'   ] = 'm/s2'
units['advT' ] = 'K/s'
units['hT'   ] = 'K/s'
units['vT'   ] = 'K/s'
units['advq' ] = 'kg/kg/s'
units['hq'   ] = 'kg/kg/s'
units['vq'   ] = 'kg/kg/s'
units['advth'] = 'K/s'
units['hth'  ] = 'K/s'
units['vth'  ] = 'K/s'
units['advr' ] = 'kg/kg/s'
units['hr'   ] = 'kg/kg/s'
units['vr'   ] = 'kg/kg/s'
units['Q1'   ] = 'K/day'
units['Q2'   ] = 'K/day'

units['pps'   ] = 'hPa'
units['zzs'   ] = 'm'
units['us'    ] = 'm/s'
units['vs'    ] = 'm/s'
units['ugs'   ] = 'm/s'
units['vgs'   ] = 'm/s'
units['omegas'] = 'Pa/s'
units['ts'    ] = 'K'
units['thetas'] = 'K'
units['qvs'   ] = 'kg/kg'
units['rvs'   ] = 'kg/kg'
units['qvs2'  ] = 'kg/kg'
units['rvs2'  ] = 'kg/kg'
units['rhs'   ] = '%'
units['divs'  ] = '1/s'
units['vorts' ] = '1/s'
units['advus' ] = 'm/s2'
units['hus'   ] = 'm/s2'
units['vus'   ] = 'm/s2'
units['advvs' ] = 'm/s2'
units['hvs'   ] = 'm/s2'
units['vvs'   ] = 'm/s2'
units['advTs' ] = 'K/s'
units['hTs'   ] = 'K/s'
units['vTs'   ] = 'K/s'
units['advqs' ] = 'kg/kg/s'
units['hqs'   ] = 'kg/kg/s'
units['vqs'   ] = 'kg/kg/s'
units['advths'] = 'K/s'
units['hths'  ] = 'K/s'
units['vths'  ] = 'K/s'
units['advrs' ] = 'kg/kg/s'
units['hrs'   ] = 'kg/kg/s'
units['vrs'   ] = 'kg/kg/s'
units['Q1s'   ] = 'K/day'
units['Q2s'   ] = 'K/day'
units['flat'  ] = 'W/m2'
units['sens'  ] = 'W/m2'
units['po1'   ] = 'mm/day'
units['po2'   ] = 'mm/day'
units['Qrad'  ] = 'W/m2'
units['Qrad_CERES'] = 'W/m2'
units['pr'    ] = 'mm/day'

names = {}

names['pp'   ] = 'Pressure'
names['zz'   ] = 'Altitude'
names['u'    ] = 'Zonal Wind'
names['v'    ] = 'Meridional Wind'
names['ug'   ] = 'Geostrophic Zonal Wind'
names['vg'   ] = 'Geostrophic Meridional Wind'
names['omega'] = 'Vertical Velocity'
names['temp' ] = 'Temperature'
names['theta'] = 'Potential Temperature'
names['qv2'  ] = 'Specific Humidity'
names['qv'   ] = 'Specific Humidity computed from RH'
names['rv2'  ] = 'Water Vapor Mixing Ratio'
names['rv'   ] = 'Water Vapor Mixing Ratio computed from RH'
names['rh'   ] = 'Relative Humidity'
names['div'  ] = 'Divergence'
names['vort' ] = 'Vorticity'
names['advu' ] = 'Total Advection of Zonal Wind'
names['hu'   ] = 'Horizontal Advection of Zonal Wind'
names['vu'   ] = 'Vertical Advection of Zonal Wind'
names['advv' ] = 'Total Advection of Meridional Wind'
names['hv'   ] = 'Horizontal Advection of Meridional Wind'
names['vv'   ] = 'Vertical Advection of Meridional Wind'
names['advT' ] = 'Total Advection of Temperature'
names['hT'   ] = 'Horizontal Advection of Temperature'
names['vT'   ] = 'Vertical Advection of Temperature'
names['advq' ] = 'Total Advection of Specific Humidity'
names['hq'   ] = 'Horizontal Advection of Specific Humidity'
names['vq'   ] = 'Vertical Advection of Specific Humidity'
names['advth'] = 'Total Advection of Potential Temperature'
names['hth'  ] = 'Horizontal Advection of Potential Temperature'
names['vth'  ] = 'Vertical Advection of Potential Temperature'
names['advr' ] = 'Total Advection of Water Vapor Mixing Ratio'
names['hr'   ] = 'Horizontal Advection of Water Vapor Mixing Ratio'
names['vr'   ] = 'Vertical Advection of Water Vapor Mixing Ratio'
names['Q1'   ] = 'Apparent Heat Source'
names['Q2'   ] = 'Apparent Moisture Sink'

names['pps'   ] = 'Surface Pressure'
names['zzs'   ] = 'Surface Altitude'
names['us'    ] = 'Surface Zonal Wind'
names['vs'    ] = 'Surface Meridional Wind'
names['ugs'   ] = 'Geostrophic Surface Zonal Wind'
names['vgs'   ] = 'Geostrophic Surface Meridional Wind'
names['omegas'] = 'Surface Vertical Velocity'
names['ts'    ] = 'Surface Temperature'
names['thetas'] = 'Surface Potential Temperature'
names['qvs'   ] = 'Surface Specific Humidity recomputed from RH'
names['rvs'   ] = 'Surface Water Vapor Mixing Ratio recomputed from RH'
names['qvs2'  ] = 'Surface Specific Humidity'
names['rvs2'  ] = 'Surface Water Vapor Mixing Ratio'
names['rhs'   ] = 'Surface Relative Humidity'
names['divs'  ] = 'Surface Divergence'
names['vorts' ] = 'Surface Vorticity'
names['advus' ] = 'Surface Total Advection of Zonal Wind'
names['hus'   ] = 'Surface Horizontal Advection of Zonal Wind'
names['vus'   ] = 'Surface Vertical Advection of Zonal Wind'
names['advvs' ] = 'Surface Total Advection of Meridional Wind'
names['hvs'   ] = 'Surface Horizontal Advection of Meridional Wind'
names['vvs'   ] = 'Surface Vertical Advection of Meridional Wind'
names['advTs' ] = 'Surface Total Advection of Temperature'
names['hTs'   ] = 'Surface Horizontal Advection of Temperature'
names['vTs'   ] = 'Surface Vertical Advection of Temperature'
names['advqs' ] = 'Surface Total Advection of Specific Humidity'
names['hqs'   ] = 'Surface Horizontal Advection of Specific Humidity'
names['vqs'   ] = 'Surface Vertical Advection of Specific Humidity'
names['advths'] = 'Surface Total Advection of Potential Temperature'
names['hths'  ] = 'Surface Horizontal Advection of Potential Temperature'
names['vths'  ] = 'Surface Vertical Advection of Potential Temperature'
names['advrs' ] = 'Surface Total Advection of Water Vapor Mixing Ratio'
names['hrs'   ] = 'Surface Horizontal Advection of Water Vapor Mixing Ratio'
names['vrs'   ] = 'Vertical Advection of Water Vapor Mixing Ratio'
names['Q1s'   ] = 'Surface Apparent Heat Source'
names['Q2s'   ] = 'Surface Apparent Moisture Sink'
names['flat'  ] = 'Surface Latent Heat Flux from WHOI OAFLUX'
names['sens'  ] = 'Surface Sensible Heat Flux from WHOI OAFLUX'
names['po1'   ] = 'Q1-Budget derived Precipitation'
names['po2'   ] = 'Q2-Budget derived Precipitation'
names['Qrad'  ] = 'Column Net Radiation from Combine Q1/Q2-budget residual using WHOI OAFLUX'
names['Qrad_CERES'] = 'Column Net Radiation from CERES'
names['pr'   ] = 'TRMM 3B42V7 Precipitation'


data = {}
for var in variables3D:
  data[var] = MV2.zeros((nt,nlev,1,1),typecode=MV2.float32) + missing_value

for var in variables2D:
  data[var] = MV2.zeros((nt,1,1),typecode=MV2.float32) + missing_value

f = open('fields.ssa_3b')
for it in range(0,nt):
  line = f.readline().split()

  year = 2000 + int(line[0])
  month = int(line[1])
  day = int(line[2])
  hour = int(line[3])

  tt = cdtime.comptime(year,month,day,hour)
  time[it] = tt.torel(tunits).value

#  print tt.tocomp()
  for ilev in range(0,nlev):
    line = f.readline().split()

    data['pp'   ][it,ilev,0,0] = float(line[0])
    data['zz'   ][it,ilev,0,0] = float(line[1])
    data['u'    ][it,ilev,0,0] = float(line[2])
    data['v'    ][it,ilev,0,0] = float(line[3])
    data['omega'][it,ilev,0,0] = float(line[4])
    data['temp' ][it,ilev,0,0] = float(line[5])
    data['theta'][it,ilev,0,0] = float(line[6]) 
    data['rv2'  ][it,ilev,0,0] = float(line[7])
    data['rh'   ][it,ilev,0,0] = float(line[8]) 
    data['div'  ][it,ilev,0,0] = float(line[9]) 
    data['vort' ][it,ilev,0,0] = float(line[10])

    if ilev == 0:
      data['pps'   ][it,0,0] = float(line[0])
      data['zzs'   ][it,0,0] = float(line[1])
      data['us'    ][it,0,0] = float(line[2])
      data['vs'    ][it,0,0] = float(line[3])
      data['omegas'][it,0,0] = float(line[4])
      data['ts'    ][it,0,0] = float(line[5])
      data['thetas'][it,0,0] = float(line[6]) 
      data['rvs2'  ][it,0,0] = float(line[7])
      data['rhs'   ][it,0,0] = float(line[8]) 
      data['divs'  ][it,0,0] = float(line[9]) 
      data['vorts' ][it,0,0] = float(line[10])	    

    
f.close()

f = open('lsf.ssa_3b')
for it in range(0,nt):
  line = f.readline().split()

  year = 2000 + int(line[0])
  month = int(line[1])
  day = int(line[2])
  hour = int(line[3])

  tt = cdtime.comptime(year,month,day,hour)
  time[it] = tt.torel(tunits).value

#  print tt.tocomp()
  for ilev in range(0,nlev):
    line = f.readline().split()

    data['hT'][it,ilev,0,0] = float(line[1])
    data['vT'][it,ilev,0,0] = float(line[2])
    data['hq'][it,ilev,0,0] = float(line[3])
    data['vq'][it,ilev,0,0] = float(line[4])

    if ilev == 0:
      data['hTs'][it,0,0] = float(line[1])
      data['vTs'][it,0,0] = float(line[2])
      data['hqs'][it,0,0] = float(line[3])
      data['vqs'][it,0,0] = float(line[4])
    
f.close()

f = open('q1q2.ssa_3b')
for it in range(0,nt):
  line = f.readline().split()

  year = 2000 + int(line[0])
  month = int(line[1])
  day = int(line[2])
  hour = int(line[3])

  tt = cdtime.comptime(year,month,day,hour)
  time[it] = tt.torel(tunits).value

#  print tt.tocomp()
  for ilev in range(0,nlev):
    line = f.readline().split()

    data['Q1'][it,ilev,0,0] = float(line[1])
    data['Q2'][it,ilev,0,0] = float(line[2])

    if ilev == 0:
      data['Q1s'][it,0,0] = float(line[1])
      data['Q2s'][it,0,0] = float(line[2])
    
f.close()

f = open('eopo.ssa_3b')
for it in range(0,nt):
  line = f.readline().split()

  year = 2000 + int(line[0])
  month = int(line[1])
  day = int(line[2])
  hour = int(line[3])

  tt = cdtime.comptime(year,month,day,hour)
  time[it] = tt.torel(tunits).value

#  print tt.tocomp()
  
  data['flat'][it,0,0] = float(line[4])
  data['po2' ][it,0,0] = float(line[5])
  data['sens'][it,0,0] = float(line[6])
  data['po1' ][it,0,0] = float(line[7])
  data['Qrad'][it,0,0] = float(line[8])
  data['Qrad_CERES'][it,0,0] = float(line[9])
    
f.close()

f = open('trmm_rain')
for it in range(0,nt):
  line = f.readline().split()

  data['pr'][it,0,0] = float(line[2])
    
f.close()

for var in variables3D:
 print var
 tmp = MV2.masked_values(data[var],missing_value)
 tmp.missing_value = missing_value
 count = MV2.count(tmp)
 if not(count == nt*nlev) and not(count == 0):
  for it in range(0,nt):
    for ilev in range(1,nlev-1):
      if data[var][it,ilev,0,0] == missing_value :
        print 'missing', var, it, ilev	      
        if data[var][it,ilev+1,0,0] <> missing_value and data[var][it,ilev-1,0,0] <> missing_value:
          data[var][it,ilev,0,0] = data[var][it,ilev-1,0,0]+(data['pp'][it,ilev-1,0,0]-data['pp'][it,ilev,0,0])*(data[var][it,ilev-1,0,0]-data[var][it,ilev+1,0,0])/(data['pp'][it,ilev-1,0,0]-data['pp'][it,ilev+1,0,0])

for var in variables2D:
 print var	
 tmp = MV2.masked_values(data[var],missing_value)
 tmp.missing_value = missing_value
 count = MV2.count(tmp)
 if not(count == nt)  and not(count == 0):	
  for it in range(1,nt-1):
    if data[var][it,0,0] == missing_value :
      print 'missing', var, it
      if data[var][it+1,0,0] <> missing_value and data[var][it-1,0,0] <> missing_value:
        data[var][it,0,0] = data[var][it-1,0,0]+(time[it-1,0,0]-time[it,0,0])*(data[var][it-1,0,0]-data[var][it+1,0,0])/(time[it-1,0,0]-time[it+1,0,0])

for var in variables3D:
  data[var] = MV2.masked_values(data[var],missing_value)
  data[var].missing_value = missing_value

data['omega'] = data['omega'] * 100./3600.
data['temp' ] = data['temp' ] + 273.16
data['rv2'  ] = data['rv2'  ] / 1000.
data['div'  ] = data['div'  ] / 1.e6
data['vort' ] = data['vort' ] / 1.e6
data['qv2'  ] = data['rv2']/(1.+data['rv2'])
data['hT'   ] = data['hT'] * -1
data['vT'   ] = data['vT'] * -1
data['hq'   ] = data['hq'] / 1000. * -1
data['vq'   ] = data['vq'] / 1000. * -1

data['advT' ] = data['hT'] + data['vT']
data['advq' ] = data['hq'] + data['vq']


###########
# Recalcul de rv/qv a partir de RH
# a partir de http://climatologie.u-bourgogne.fr/data/matlab/goff_gratch.m


tempK = data['temp']*1. # Temperature en K
# tension de vapeur saturante
# par rapport a la glace 
eilog = -9.09718 * ((273.16/tempK) -1.)
eilog2 = -3.5654 * MV2.log10(273.16/tempK)
eilog3 = 0.876793 * (1. - (tempK/273.16))
es1=6.1071*MV2.exp((eilog+eilog2+eilog3)*math.log(10.))

# par rapport a l'eau 
eilog=-7.90298*((373.16/tempK) - 1.)
eilog2=5.02808*MV2.log10((373.16/tempK))
eilog3=-1.3816e-7*(MV2.exp((11.344*(1.-(tempK/373.16)))*math.log(10.)) -1.)
eilog4=8.1328e-3*(MV2.exp((-3.49149*((373.16/tempK) - 1.) )*math.log(10)) -1.)
es2=1013.246*MV2.exp((eilog+eilog2+eilog3+eilog4)*math.log(10.))

es = MV2.where(tempK < 273.15, es1,es2)

# masse
ws = 0.62197* es/(data['pp'] - 0.378*es) # pression en hPa

# humidite specifique
data['qv'] = (data['rh']/100.0)*ws # rh en %, qv en kg/kg

data['rv'] = data['qv']/(1.-data['qv'])

data['qvs'] = data['qv'][:,0,:,:]
data['rvs'] = data['rv'][:,0,:,:]

###########


for var in variables3D:
  data[var].setAxis(0,time)
  data[var].setAxis(1,lev)
  data[var].setAxis(2,lat)
  data[var].setAxis(3,lon)
  data[var].id = var
  data[var].units = units[var]
  data[var].long_name = names[var]
  data[var].title = var
  data[var].positive = ""



for var in variables2D:
  data[var] = MV2.masked_values(data[var],missing_value)
  data[var].missing_value = missing_value

data['omegas'] = data['omegas'] * 100./3600.
data['ts'    ] = data['ts'    ] + 273.16
data['rvs2'  ] = data['rvs2'  ] / 1000.
data['divs'  ] = data['divs'  ] / 1.e6
data['vorts' ] = data['vorts' ] / 1.e6
data['qvs2'  ] = data['rvs2']/(1.+data['rvs2'])
data['hTs'   ] = data['hTs'] * -1
data['vTs'   ] = data['vTs'] * -1
data['hqs'   ] = data['hqs'] / 1000. * -1
data['vqs'   ] = data['vqs'] / 1000. * -1
data['flat'  ] = data['flat'] /3.45*100.
data['sens'  ] = data['sens'] /3.45*100.
data['Qrad'  ] = data['Qrad'] /3.45*100.
data['Qrad_CERES'] = data['Qrad'] /3.45*100.

data['advTs' ] = data['hTs'] + data['vTs']
data['advqs' ] = data['hqs'] + data['vqs']

for var in variables2D:
  data[var].setAxis(0,time)
  data[var].setAxis(1,lat)
  data[var].setAxis(2,lon)
  data[var].id = var
  data[var].units = units[var]
  data[var].long_name = names[var]
  data[var].title = var
  data[var].positive = ""

g = cdms2.open('cindy_ssa3b.nc','w')
for var in variables3D:
  g.write(data[var])
for var in variables2D:
  g.write(data[var])


# Forcing

g.tendTquvw = '1 1 0 0 -1'
g.nudgingTquv = '0 0 10800 10800'
g.surfaceForcing = 'ts' 

g.close()
