import cdms2
import MV2
import cdtime

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)


lat = 0.1
lon = 80.5

latmin = lat - 1.5
latmax = lat + 1.5
lonmin = lon - 1.5
lonmax = lon + 1.5
levmin = 14.
levmax = 0.


f = cdms2.open('CINDY-DYNAMO_Revelle-ARM-CSU_driver_RR.nc')
variables = f.listvariables()

f1 = cdms2.open('thermo_ERAI_6hourly.nc')
ta = f1('t', latitude = (latmin,latmax), longitude = (lonmin,lonmax), level = (levmin,levmax))
ta = MV2.average(ta,axis=3)
ta = MV2.average(ta,axis=2)

nlevp = ta.shape[1]
levp = ta.getLevel()

hus = f1('q', latitude = (latmin,latmax), longitude = (lonmin,lonmax), level = (levmin,levmax))
hus = MV2.average(hus,axis=3)
hus = MV2.average(hus,axis=2)



fw = cdms2.open('wind_ERAI_6hourly.nc')
ua = fw('u', latitude = (latmin,latmax), longitude = (lonmin,lonmax), level = (levmin,levmax))
ua = MV2.average(ua,axis=3)
ua = MV2.average(ua,axis=2)

va = fw('v', latitude = (latmin,latmax), longitude = (lonmin,lonmax), level = (levmin,levmax))
va = MV2.average(va,axis=3)
va = MV2.average(va,axis=2)

wap = fw('w', latitude = (latmin,latmax), longitude = (lonmin,lonmax), level = (levmin,levmax))
wap = MV2.average(wap,axis=3)
wap = MV2.average(wap,axis=2)



f2 = cdms2.open('Standard_Atmosphere_plevel.nc')
ta2 = f2('temp',level=(101,0))
zz2 = f2('zz',level=(101,0))*1000.

nlevp2 = ta2.shape[0]
levp2 = ta2.getLevel()


dataext = {}
dataext['temp'] = ta
dataext['u'] = ua
dataext['v'] = va
dataext['omega'] = wap
dataext['qv'] = hus

dataext2 = {}
dataext2['temp'] = ta2

g = cdms2.open('CINDY-DYNAMO_Revelle-ARM-CSU_driver_RR_extended.nc','w')

pp = f('pressure')
nt,nlev,nlat,nlon = pp.shape
time = pp.getTime()
lev = pp.getLevel()
lat = pp.getLatitude()
lon = pp.getLongitude()
levnew = MV2.array(range(0,nlev+nlevp+nlevp2),typecode=MV2.int)
levnew = cdms2.createAxis(levnew)
levnew.designateLevel()
levnew.id = 'lev'
levnew.units = '-'
levnew.long_name = 'level number'

for var in variables:
  print var
  data = f(var)
  if len(data.shape) == 3:
    g.write(data)
  elif len(data.shape) == 4:
    data = f(var)
    datanew = MV2.zeros((nt,nlev+nlevp+nlevp2,nlat,nlon),typecode=MV2.float)
    datanew[:,0:nlev,:,:] = data[:,:,:,:]
    if var == 'pressure':
      for ilev in range(0,nlevp):
        datanew[:,nlev+ilev,:,:] = datanew[:,nlev+ilev,:,:] + levp[ilev]*100.
      for ilev in range(0,nlevp2):
        datanew[:,nlev+nlevp+ilev,:,:] = datanew[:,nlev+nlevp+ilev,:,:] + levp2[ilev]
    elif var in ['temp','u','v','qv']:	    
      for it in range(0,nt):
        tt = cdtime.reltime(time[it],time.units)
	if time[it] % 21600 == 0:
          for ilev in range(0,nlevp):
            datanew[it,nlev+ilev,:,:] = datanew[it,nlev+ilev,:,:]+dataext[var](time = tt)[0,ilev]
	else:
          for ilev in range(0,nlevp):
            t1 = tt.add(-3,cdtime.Hour)		  
	    t2 = tt.add(3,cdtime.Hour)
            datanew[it,nlev+ilev,:,:] = datanew[it,nlev+ilev,:,:]+(dataext[var](time = t1)[0,ilev]+dataext[var](time = t2)[0,ilev])/2.
      if var in ['temp']:
        for ilev in range(0,nlevp2):	      
          datanew[:,nlev+nlevp+ilev,:,:] = datanew[:,nlev+nlevp+ilev,:,:] + dataext2[var][ilev]

    datanew.setAxis(0,time)
    datanew.setAxis(1,levnew)
    datanew.setAxis(2,lat)
    datanew.setAxis(3,lon)
    datanew.id = data.id
    datanew.title = data.title
    datanew.units = data.units
    datanew.missing_value = data.missing_value

    g.write(datanew)



g.comment = 'Forcing and initial conditions for CINDY-DYNAMO Revelle ARM-CSU case'
g.reference = 'TBD'
g.author = 'R. Roehrig'


g.case = 'CINDY-DYNAMO - Revelle ARM-CSU'
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
g.s0 = 0.1
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'

g.close()
f.close()
