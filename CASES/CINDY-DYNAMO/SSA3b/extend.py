import cdms2
import MV2
import cdtime

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)


latmin = -8
latmax = 0
lonmin = 72
lonmax = 80
levmin = 49.
levmax = 0.


f = cdms2.open('cindy_ssa3b.nc')
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

hur = f1('r', latitude = (latmin,latmax), longitude = (lonmin,lonmax), level = (levmin,levmax))
hur = MV2.average(hur,axis=3)
hur = MV2.average(hur,axis=2)



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

zz = fw('z', latitude = (latmin,latmax), longitude = (lonmin,lonmax), level = (levmin,levmax))/9.81
zz = MV2.average(zz,axis=3)
zz = MV2.average(zz,axis=2)


f2 = cdms2.open('Standard_Atmosphere_plevel.nc')
ta2 = f2('temp',level=(99,0))   # highest level in ERAI : 1hPa = 100 Pa
zz2 = f2('zz',level=(99,0))*1000.

nlevp2 = ta2.shape[0]
levp2 = ta2.getLevel()


dataext = {}
dataext['temp'] = ta
dataext['u'] = ua
dataext['v'] = va
dataext['omega'] = wap
dataext['zz'] = zz
dataext['qv'] = hus
dataext['rh'] = hur

dataext2 = {}
dataext2['temp'] = ta2
dataext2['zz'] = zz2

g = cdms2.open('cindy_ssa3b_extended.nc','w')

pp = f('pp')
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
    if var == 'pp':
      for ilev in range(0,nlevp):
        datanew[:,nlev+ilev,:,:] = datanew[:,nlev+ilev,:,:] + levp[ilev]
      for ilev in range(0,nlevp2):
        datanew[:,nlev+nlevp+ilev,:,:] = datanew[:,nlev+nlevp+ilev,:,:] + levp2[ilev]/100.
#    elif var in ['temp','u','v','omega','zz','qv','rh']:
    elif var in ['temp','u','v','zz','qv','rh']:	    
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
      if var in ['temp','zz']:
        for ilev in range(0,nlevp2):	      
          datanew[:,nlev+nlevp+ilev,:,:] = datanew[:,nlev+nlevp+ilev,:,:] + dataext2[var][ilev]

    datanew.setAxis(0,time)
    datanew.setAxis(1,levnew)
    datanew.setAxis(2,lat)
    datanew.setAxis(3,lon)
    datanew.id = data.id
    datanew.title = data.title
    datanew.positive = data.positive
    datanew.long_name = data.long_name
    datanew.units = data.units
    datanew.missing_value = data.missing_value

    g.write(datanew)



g.tendTquvw = '1 1 0 0 -1'
g.nudgingTquv = '0 0 10800 10800'
g.surfaceForcing = 'ts' 

g.close()
f.close()
