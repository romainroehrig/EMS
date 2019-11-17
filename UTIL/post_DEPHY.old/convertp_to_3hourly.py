import cdms2
import MV2
import cdtime
#from sets import Set

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

rep0 = './'

nt = 100*24/3

f = cdms2.open(rep0 + '/netcdf/Out_plevel.nc')
tmp = f.getAxis('time')
dt = tmp[1]-tmp[0]
t0 = cdtime.reltime(tmp[0]-dt/2.,tmp.units)

unitsTime = tmp.units.replace('seconds','hours') #'hours since 2011-10-01 0:0:0.0'

time0 = cdms2.createAxis(MV2.array(range(0,nt),typecode=MV2.float32))
for it in range(0,nt):
#  tt = cdtime.comptime(2011,10,1,0,0,0)
#  tt = tt.add(3.*it+1.5,cdtime.Hour)
  tt = t0.add(3.*it+1.5,cdtime.Hour)
  time0[it] = tt.torel(unitsTime).value

time0.designateTime()
time0.id = 'time'
time0.units = unitsTime
time0.calendar = 'gregorian'

f = cdms2.open(rep0 + '/netcdf/Out_plevel.nc')

g = cdms2.open(rep0 + '/netcdf/Out_3hourly_plevel.nc','w')

for var in f.listvariables():
 if not(var in ['bounds_time','boxtauisccp','boxptopisccp','fracout','atb532','cfadLidarsr532','dbze94','cfadDbze94','clisccp','clmodis','clMISR']):
  print var
  data = 0.
  rms = 0.

  tmp = f(var, squeeze=1)

  nt0 = tmp.shape[0]
  nt = nt0/(3*4)-1
  nt0 = nt*3*4+1
  tmp = tmp[0:nt0]

  for it in range(0,4*3+1):
    if it == 0:
      data = data + tmp[0:nt0-1:4*3]/(8*3.)
      rms = rms + tmp[0:nt0-1:4*3]*tmp[0:nt0-1:4*3]/(8*3.)
    elif it == 4*3:
      data = data + tmp[4*3:nt0:4*3]/(8*3.)
      rms = rms + tmp[4*3:nt0:4*3]*tmp[4*3:nt0:4*3]/(8*3.)
    else:
      data = data + tmp[it:nt0-1:4*3]/(4*3.)
      rms = rms + tmp[it:nt0-1:4*3]*tmp[it:nt0:4*3]/(4*3.)

  time = time0[0:nt]
  time = cdms2.createAxis(time)
  time.designateTime()
  time.id = 'time'
  time.units = unitsTime
  time.calendar = 'gregorian'

  data.setAxis(0,time)
  rms.setAxis(0,time)

  if len(tmp.shape) == 2 and tmp.shape[1] >= 31:
    lev = tmp.getLevel()	 
    data.setAxis(1,lev)
    rms.setAxis(1,lev)

  data.id = var
  data.units = tmp.units
  data.long_name = tmp.long_name

  rms = MV2.sqrt(rms)
  rms.id = 'rms_' + var
  rms.units = tmp.units
  rms.name = 'RMS of ' + tmp.long_name

  g.write(data)
  g.write(rms)

  del(data)

g.close()




	  

