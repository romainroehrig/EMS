import cdms2
import MV2
import cdtime
#from sets import Set

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

rep0 = './'

f = cdms2.open(rep0 + '/netcdf/Out_plevel.nc')

time0 = f('pf').getTime()

nt0 = time0.shape[0]
t0 = cdtime.reltime(time0[0],time0.units)
dt = time0[1]-time0[0]

nt = int(nt0*dt/86400)-1

kt = int(86400/dt)
nt1 = kt*nt


time = cdms2.createAxis(MV2.array(range(0,nt),typecode=MV2.float32))
for it in range(0,nt):
  tt = t0.add(it+0.5,cdtime.Day)
  time[it] = tt.torel(time0.units).value

time.designateTime()
time.id = 'time'
time.units = time0.units
time.calendar = 'gregorian'

g = cdms2.open(rep0 + '/netcdf/Out_daily_plevel.nc','w')

for var in f.listvariables():
 if not(var in ['bounds_time','boxtauisccp','boxptopisccp','fracout','atb532','cfadLidarsr532','dbze94','cfadDbze94','clisccp','clmodis','clMISR']):
  print var
  data = 0.
  rms = 0.

  tmp = f(var, squeeze=1)

  for it in range(0,kt+1):
    if it == 0:
      data = data + tmp[0:nt1-1:kt]/(kt*2.)
      rms = rms + tmp[0:nt1-1:kt]*tmp[0:nt1-1:kt]/(kt*2.)
    elif it == kt:
      data = data + tmp[kt:nt1+1:kt]/(kt*2.)
      rms = rms + tmp[kt:nt1+1:kt]*tmp[kt:nt1+1:kt]/(kt*2.)
    else:
      data = data + tmp[it:nt1:kt]/(kt*1.)
      rms = rms + tmp[it:nt1:kt]*tmp[it:nt1:kt]/(kt*1.)

  data.setAxis(0,time)
  rms.setAxis(0,time)

  if len(tmp.shape) == 2 and tmp.shape[1] >= 10:
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




	  

