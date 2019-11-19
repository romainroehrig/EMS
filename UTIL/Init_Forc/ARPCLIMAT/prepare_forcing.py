import cdms2
import MV2
import numpy as np
import sys
import config
import cdtime

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

lDEPHY = config.lDEPHY

dt = int(config.dt)

dirout = 'files_L' + str(config.nlev) + '_' + str(int(dt)) + 's/'

f = cdms2.open('data_input.nc')

startDate = f.startDate
yyyy1 = int(startDate[0:4])
mm1 = int(startDate[4:6])
dd1 = int(startDate[6:8])
hh1 = int(startDate[8:10])
mi1 = int(startDate[10:12])
ss1 = int(startDate[12:14])

tstart = cdtime.comptime(yyyy1,mm1,dd1,hh1,mi1,ss1)

endDate = f.endDate
yyyy2 = int(endDate[0:4])
mm2 = int(endDate[4:6])
dd2 = int(endDate[6:8])
hh2 = int(endDate[8:10])
mi2 = int(endDate[10:12])
ss2 = int(endDate[12:14])

tend = cdtime.comptime(yyyy2,mm2,dd2,hh2,mi2,ss2)

attributes = {}
for att in ['tadvh','qadvh','qvadvh','qvadv','qvadvv','qtadvh','uadvh','vadvh','tadvv','qadvv','qtadvv','uadvv','vadvv','tadv','qadv','uadv','vadv','trad','forc_omega','forc_w','forc_geo','nudging_t','nudging_q','nudging_u','nudging_v']:
  attributes[att] = 0

for att in f.listglobal():
  attributes[att] = f.getglobal(att)

#nt = f['temp'].shape[0]

time0 = f.getAxis('time')

nt = time0.shape[0]

variables3D = []

if attributes['trad'] == 1:
  if not(attributes['tadv'] == 1 or attributes['tadvh'] == 1 or attributes['tadvv'] == 1):
    variables3D.append('trad')
    nt = f['trad'].shape[0]
if attributes['tadv'] == 1:
  variables3D.append('tadv')
  nt = f['tadv'].shape[0]
if attributes['qadv'] == 1:
  variables3D.append('qadv')
  nt = f['qadv'].shape[0]
if attributes['qvadv'] == 1:
  variables3D.append('qvadv')
  nt = f['qvadv'].shape[0]
if attributes['tadvh'] == 1:
  variables3D.append('tadvh')
  nt = f['tadvh'].shape[0]
if attributes['qadvh'] == 1:
  variables3D.append('qadvh')
  nt = f['qadvh'].shape[0]
if attributes['qvadvh'] == 1:
  variables3D.append('qvadvh')
  nt = f['qvadvh'].shape[0]
if attributes['qtadvh'] == 1:
  variables3D.append('qadvh')
  nt = f['qtadvh'].shape[0]
if attributes['uadvh'] == 1:
  variables3D.append('uadvh')
  nt = f['uadvh'].shape[0]
if attributes['vadvh'] == 1:
  variables3D.append('vadvh')
  nt = f['vadvh'].shape[0]
if attributes['tadvv'] == 1:
  variables3D.append('tadvv')	
if attributes['qadvv'] == 1:
  variables3D.append('qadvv')	
if attributes['qvadvv'] == 1:
  variables3D.append('qvadvv')	
if attributes['qtadvv'] == 1:
  variables3D.append('qadvv')
if attributes['uadvv'] == 1:
  variables3D.append('uadvv')	
if attributes['vadvv'] == 1:
  variables3D.append('vadvv')	
if attributes['forc_omega'] == 1:
  variables3D.append('omega')
  nt = f['omega'].shape[0]
if attributes['forc_w'] == 1:
  variables3D.append('w')
  nt = f['w'].shape[0]
if attributes['forc_geo'] == 1:
  variables3D.append('ug')
  nt = f['ug'].shape[0]
  variables3D.append('vg')	

if lDEPHY:
  if attributes['nudging_u'] > 0.:
    variables3D.append('u_nudg')
    nt = f['u_nudg'].shape[0]
  if attributes['nudging_v'] > 0.:
    variables3D.append('v_nudg')
    nt = f['v_nudg'].shape[0]
  if attributes['nudging_t'] > 0.:
    variables3D.append('temp_nudg')
    nt = f['temp_nudg'].shape[0]
  if attributes['nudging_qv'] > 0.:
    variables3D.append('qv_nudg')
    nt = f['qv_nudg'].shape[0]
else:
  if attributes['nudging_u'] > 0.:
    variables3D.append('u')
    nt = f['u'].shape[0]
  if attributes['nudging_v'] > 0.:
    variables3D.append('v')
    nt = f['v'].shape[0]
  if attributes['nudging_t'] > 0.:
    variables3D.append('temp')
    nt = f['temp'].shape[0]
  if attributes['nudging_q'] > 0.:
    variables3D.append('qv')
    nt = f['qv'].shape[0]

f.close()

variables3D = set(variables3D)


#variables3D = ['T','q','u','v','omega','hT','hq']

variables2D = ['ps']

names = {}

for var in variables3D:
  names[var] = var	

names['ps'] = 'Ps'
names['trad'] = 'dT'
names['tadvh'] = 'dT'
names['qadvh'] = 'dq'
names['qvadvh'] = 'dq'
names['uadvh'] = 'du'
names['vadvh'] = 'dv'
names['tadvv'] = 'dT'
names['qadvv'] = 'dq'
names['qvadvv'] = 'dq'
names['uadvv'] = 'du'
names['vadvv'] = 'dv'
names['tadv'] = 'dT'
names['qadv'] = 'dq'
names['qvadv'] = 'dq'
names['uadv'] = 'du'
names['vadv'] = 'dv'
names['u'] = 'u'
names['v'] = 'v'
names['u_nudg'] = 'u'
names['v_nudg'] = 'v'
names['ug'] = 'ug'
names['vg'] = 'vg'
names['w'] = 'W'
names['omega'] = 'Omega'
names['temp'] = 'T'
names['qv'] = 'q'
names['temp_nudg'] = 'T'
names['qv_nudg'] = 'q'

nlev = config.nlev

lev = MV2.array(range(0,nlev),typecode=MV2.float)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'lev'


gNetcdf = cdms2.open('forcing_interp_' + str(nlev) + '.nc','w')

for var in variables3D:
  print var	
  f = open(dirout + var + '_profiles_L' + str(nlev))

  time = MV2.zeros(nt,typecode=MV2.float)
  data = MV2.zeros((nt,nlev),typecode=MV2.float)

  for it in range(0,nt):

    time[it] = float(f.readline().split()[-1])
    for ilev in range(0,nlev):
      data[it,ilev] = float(f.readline().split()[0])	 

  f.close()

  if time.shape[0] == 1:
    uu = 'seconds since ' + str(yyyy1) + '-' + str(mm1) + '-' + str(dd1) + ' 0:0:0.0'	  
    dt0 = tend.torel(uu).value - tstart.torel(uu).value
    ntin = int(dt0/dt)

    data_interp = MV2.zeros((ntin+1,nlev),typecode=MV2.float)
    time_interp = MV2.zeros(ntin+1,typecode=MV2.float)

    for itin in range(0,ntin+1):
      ttin = time[0] + itin*dt
      time_interp[itin] = ttin
      data_interp[itin,:] = data[it,:]

    for ii in range(0,ntin+1):
#    print ii	  
#for ii in range(0,1):	
      g = open(dirout + names[var] + '_forcing_%(ii)5.5i.txt'%{"ii": ii},'w')
      for ilev in range(0,nlev):
        print >>g,  data_interp[ii,ilev]

      g.close()

  else:	  
    dt0 = time[1] - time[0]
    ntin = int(dt0/dt)

    if ntin <> dt0/dt:
      print 'problem', ntin, dt0, dt
      sys.exit()

    data_interp = MV2.zeros(((nt-1)*ntin+1,nlev),typecode=MV2.float)
    time_interp = MV2.zeros((nt-1)*ntin+1,typecode=MV2.float)

    for it in range(0,nt-1):
      for itin in range(0,ntin):
        ttin = time[it] + itin*dt
        time_interp[it*ntin+itin] = ttin
        data_interp[it*ntin+itin,:] = data[it,:] + (data[it+1,:]-data[it,:])/(time[it+1]-time[it])*(ttin-time[it])

    data_interp[(nt-1)*ntin,:] = data[nt-1,:]
    time_interp[(nt-1)*ntin] = time[nt-1]

    for ii in range(0,(nt-1)*ntin+1):
#    print ii	  
#for ii in range(0,1):	
      g = open(dirout + names[var] + '_forcing_%(ii)5.5i.txt'%{"ii": ii},'w')
      for ilev in range(0,nlev):
        print >>g,  data_interp[ii,ilev]

      g.close()

  time_interp = cdms2.createAxis(time_interp)
  time_interp.designateTime()
  time_interp.id = 'time'
  time_interp.units = time0.units #'seconds since 2011-10-01 0:0:0.0'
  time_interp.calendar = 'gregorian'

  data_interp.setAxis(0,time_interp)
  data_interp.setAxis(1,lev)
  
  if var == 'T':
    data_interp.id = 'Temp'	  
  else:
    data_interp.id = var

  gNetcdf.write(data_interp)





for var in variables2D:
  print var	
  f = open(dirout + var)

  time = MV2.zeros(nt,typecode=MV2.float)
  data = MV2.zeros(nt,typecode=MV2.float)

  for it in range(0,nt):

    time[it] = float(f.readline().split()[-1])
    data[it] = float(f.readline().split()[0])	 

  f.close()

  if time.shape[0] == 1:
    uu = 'seconds since ' + str(yyyy1) + '-' + str(mm1) + '-' + str(dd1) + ' 0:0:0.0'	  
    dt0 = tend.torel(uu).value - tstart.torel(uu).value
    ntin = int(dt0/dt)

    data_interp = MV2.zeros((ntin+1),typecode=MV2.float)
    time_interp = MV2.zeros(ntin+1,typecode=MV2.float)

    for itin in range(0,ntin+1):
      ttin = time[0] + itin*dt
      time_interp[itin] = ttin
      data_interp[itin] = data[it]


    for ii in range(0,ntin+1):
#    print ii	  
#for ii in range(0,1):	
      g = open(dirout + names[var] + '_forcing_%(ii)5.5i.txt'%{"ii": ii},'w')
      print >>g,  data_interp[ii]
      g.close()

  else:
    dt0 = time[1] - time[0]
    ntin = int(dt0/dt)

    if ntin <> dt0/dt:
      print 'problem', ntin, dt0, dt
      sys.exit()

    data_interp = MV2.zeros(((nt-1)*ntin+1),typecode=MV2.float)
    time_interp = MV2.zeros((nt-1)*ntin+1,typecode=MV2.float)

    for it in range(0,nt-1):
      for itin in range(0,ntin):
        ttin = time[it] + itin*dt
        time_interp[it*ntin+itin] = ttin
        data_interp[it*ntin+itin] = data[it] + (data[it+1]-data[it])/(time[it+1]-time[it])*(ttin-time[it])

    data_interp[(nt-1)*ntin] = data[nt-1]
    time_interp[(nt-1)*ntin] = time[nt-1]

    for ii in range(0,(nt-1)*ntin+1):
#    print ii	  
#for ii in range(0,1):	
      g = open(dirout + names[var] + '_forcing_%(ii)5.5i.txt'%{"ii": ii},'w')
      print >>g,  data_interp[ii]
      g.close()

  time_interp = cdms2.createAxis(time_interp)
  time_interp.designateTime()
  time_interp.id = 'time'
  time_interp.units = 'seconds since 2011-10-01 0:0:0.0'
  time_interp.calendar = 'gregorian'

  data_interp.setAxis(0,time_interp)

  data_interp.id = var

  gNetcdf.write(data_interp)

gNetcdf.close()

