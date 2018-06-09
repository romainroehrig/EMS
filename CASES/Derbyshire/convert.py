import cdms2 
import MV2
import math
import sys


import thermo

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

rhcase = float(sys.argv[1])


lat = MV2.array([0.,],typecode=MV2.float32)
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.array([0,],typecode=MV2.float32)
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

t0= MV2.array([0,],typecode=MV2.float32)
t0= cdms2.createAxis(t0)
#t0.designateTime()
t0.id = 't0'
t0.calendar = 'gregorian'
t0.units = "seconds since 2000-01-01 00:00:00"

time= MV2.array([0,86400],typecode=MV2.float32)
time= cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.calendar = 'gregorian'
time.units = "seconds since 2000-01-01 00:00:00"

nt, = time.shape

levin0 = [z*10 for z in range(0,1501)]
nlevin0 = len(levin0)

lev1 = cdms2.createAxis(levin0)
lev1.designateLevel()
lev1.id = 'lev1'


ps = 100000.

u0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32)
z0 = 0.1
for i in range(0,nlevin0):
  u0[:,i,:,:] = 0.5*math.log(1+levin0[i]/z0)    

v0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32)

th0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32)
for i in range(0,nlevin0):
  if levin0[i] < 1000:
    th0[:,i,:,:] = 294 - (294.-293.)/(0.-1000.)*(0.-levin0[i])      
  else:
    th0[:,i,:,:] = 293 + 3*(levin0[i]/1000.-1.)    

rh0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32)
for i in range(0,nlevin0):
  if levin0[i] < 1000:
    rh0[:,i,:,:] = 100.
  elif levin0[i] >= 1000. and levin0[i] < 2000:
    rh0[:,i,:,:] = 80.
  else:
    rh0[:,i,:,:] = rhcase  



kappa = 2./7. #0.286 #2./7.
p0 = 100000.
g = 9.80665 #9.81
R = 287.0596736665907 #287 

integ = 0.

plevin0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32)
plevin0[:,0,:,:] = plevin0[:,0,:,:] + ps

for ilev in range(1,nlevin0):
  dz = levin0[ilev]- levin0[ilev-1]    
#  print 'dz =', dz
  integ = integ + (g/(R*th0[0,ilev-1,0,0])+g/(R*th0[0,ilev,0,0]))/2*dz
#  print 'integ =', integ
  tmp = ps**kappa-p0**kappa*kappa*integ
  plevin0[0,ilev,0,0] = math.exp(math.log(tmp)/kappa)


qv0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32)

for ilev in range(0,nlevin0):
  temp =  th0[:,ilev,:,:] * (plevin0[:,ilev,:,:]/p0)**kappa
  tmp1 = thermo.qv(rh0[:,ilev,:,:],temp, plevin0[:,ilev,:,:],ice=0)
  tmp2 = thermo.qv(rh0[:,ilev,:,:],temp, plevin0[:,ilev,:,:],ice=1)
  print ilev, rh0[:,ilev,:,:], temp, plevin0[:,ilev,:,:], tmp1, tmp2
  qv0[:,ilev,:,:] = min(tmp1,tmp2)



lev = cdms2.createAxis(plevin0[0,:,0,0])
lev.designateLevel()
lev.id = 'lev'

nlevin = nlevin0

plevin = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
u = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
v = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
temp = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
qv = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
th = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
rh = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)

for it in range(0,nt):
    plevin[it,:,:,:] = plevin0[0,:,:,:]
    u[it,:,:,:] = u0[0,:,:,:]
    v[it,:,:,:] = v0[0,:,:,:]
    th[it,:,:,:] = th0[0,:,:,:]
    rh[it,:,:,:] = rh0[0,:,:,:]
    for ilev in range(0,nlevin):
      temp[it,ilev,:,:] = th0[0,ilev,:,:]*(plevin[it,ilev,:,:]/p0)**kappa
    qv[it,:,:,:] = qv0[0,:,:,:]


u.setAxis(0,time)
u.setAxis(1,lev1)
u.setAxis(2,lat)
u.setAxis(3,lon)
u.id = 'u'

v.setAxis(0,time)
v.setAxis(1,lev1)
v.setAxis(2,lat)
v.setAxis(3,lon)
v.id = 'v'

th.setAxis(0,time)
th.setAxis(1,lev1)
th.setAxis(2,lat)
th.setAxis(3,lon)
th.id = 'theta'


rh.setAxis(0,time)
rh.setAxis(1,lev1)
rh.setAxis(2,lat)
rh.setAxis(3,lon)
rh.id = 'rh'


temp.setAxis(0,time)
temp.setAxis(1,lev1)
temp.setAxis(2,lat)
temp.setAxis(3,lon)
temp.id = 'temp'

qv.setAxis(0,time)
qv.setAxis(1,lev1)
qv.setAxis(2,lat)
qv.setAxis(3,lon)
qv.id = 'qv'

plevin.setAxis(0,time)
plevin.setAxis(1,lev)
plevin.setAxis(2,lat)
plevin.setAxis(3,lon)
plevin.id = 'pressure'


sst = MV2.zeros((nt,1,1),typecode=MV2.float32) + 294

sst.setAxis(0,time)
sst.setAxis(1,lat)
sst.setAxis(2,lon)
sst.id = 'ts'

ps = MV2.zeros((nt,1,1),typecode=MV2.float32) + ps

ps.setAxis(0,time)
ps.setAxis(1,lat)
ps.setAxis(2,lon)
ps.id = 'ps'

g = cdms2.open('Derbyshire_RH' + str(int(rhcase)) + '_driver_RR.nc','w')
g.write(plevin)
g.write(u)
g.write(v)
g.write(th)
g.write(rh)
g.write(temp)
g.write(qv)
g.write(ps)
g.write(sst)

g.comment = 'Forcing and initial conditions for Derbyshire case - RH = ' + str(int(rhcase))
g.reference = 'Derbyshire et al. (2004, QJRMS)'
g.author = 'R. Roehrig'


g.case = 'Derbyshire' + str(int(rhcase))
g.startDate = '20000101000000'
g.endDate = '20000102000000'

g.qadvh = 0
g.tadvh = 0
g.tadvv = 0
g.qadvv = 0
g.trad = 0 #1 ????

g.forc_omega = 0
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 3600
g.nudging_v = 3600
g.nudging_t = 3600
g.nudging_q = 3600

g.p_nudging_u = 89300.
g.p_nudging_v = 89300.
g.p_nudging_t = 89300.
g.p_nudging_q = 89300.


g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'
g.z0 = z0

g.close()

