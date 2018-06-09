import cdms2 
import MV2
import math

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)


lat = MV2.array([35.762,],typecode=MV2.float32)
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.array([97.48,],typecode=MV2.float32)
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

t0= MV2.array([41400,],typecode=MV2.float32)
t0= cdms2.createAxis(t0)
#t0.designateTime()
t0.id = 't0'
t0.calendar = 'gregorian'
t0.units = "seconds since 1997-6-21 00:00:00"

time= MV2.array([41400,52200,63000,73800,84600,86400+9000],typecode=MV2.float32)
time= cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.calendar = 'gregorian'
time.units = "seconds since 1997-6-21 00:00:00"

nt, = time.shape

levin0 = [0,50,350,650,700,1300,2500,5500]
nlevin0 = len(levin0)

lev1 = cdms2.createAxis(MV2.array(range(0,nlevin0),typecode=MV2.float32))
lev1.designateLevel()
lev1.id = 'lev1'


ps = 97000.

u0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32) + 10.
v0 = MV2.zeros((1,nlevin0,1,1),typecode=MV2.float32) + 10.

th = MV2.array([299,301.5,302.5,303.53,303.7,307.13,314.0,342.2],typecode=MV2.float32)
th = MV2.reshape(th,(1,nlevin0,1,1))
rv = MV2.array([15.19,15.17,14.98,14.8,14.7,13.5,3,3],typcode=MV2.float32)*1.e-3
rv = MV2.reshape(rv,(1,nlevin0,1,1))


qv = rv/(1.+rv)



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
  integ = integ + (g/(R*th[0,ilev-1,0,0])+g/(R*th[0,ilev,0,0]))/2*dz
#  print 'integ =', integ
  tmp = ps**kappa-p0**kappa*kappa*integ
  plevin0[0,ilev,0,0] = math.exp(math.log(tmp)/kappa)


levin = [0,1000,3000,5000]
nlevin = len(levin)


plevin = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)

plevin[:,0,:,:] = plevin[:,0,:,:] + ps
plevin[:,1,:,:] = plevin0[:,4,:,:] + (levin[1]-levin0[4])*(plevin0[:,5,:,:]-plevin0[:,4,:,:])/(levin0[5]-levin0[4])
plevin[:,2,:,:] = plevin0[:,7,:,:] + (levin[2]-levin0[7])*(plevin0[:,8,:,:]-plevin0[:,7,:,:])/(levin0[8]-levin0[7])
plevin[:,3,:,:] = plevin0[:,7,:,:] + (levin[3]-levin0[7])*(plevin0[:,8,:,:]-plevin0[:,7,:,:])/(levin0[8]-levin0[7])


lev = cdms2.createAxis(MV2.array(range(0,4),typecode=MV2.float32))
lev.designateLevel()
lev.id = 'lev'

u = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
v = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
dth = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)
drv = MV2.zeros((nt,nlevin,1,1),typecode=MV2.float32)

f = open('forc_z_u_v_th_rv_w_dth_drv')
for i in range(0,2):
  f.readline()
lines = f.readlines()

for it in range(0,nt):
  for ilev in range(0,nlevin):    
    tmp = lines[6+ilev+it*10].split()
#    print tmp
    u[it,ilev,0,0] = float(tmp[1])
    v[it,ilev,0,0] = float(tmp[2])
    dth[it,ilev,0,0] = float(tmp[6])
    drv[it,ilev,0,0] = float(tmp[7])

f.close()

u.setAxis(0,time)
u.setAxis(1,lev)
u.setAxis(2,lat)
u.setAxis(3,lon)
u.id = 'u'

v.setAxis(0,time)
v.setAxis(1,lev)
v.setAxis(2,lat)
v.setAxis(3,lon)
v.id = 'v'

plevin.setAxis(0,time)
plevin.setAxis(1,lev)
plevin.setAxis(2,lat)
plevin.setAxis(3,lon)
plevin.id = 'pressure'


Lv = 2.5008e+6

f = open('surf')

lines = f.readlines()

time1= MV2.zeros((31,),typecode=MV2.float32) + 41400.
time1= cdms2.createAxis(time1)
#time.designateTime()
time1.id = 'time'
time1.calendar = 'gregorian'
time1.units = "seconds since 1997-6-21 00:00:00"

nt1, = time1.shape

sh = MV2.zeros((nt1,1,1),typecode=MV2.float32)
lh = MV2.zeros((nt1,1,1),typecode=MV2.float32)

for it in range(0,nt1):
  tmp = lines[it].split('=')
  time1[it] = time1[it]+float(tmp[1].split(',')[0])
  tmp = lines[nt1+it].split('=')
  sh[it,0,0] = float(tmp[1].split(',')[0])
  tmp = lines[2*nt1+it].split('=')
  lh[it,0,0] = float(tmp[1].split(',')[0])*Lv

f.close()

sh.setAxis(0,time1)
sh.setAxis(1,lat)
sh.setAxis(2,lon)
sh.id = 'sfc_sens_flx'
ln.setAxis(0,time1)
lh.setAxis(1,lat)
lh.setAxis(2,lon)
lh.id = 'sfc_lat_flx'


stop
data = {}
for var in f.listvariables():
  data[var] = f(var)

time = data['tadvh'].getTime()

lev = data['tadvh'].getAxis(1)
lev.designateLevel()




data['w'].id = 'omega' 
data['w'].title = 'Vertical Pressure Velocity'

var = 'qadvh'
tmp = data[var]/1000.
for att in data[var].listattributes():
  tmp.setattribute(att, data[var].getattribute(att))

tmp.id = var
tmp.setAxis(0,time)
tmp.setAxis(1,lev)
tmp.setAxis(2,lat)
tmp.setAxis(3,lon)
data[var] = tmp


for var in ['temp','qv','u','v','height','pressure']:
  nlev,nlat,nlon = data[var].shape	
  tmp = data[var].reshape((1,nlev,nlat,nlon))
  for att in data[var].listattributes():
    tmp.setattribute(att, data[var].getattribute(att))

  tmp.id = var
  tmp.setAxis(0,t0)
  tmp.setAxis(1,lev)
  tmp.setAxis(2,lat)
  tmp.setAxis(3,lon)
  data[var] = tmp

g = cdms2.open('rico_driver_RR.nc','w')
for var in f.listvariables():
  if len(data[var].shape) == 4:	
    data[var].setAxis(1,lev)
    data[var].setAxis(2,lat)
    data[var].setAxis(3,lon)
  elif len(data[var].shape) == 3:
    if data[var].shape == (nlev,nlat,nlon):
      data[var].setAxis(0,lev)
      data[var].setAxis(1,lat)
      data[var].setAxis(2,lon)
    else:
      data[var].setAxis(1,lat)
      data[var].setAxis(2,lon)
     		  
  g.write(data[var])

g.comment = 'Forcing and initial conditions for ARM-Cumulus case'
g.reference = '??'
g.author = 'R. Roehrig'


g.case = 'ARMCU'
g.startDate = '19970621113000'
g.endDate = '19970622023000'

g.qadvh = 1
g.tadvh = 1
g.tadvv = 0
g.qadvv = 0
g.trad = 0 #1 ????

g.forc_omega = 0
g.forc_w = 1

g.forc_geo = 1

g.nudging_u = 0
g.nudging_v = 0
g.nudging_t = 0
g.nudging_q = 0

g.zorog = 0.
g.surfaceType = 'ocean'
g.surfaceForcing = 'flux'

g.close()

f.close()

