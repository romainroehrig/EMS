import cdms2, MV2
import cdtime
import numpy as np
from scipy import interpolate

value = 0
cdms2.setNetcdfShuffleFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateLevelFlag(value) ## where value is a integer between 0 and 9 included

lat = MV2.zeros(1,typecode=MV2.float32) + 35.762
lat = cdms2.createAxis(lat)
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = MV2.zeros(1,typecode=MV2.float32) - 97.48
lon = cdms2.createAxis(lon)
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

data = {}


f = open('data_create_profiles/data_received_from_francoise/KIT_IDEAL/DATA/layer_idea_2days_advver_with_prescribed_rad.dat')

f.readline()
line = f.readline()
nt = int(float(line.split()[0]))

f.readline()
line = f.readline()
nlev = int(float(line.split()[0]))

f.readline()
tmp = []
for i in range(0,4):
    line = f.readline()
    tmp = tmp + line.split()

lev = MV2.array([float(x)*100. for x in tmp],typecode=MV2.float)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'lev'
lev.name = 'pressure'
lev.units = 'Pa'


for ifield in range(0,6):
    f.readline()
    tmp = []
    for i in range(0,4):
        line = f.readline()
        tmp = tmp + line.split()
    data[ifield] = MV2.array([float(x) for x in tmp],typecode=MV2.float)

time = MV2.zeros(nt,typecode=MV2.float)
units = 'seconds since 1997-06-27 00:00:0.0'

for it in range(0,nt):
    tt = cdtime.comptime(int(data[1][it]),int(data[2][it]),int(data[3][it]),int(data[4][it]),int(data[5][it]))
    print tt
    time[it] = tt.torel(units).value

time = cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.units = units
time.calendar = 'gregorian'

f.readline()
line = f.readline()

nfield = int(float(line.split()[0]))

var = {}
var[0] = 'temp'
var[1] = 'rv'
var[2] = 'u'
var[3] = 'v'
var[4] = 'omega'
var[5] = 'div'
var[6] = 'tadvh'
var[7] = 'tadvv0'
var[8] = 'rvadvh'
var[9] = 'rvadvv'
var[10] = 's'
var[11] = 'sadvh'
var[12] = 'sadvv'
var[13] = 'ds_dt'
var[14] = 'dT_dt'
var[15] = 'drv_dt'
var[16] = 'Q1'
var[17] = 'Q2'
var[18] = 'trad'

for ii in range(0,nfield):
    print ii, '/', nfield, var[ii]
    data[ii] = MV2.zeros((nt,nlev),typecode=MV2.float)
    f.readline()
    for ilev in range(0,nlev):
        tmp = []
        for j in range(0,4):
            line = f.readline()
            tmp = tmp + line.split()
        data[ii][:,ilev] = MV2.array([float(x) for x in tmp],typecode=MV2.float)

    if var[ii] in ['temp','s']:
        data[ii].units = 'K'

    if var[ii] in ['u','v']:
        data[ii].units = 'm s-1'

    if var[ii] in ['omega']:
        data[ii] = data[ii]/3600*100.
        data[ii].units = 'Pa s-1'

    if var[ii] in ['div']:
        data[ii].units = 's-1'

    if var[ii] in ['rv']:
        data[ii] = data[ii]/1000.
        data[ii].units = 'kg kg-1'

    if var[ii] in ['tadvh','tadvv0','sadvh','sadvv','ds_dt','dT_dt','Q1','Q2']:
        data[ii] = data[ii]/3600.
        data[ii].units = 'K s-1'

    if var[ii] in ['trad']:
        data[ii] = data[ii]/86400.
        data[ii].units = 'K s-1'

    if var[ii] in ['rvadvh','rvadvv','drv_dt']:
        data[ii] = data[ii]/3600./1000.
        data[ii].units = 'kg kg-1 s-1'

    data[ii].setAxis(0,time)
    data[ii].setAxis(1,lev)
    data[ii].id = var[ii]

#ii = ii - 1

ii = ii+1
var[ii] = 'qv'
print ii, var[ii]
data[ii] = data[1]/(1+data[1])
data[ii].id = var[ii]
data[ii].units = 'kg kg-1'

ii = ii+1
var[ii] = 'qadvh'
print ii, var[ii]
data[ii] = 1./((1+data[1])*(1+data[1]))*data[8]
data[ii].id = var[ii]
data[ii].units = 'kg kg-1 s-1'

ii = ii+1
var[ii] = 'qadvv'
print ii, var[ii]
data[ii] = 1./((1+data[1])*(1+data[1]))*data[9]
data[ii].id = var[ii]
data[ii].units = 'kg kg-1 s-1'

ii = ii+1
var[ii] = 'thadvh'
print ii, var[ii]
data[ii] = data[11]*1.
data[ii].id = var[ii]
data[ii].units = 'K s-1'

ii = ii+1
var[ii] = 'thadvv'
print ii, var[ii]
data[ii] = data[12]*1.
data[ii].id = var[ii]
data[ii].units = 'K s-1'

ii = ii+1
var[ii] = 'theta'
print ii, var[ii]
data[ii] = data[0]*0.
for ilev in range(0,nlev):
    data[ii][:,ilev] = data[0][:,ilev]*(100000./lev[ilev])**(2./7.)
data[ii].id = var[ii]
data[ii].units = 'K'

ii = ii+1
var[ii] = 'tadvv'
print ii, var[ii]
data[ii] = data[0]*0.
for ilev in range(0,nlev):
    data[ii][:,ilev] = data[12][:,ilev]*(lev[ilev]/100000.)**(2./7.)
data[ii].id = var[ii]
data[ii].units = 'K'

ii = ii+1
var[ii] = 'pressure'
print ii, var[ii]
data[ii] = data[0]*0.
for it in range(0,nt):
    data[ii][it,:] = lev[:]
data[ii].id = var[ii]
data[ii].units = 'Pa'

# From data_create_profiles/data_received_from_francoise/KIT_IDEAL/DATA/surface_idea_2days.dat - Area Mean Ps(mb)
data['ps'] = [.9728589E+03,  .9732012E+03,   .9728380E+03,   .9719967E+03,   .9715103E+03,   .9715473E+03,   .9719656E+03,   .9725129E+03,   .9728589E+03,   .9732012E+03,   .9728380E+03,   .9719967E+03,   .9715103E+03,   .9715473E+03,   .9719656E+03,   .9725129E+03,   .9728589E+03]
data['ps'] = MV2.array(data['ps'],typecode=MV2.float)*100.

datanew = {}
f = cdms2.open('ARMCVP_sfc_FG.nc')
for vv in ['sfc_lat_flx','sfc_sens_flx','ts']:
    datanew[vv] = f(vv)
f.close()


timenew = datanew['ts'].getTime()
ntnew, = timenew.shape

ff = interpolate.interp1d(np.array(time[:]),np.array(data['ps'][:]))
datanew['ps'] = MV2.array(ff(np.array(timenew[:])),typecode=MV2.float)
datanew['ps'].units = 'Pa'

for vv in ['sfc_lat_flx','sfc_sens_flx','ts','ps']:
  datanew[vv] = datanew[vv].reshape((ntnew,1,1))
  datanew[vv].setAxis(0,timenew)
  datanew[vv].setAxis(1,lat)
  datanew[vv].setAxis(2,lon)
  datanew[vv].id = vv


for i in range(0,ii+1):
  print i, var[i]
  datanew[i] = MV2.zeros((ntnew,nlev),typecode=MV2.float32)
  for ilev in range(nlev):
      ff = interpolate.interp1d(np.array(time[:]),np.array(data[i][:,ilev]))
      datanew[i][:,ilev] = ff(np.array(timenew[:]))
  datanew[i] = MV2.reshape(datanew[i],(ntnew,nlev,1,1))
  datanew[i].setAxis(0,timenew)
  datanew[i].setAxis(1,lev)
  datanew[i].setAxis(2,lat)
  datanew[i].setAxis(3,lon)
  datanew[i].id = data[i].id
  datanew[i].units = data[i].units

g = cdms2.open('ARMCVP_FG_norad.nc','w')

for i in range(0,ii+1):
    g.write(datanew[i])

for vv in ['sfc_lat_flx','sfc_sens_flx','ts','ps']:
    g.write(datanew[vv])

g.comment = 'Initial conditions and forcing for ARM CVP case - No Radiation'
g.reference = 'Guichard et al. (2004, QJRMS)'
g.author = 'R. Roehrig'


g.case = 'ARMCVP'
g.startDate = '19970627113000'
g.endDate = '19970629113000'

g.qadvh = 0
g.tadvh = 0
g.tadvv = 1
g.qadvv = 1
g.trad = 1

g.forc_omega = 0
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 3600*2.
g.nudging_v = 3600*2.
g.nudging_t = 0
g.nudging_q = 0

g.p_nudging_u = 110000.
g.p_nudging_v = 110000.

g.zorog = 0.
g.z0 = 0.1
g.surfaceType = 'land'
g.surfaceForcing = 'surfaceFlux'
g.surfaceForcingWind = 'z0'

g.close()

