import cdms2
import MV2
import vcs

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

ff = 'cindy_nsa3a.nc'

f =cdms2.open(ff)
hus = f('qv',squeeze=1)
pres = f('pp',squeeze=1)
hq = f('hq',squeeze=1)
vq = f('vq',squeeze=1)
f.close()

nt,nlev = hus.shape
time = hus.getTime()

prw = MV2.zeros(nt,typecode=MV2.float)
hqt = MV2.zeros(nt,typecode=MV2.float)
vqt = MV2.zeros(nt,typecode=MV2.float)

for ilev in range(1,nlev-1):
  prw[:] = prw[:] + hus[:,ilev]*(pres[:,ilev-1]-pres[:,ilev+1])*100./2./9.81
  hqt[:] = hqt[:] + hq[:,ilev]*(pres[:,ilev-1]-pres[:,ilev+1])*100./2./9.81
  vqt[:] = vqt[:] + vq[:,ilev]*(pres[:,ilev-1]-pres[:,ilev+1])*100./2./9.81

prw.setAxis(0,time)
hqt.setAxis(0,time)
vqt.setAxis(0,time)

x = vcs.init()
x.plot(prw)

x1 = vcs.init()
x1.plot(hqt*86400)

x2 = vcs.init()
x2.plot(vqt*86400)

x3 = vcs.init()
x3.plot((vqt+hqt)*86400)


prw.id = 'prw'

g = cdms2.open('tmp.nc','w')
g.write(prw)
g.close()
