import MV2
import cdms2

import numpy as np
from scipy import interpolate

f = open('/Users/romainroehrig/Tools/EMS/UTIL/Init_Forc/ARPCLIMAT/L91.dta')
lines = f.readlines()
f.close()

a0 = []
b0 = []
for line in lines:
    tmp = line.split()
    a0.append(float(tmp[0]))
    b0.append(float(tmp[1]))

a0 = MV2.array(a0,typecode=MV2.float)
b0 = MV2.array(b0,typecode=MV2.float)

ps = 101250.


p0 = a0 + b0*ps
p0 = p0[::-1]

f = cdms2.open('../CINDY-DYNAMO/NSA3a/Standard_Atmosphere_zlevel.nc')
pstd = f('pp')
zstd = f('zz')*1000.
tstd = f('temp')
f.close()

pstd = pstd/pstd[0]*ps

lev = [0] + range(5,1000,10) + range(1000,3000,100)
nlev = len(lev)

ff = interpolate.interp1d(np.array(zstd),np.array(pstd))
pstd2 = ff(np.array(lev)) 


pnew = list(pstd2)
for p in p0:
    if p < pstd2[-1]:
        pnew.append(p)

pnew = MV2.array(pnew,typecode=MV2.float)
pnew = pnew
nlev2, = pnew.shape

a = MV2.zeros(nlev2,typecode=MV2.float)
b = MV2.zeros(nlev2,typecode=MV2.float)

b = pnew/ps

g = open('L{0}.dta'.format(nlev2-1),'w')

for ilev in range(0,nlev2):
    print >>g, a[nlev2-ilev-1],b[nlev2-ilev-1]

g.close()
