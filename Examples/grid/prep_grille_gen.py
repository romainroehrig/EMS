import cdms2, MV2

f = cdms2.open('/cnrm/amacs/USERS/roehrig/simulations/V640/AGCMV640v6_vertRes/AGCMV640v6_vertRes_arpsfx_instant_pah_1979-1979.nc')

p0 = f('pah')[0,:,:]
np,nx = p0.shape

p0 = p0[:,nx/2]
ps = p0[0]

f.close()

pmax = 60000.
ratio = 5

P = [ps]

for i in range(1,np):
  p = p0[i]
  if p > pmax:
      dp = (p0[i]-p0[i-1])/ratio
      for k in range(1,ratio):
        P.append(p0[i-1]+k*dp)

  P.append(p0[i])


P = MV2.array(P,typecode=MV2.float)

nlev, = P.shape

a = MV2.zeros(nlev,typecode=MV2.float)
b = MV2.zeros(nlev,typecode=MV2.float)

b = P/ps

g = open('test/L{0}.dta'.format(nlev-1),'w')

for ilev in range(0,nlev):
    print >>g, a[nlev-ilev-1],b[nlev-ilev-1]

g.close()
