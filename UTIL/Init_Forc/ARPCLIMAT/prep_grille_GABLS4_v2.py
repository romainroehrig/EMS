import cdms2
import MV2

f = cdms2.open('/cnrm/moana/user/couvreux/GABLS4_INTERCOMPARISON_LES/POUR_OLIVIER/MESONH/24h/gabls4_instprofile_les_MESONH_stage3_zo3.nc')
pres = f('P')
f.close()

nt,nlev = pres.shape

ps = 65100.

ph = MV2.zeros((nlev+1,),typecode=MV2.float)
pf = MV2.zeros(nlev,typecode=MV2.float)

pf[:] = pres[0,:]

ph[0] = ps

for i in range(0,nlev-1):
  ph[i+1] = (pf[i]+pf[i+1])/2.    

ph[nlev] = 59800

a137 = MV2.zeros(137+1,typecode=MV2.float32)
b137 = MV2.zeros(137+1,typecode=MV2.float32)

f = open('L137.dta')
lines = f.readlines()
f.close()

for i,line in enumerate(lines):
  tmp = line.split()
  a137[i] = float(tmp[0])
  b137[i] = float(tmp[1])

ph137 = a137 + b137*ps

tmp = MV2.where(ph137 > ph[nlev], 1.e20, ph137)
tmp = MV2.masked_values(tmp,1.e20)
nlev0 = MV2.count(tmp)

tmp = ph137[0:nlev0]
tmp = tmp[::-1]

nlev2 = nlev+1 + len(tmp)

ph2 = MV2.zeros(nlev2,typecode=MV2.float)
ph2[0:nlev+1] = ph[:]
ph2[nlev+1:] = tmp[:]


a = MV2.zeros(nlev2,typecode=MV2.float)
b = MV2.zeros(nlev2,typecode=MV2.float)


tmp = a137[0:nlev0]
a[nlev+1:] = tmp[::-1]
tmp = b137[0:nlev0]
b[nlev+1:] = tmp[::-1]

b[0:nlev+1] = ph/ps

g = open('L{0}.dta'.format(nlev2-1),'w')

for ilev in range(0,nlev2):
    print >>g, a[nlev2-ilev-1],b[nlev2-ilev-1]

g.close()

