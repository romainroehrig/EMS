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

for i in range(1,nlev-1):
  ph[i] = (pf[i]+pf[i+1])/2.    

ph[nlev-1] = 59800.
ph[nlev] = 59000.

tmp = [50000.,40000.,30000.,20000.,10000.,1000.,100.,0.]

nlev2 = nlev+1 + len(tmp)

ph2 = MV2.zeros(nlev2,typecode=MV2.float)
ph2[0:nlev+1] = ph[:]
ph2[nlev+1:] = tmp[:]


a = MV2.zeros(nlev2,typecode=MV2.float)
b = MV2.zeros(nlev2,typecode=MV2.float)

b = ph2/ps

g = open('L{0}.dta'.format(nlev2-1),'w')

for ilev in range(0,nlev2):
    print >>g, a[nlev2-ilev-1],b[nlev2-ilev-1]

g.close()

