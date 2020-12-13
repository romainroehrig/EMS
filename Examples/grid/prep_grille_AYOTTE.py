import MV2

f = open('grille_AYOTTE.txt')
lines = f.readlines()
f.close()

nlev = len(lines)

ps = 100000.

ph = MV2.zeros((nlev+1,),typecode=MV2.float)
pf = MV2.zeros(nlev,typecode=MV2.float)

for i,line in enumerate(lines):
  tmp = line.split()
  pf[i] = float(tmp[1])


ph[0] = ps

for i in range(0,nlev-1):
  ph[nlev-i-1] = (pf[i]+pf[i+1])/2.    

ph[nlev] = 60000.

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

