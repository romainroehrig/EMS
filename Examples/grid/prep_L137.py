import MV2

f = open('nam1D_TOGA1_l137')
lines = f.readlines()
f.close()

nlev = 137
a = MV2.zeros(nlev+1,typecode=MV2.float32)
b = MV2.zeros(nlev+1,typecode=MV2.float32)

for i,line in enumerate(lines):
  if line.split()[0] == 'ETA':
      break

for k in range(0,nlev+1):
  a[k] = float(lines[i+2+k].split()[0])    
  b[k] = float(lines[i+2+nlev+2+k].split()[0])

g = open('L{0}.dta'.format(nlev),'w')

for ilev in range(0,nlev+1):
    print >>g, a[ilev],b[ilev]

g.close()
