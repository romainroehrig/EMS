import cdms2
import MV2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

nlev = 87

lev = MV2.array(range(0,nlev),typecode=MV2.float32)
lev = cdms2.createAxis(lev)
lev.id = 'lev'
lev.designateLevel()
lev.long_name = 'Altitude'
lev.units = 'km'


variables = ['zz','temp','pp','rho']

units = {}
units['zz'  ] = 'km'
units['temp'] = 'K'
units['pp'  ] = 'Pa'
units['rho' ] = 'kg/m3'

names = {}
names['zz'  ] = 'Altitude'
names['temp'] = 'Air Temperature'
names['pp'  ] = 'Pressure'
names['rho' ] = 'Air density'

data = {}
for var in variables:
  data[var] = MV2.zeros(nlev,typecode=MV2.float32)

f = open('Standard_Atmosphere_1979.txt')
f.readline()
f.readline()

for ilev in range(0,nlev):
  line = f.readline().split()
  data['zz'  ][ilev] = float(line[0])
  data['temp'][ilev] = float(line[1])
  data['pp'  ][ilev] = float(line[2])
  data['rho' ][ilev] = float(line[3])

f.close()

for var in variables:
  data[var].setAxis(0,lev)
  data[var].id = var
  data[var].units = units[var]
  data[var].long_name = names[var]

g = cdms2.open('Standard_Atmosphere_zlevel.nc','w')
for var in variables:
  g.write(data[var])

g.source = 'http://www.digitaldutch.com/atmoscalc/tableatmosphere.htm'

g.close()

lev = cdms2.createAxis(MV2.array(data['pp'],typecode=MV2.float32))
lev.id = 'lev'
lev.designateLevel()
lev.long_name = 'Pressure'
lev.units = 'Pa'


for var in variables:
  data[var].setAxis(0,lev)
  data[var].id = var
  data[var].units = units[var]
  data[var].long_name = names[var]

g = cdms2.open('Standard_Atmosphere_plevel.nc','w')
for var in variables:
  g.write(data[var])

g.source = 'http://www.digitaldutch.com/atmoscalc/tableatmosphere.htm'

g.close()
