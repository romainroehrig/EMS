import cdms2
import MV2

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value)
cdms2.setNetcdfDeflateLevelFlag(value)

data = {}
f = cdms2.open('BOMEX_driver_FC.nc')

variables = f.listvariables()

for var in variables:
  data[var] = f(var)

f.close()

lev = data['height_f'](squeeze=1)
lev = cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'lev'
lev.units = 'm'

nlev, = lev.shape

g = cdms2.open('BOMEX_driver_FC_RR.nc','w')

for var in variables:
    try:
      if data[var].shape[0] == nlev:
        data[var].setAxis(0,lev)
    except:
      pass
    try:
      if data[var].shape[1] == nlev:
        data[var].setAxis(1,lev)
    except:
      pass
    g.write(data[var])

g.close()
