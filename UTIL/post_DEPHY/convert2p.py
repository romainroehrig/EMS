import cdms2
import MV2
import sys
import convert2p
import config

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

varall = config.saveall

var2save = config.var2save

levout = config.levout
levout = MV2.array(levout,typecode=MV2.float32)
nlevout = levout.shape[0]

levout = cdms2.createAxis(levout)
levout.id = 'levp'
levout.designateLevel()
levout.units = 'hPa'
levout.long_name = 'pressure_level'

missing_value = 1.e20


if varall:
  f =cdms2.open('netcdf/Out_klevel.nc')
  var2save = f.listvariables()
  f.close
else:
  f =cdms2.open('netcdf/Out_klevel.nc')
  var2save0 = f.listvariables()
  f.close 
  var2save = set(var2save).intersection(set(var2save0))

f = cdms2.open('netcdf/Out_klevel.nc')
pres = f('ph')/100.
presf = f('pf')/100.

time = pres.getTime()

nlev = presf.shape[1]
nlev1 = pres.shape[1]

g = cdms2.open('netcdf/Out_plevel.nc','w')

for var in var2save:
  if config.verbose >= 1:	
    print var	
  data0 = f(var)
  if (len(data0.shape) == 2 and data0.shape[1] >= nlev) and var not in ['clcalipso','lidarBetaMol532','clcalipsoice','clcalipsoliq','clcalipsoun','clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun','fracout','atb532','cfadLidarsr532','dbze94','cfadDbze94','clisccp','clmodis','clMISR','parasolRefl','clcalipso2','boxtauisccp','boxptopisccp','VETAF']:
    if data0.shape[1] == nlev:
      data,missing = convert2p.convert2p(presf,levout,data0)
    elif data0.shape[1] == nlev1:
      data,missing = convert2p.convert2p(pres,levout,data0)
    else:
      print 'vertical dimension unexpected for ' + var + ': ' + str(f[var].shape[1])
      sys.exit()

    data = MV2.array(data,typecode=MV2.float)
    data = MV2.where(missing == 1, missing_value,data)
    data = MV2.masked_values(data,missing_value)
    data.missing_value = missing_value
    data.setAxis(0,time)
    data.setAxis(1,levout)
    data.id = var
    for att in data0.listattributes():
      data.setattribute(att,data0.getattribute(att))

    g.write(data)
  else:
    g.write(data0)	  


f.close()
g.close()
