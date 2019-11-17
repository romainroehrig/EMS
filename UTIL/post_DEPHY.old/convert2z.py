import cdms2
import MV2
import sys
import convert2z
import config

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

varall = config.saveall

var2save = config.var2save

levelz = config.levoutz
levelz = MV2.array(levelz,typecode=MV2.float32)
nlevz = levelz.shape[0]

levelz = cdms2.createAxis(levelz)
levelz.id = 'levz'
levelz.designateLevel()
levelz.units = 'm'
levelz.long_name = 'Altitude'


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
phi = f('zh')
phif = f('zf')

time = phi.getTime()

nlev = phif.shape[1]
nlev1 = phi.shape[1]

for ilev1 in range(0,nlev1):
    phi[:,ilev1] = phi[:,ilev1] - phi[:,nlev1-1]

for ilev in range(0,nlev):
    phif[:,ilev] = phif[:,ilev] - phif[:,nlev-1]

g = cdms2.open('netcdf/Out_zlevel.nc','w')

for var in var2save:
  print var	
  data0 = f(var)
  if (len(data0.shape) == 2 and data0.shape[1] >= nlev) and var not in ['clcalipso','lidarBetaMol532','clcalipsoice','clcalipsoliq','clcalipsoun','clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun','fracout','atb532','cfadLidarsr532','dbze94','cfadDbze94','clisccp','clmodis','clMISR','parasolRefl','clcalipso2','boxtauisccp','boxptopisccp','VETAF']:
    if data0.shape[1] == nlev:
      data,missing = convert2z.convert2z(phif,levelz,data0)
    elif data0.shape[1] == nlev1:
      data,missing = convert2z.convert2z(phi,levelz,data0)
    else:
      print 'vertical dimension unexpected for ' + var + ': ' + str(f[var].shape[1])
      sys.exit()
  
    data = MV2.array(data,typecode=MV2.float)
    data = MV2.where(missing == 1, missing_value,data)
    data = MV2.masked_values(data,missing_value)
    data.missing_value = missing_value
    data.setAxis(0,time)
    data.setAxis(1,levelz)
    data.id = var
    for att in data0.listattributes():
      data.setattribute(att,data0.getattribute(att))

    g.write(data)
  else:
    g.write(data0)	  


f.close()
g.close()
