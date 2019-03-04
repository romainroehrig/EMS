import sys, os
sys.path = ['./'] + sys.path
import cdms2
import MV2
import cdtime

import variables as vv
import config

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

saveall = config.saveall

var2save = config.var2save
if not('pf' in var2save):
  var2save.append('pf')
if not('ph' in var2save):
  var2save.append('ph')
if not('zf' in var2save):
  var2save.append('zf')
if not('zh' in var2save):
  var2save.append('zh')
varnames = vv.varnames
names = vv.names
varunits = vv.units
coefs = vv.coefs

f = cdms2.open('global.nc')

if saveall:
  var2save0 = f.listvariables()
  var2save = []
  for var in var2save0:
    if var in vv.varnames.values():
      for vvar in vv.varnames.keys():
        if vv.varnames[vvar] == var:	      
          var2save.append(vvar)
    else:
      var2save.append(var)
      varnames[var] = var
      names[var] = var
      coefs[var] = 1.
      varunits[var] = '-'
else:
  var2save0 = f.listvariables()
  var2save1 = []
  for var in var2save0:
    if var in vv.varnames.values():
      for vvar in vv.varnames.keys():
        if vv.varnames[vvar] == var:	      
          var2save1.append(vvar)
    else:
      var2save1.append(var)
      varnames[var] = var
      names[var] = var
      coefs[var] = 1.
      varunits[var] = '-'
  var2save = set(var2save).intersection(set(var2save1))

#print var2save

nindat = f('NINDAT')
nsssss = f('NSSSSS')
rstati = f('RSTATI')
step = f('TSPHY')[0]

nt = nindat.shape[0]

time = MV2.array(range(0,nt),typecode=MV2.float32)
time_bnds = MV2.zeros((nt,2),typecode=MV2.float32)
#units = config.tunits

year = int(str(nindat[0])[0:4])
month = int(str(nindat[0])[4:6])
day = int(str(nindat[0])[6:8])
units = 'seconds since %(year)4.4i-%(month)2.2i-%(day)2.2i 0:0:0.0'%{"year": year, "month": month, "day": day}

for it in range(0,nt):
  year = int(str(nindat[it])[0:4])
  month = int(str(nindat[it])[4:6])
  day = int(str(nindat[it])[6:8])
  hour = int((nsssss[it]+rstati[it])/3600)
  minutes = int((nsssss[it]+rstati[it]-hour*3600)/60)
  seconds = nsssss[it]+rstati[it]-hour*3600-minutes*60

  lhour = False
  if hour >= 24:
    ndays = int(hour/24) 
    hour = hour - ndays*24
    lhour = True	

  tt = cdtime.comptime(year,month,day,hour,minutes,seconds)
  if lhour:
    tt = tt.add(ndays*24,cdtime.Hour)

  time[it] = float(tt.torel(units).value)

  tt1 = tt.add(-step/2.,cdtime.Second)
  tt2 = tt.add( step/2.,cdtime.Second)
  if config.verbose >= 2:
    print tt.tocomp(), tt1.tocomp(), tt2.tocomp()
  time_bnds[it,0] = float(tt1.torel(units).value)
  time_bnds[it,1] = float(tt2.torel(units).value)


time = cdms2.createAxis(time,bounds=time_bnds)
time.designateTime()
time.id = 'time'
time.units = units
time.calendar = 'gregorian'

paprsf = f('PAPRSF')[0,:]
nlev = paprsf.shape[0]

lalt40 = False
for var in ['clcalipso','clcalipso2','clcalipsoice','clcalipsoliq','clcalipsoun']:
  try:
    tmp = f(var)[0,:]
    lalt40 = True
  except:
    pass


ltemp = False
for var in ['clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun']:
  try:
    tmp = f(var)[0,:]
    ltemp = True
  except:
    pass

lsza5 = False
for var in ['parasolRefl']:
  try:
    tmp = f(var)[0,:]
    lsza5 = True
  except:
    pass


lcolumn = False
for var in ['boxtauisccp','boxptopisccp','dbze94','atb532','fracout']:
  try:
    tmp = f(var)[0,:]
    ncol = tmp.shape[0]
    lcolumn=True
  except:
    pass	  

ldbze = False
for var in ['cfadDbze94']:
  try:
    tmp = f(var)[0,:,:]
    ldbze=True
    lalt40 = True
  except:
    pass

lsratio = False
for var in ['cfadLidarsr532']:
  try:
    tmp = f(var)[0,:,:]
    lsratio=True
    lalt40 = True
  except:
    pass

ltau = False
lplev7 = False
for var in ['clisccp','clmodis']:
  try:
    tmp = f(var)[0,:,:]
    ltau=True
    lplev7=True
  except:
    pass

lmisr = False
for var in ['clMISR']:
  try:
    tmp = f(var)[0,:,:]
    lmisr=True
  except:
    pass

levels = MV2.array(range(0,nlev),typecode=MV2.float32)
levels = cdms2.createAxis(levels)
levels.designateLevel()
levels.id = 'levf'
levels.name = 'Full Pressure Level Number'
levels.units = '-'

levels0 = MV2.array(range(0,nlev+1),typecode=MV2.float32)
levels0 = cdms2.createAxis(levels0)
levels0.designateLevel()
levels0.id = 'levh'
levels0.name = 'Half Pressure Level Number'
levels0.units = '-'

if lalt40:
  alt40 = [240., 720., 1200., 1680., 2160., 2640., 3120., 3600., 4080., 4560., 5040., 5520., 6000., 6480., 6960., 7440., 7920., 8400., 8880., 9360., 9840., 10320., 10800., 11280., 11760., 12240., 12720., 13200., 13680., 14160., 14640., 15120., 15600., 16080., 16560., 17040., 17520., 18000., 18480., 18960.]
  alt40 = MV2.array(alt40,typecode=MV2.float32)
  alt40 = cdms2.createAxis(alt40)
  alt40.designateLevel()
  alt40.id = 'alt40'
  alt40.name = 'altitude'
  alt40.units = 'm'

if ltemp:
  temp = [-91.5,-88.5,-85.5,-82.5,-79.5,-76.5,-73.5,-70.5,-67.5,-64.5,-61.5,-58.5,-55.5,-52.5,-49.5,-46.5,-43.5,-40.5,-37.5,-34.5,-31.5,-28.5,-25.5,-22.5,-19.5,-16.5,-13.5,-10.5, -7.5, -4.5,-1.5,  1.5,  4.5,  7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5]
  temp = MV2.array(temp,typecode=MV2.float32)
  temp = cdms2.createAxis(temp)
  temp.designateLevel()
  temp.id = 'temp'
  temp.name = 'Temperature'
  temp.units = 'C'

if lsza5:
  sza5 = [0., 20., 40., 60., 80.]
  sza5 = MV2.array(sza5,typecode=MV2.float32)
  sza5 = cdms2.createAxis(sza5)
  sza5.id = 'sza5'
  sza5.name = 'Solar Zenith Angle'
  sza5.units = 'degree'

if lcolumn:
  column = MV2.array(range(0,ncol),typecode=MV2.float32)
  column = cdms2.createAxis(column)
  column.id = 'column'
  column.name = 'Column'
  column.units = '-'

if ldbze:
  dbze = [-47.5, -42.5, -37.5, -32.5, -27.5, -22.5, -17.5, -12.5, -7.5, -2.5, 2.5, 7.5, 12.5, 17.5, 22.5]
  dbze = MV2.array(dbze,typecode=MV2.float32)
  dbze = cdms2.createAxis(dbze)
  dbze.id = 'dbze'
  dbze.name = 'CloudSat simulator equivalent radar reflectivity factor'
  dbze.units = 'dBZ'

if lsratio:
  sratio = [0.005, 0.605, 2.1, 4., 6., 8.5, 12.5, 17.5, 22.5, 27.5, 35., 45., 55., 70., 50040.]
  sratio = MV2.array(sratio,typecode=MV2.float32)
  sratio = cdms2.createAxis(sratio)
  sratio.id = 'scatratio'
  sratio.name = 'lidar backscattering ratio'
  sratio.units = '1'

if ltau:
  tau = [0.15, 0.8, 2.45, 6.5, 16.2, 41.5, 100.]
  tau = MV2.array(tau,typecode=MV2.float32)
  tau = cdms2.createAxis(tau)
  tau.id = 'tau'
  tau.name = 'cloud optical depth'
  tau.units = '1'

if lplev7:
  plev7 = [90000., 74000., 62000., 50000., 37500., 24500., 9000.]
  plev7 = MV2.array(plev7,typecode=MV2.float32)
  plev7 = cdms2.createAxis(plev7)
  plev7.id = 'plev7'
  plev7.name = 'pressure'
  plev7.units = 'Pa'

if lmisr:
  cth16 = [1000.*x for x in [0., 0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.5, 4.5, 6., 8., 10., 12., 14.5, 16., 18.]]
  cth16 = MV2.array(cth16,typecode=MV2.float32)
  cth16 = cdms2.createAxis(cth16)
  cth16.id = 'cth16'
  cth16.name = 'altitude'
  cth16.units = 'm'


g = cdms2.open('Out_klevel.nc','w')
for var in var2save:
  if config.verbose >= 1:
    print var
  data = f(varnames[var])*coefs[var]
  data = MV2.array(data, typecode=MV2.float32)
  if var in ['cltcalipso','cllcalipso','clmcalipso','clhcalipso','clcalipso','cllcalipsoice','clmcalipsoice','clhcalipsoice','cltcalipsoice','cllcalipsoliq','clmcalipsoliq','clhcalipsoliq','cltcalipsoliq','cllcalipsoun','clmcalipsoun','clhcalipsoun','cltcalipsoun','clcalipso','lidarBetaMol532','clcalipsoice','clcalipsoliq','clcalipsoun','clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun','parasolRefl','cltlidarradar','clcalipso2','cltisccp','pctisccp','tauisccp','albisccp','meantbisccp','meantbclrisccp','boxtauisccp','boxptopisccp','cltmodis','clwmodis','climodis','clhmodis','clmmodis','cllmodis','tautmodis','tauwmodis','tauimodis','tautlogmodis','tauwlogmodis','tauilogmodis','reffclwmodis','reffclimodis','pctmodis','lwpmodis','iwpmodis','fracout', 'atb532', 'dbze94','cfadDbze94','cfadLidarsr532','clisccp', 'clmodis','clMISR']:
    data = MV2.where(data < -0.5e20, 1.e20,data)
    data = MV2.masked_values(data,1.e20)
    data.missing_value = 1.e20
  data.setAxis(0,time)
  if data.shape == (nt,nlev):
    data.setAxis(1,levels)
  if data.shape == (nt,nlev+1):
    data.setAxis(1,levels0)
  if var in ['clcalipso','clcalipso2','clcalipsoice','clcalipsoliq','clcalipsoun'] and data.shape == (nt,40):
    data.setAxis(1,alt40)
  if var in ['clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun'] and data.shape == (nt,40):
    data.setAxis(1,temp)
  if var in ['parasolRefl'] and data.shape == (nt,5):
    data.setAxis(1,sza5)
  if var in ['boxtauisccp','boxptopisccp'] and data.shape == (nt,ncol):
    data.setAxis(1,column)

  if var in ['fracout', 'atb532', 'dbze94']:
    data.setAxis(1,column)
    data.setAxis(2,levels)
  if var in ['cfadDbze94']:
    data.setAxis(1,dbze)
    data.setAxis(2,alt40)
  if var in ['cfadLidarsr532']:
    data.setAxis(1,sratio)
    data.setAxis(2,alt40)
  if var in ['clisccp', 'clmodis']:
    data.setAxis(1,tau)
    data.setAxis(2,plev7)
  if var in ['clMISR']:
    data.setAxis(1,tau)
    data.setAxis(2,cth16)


  data.id = var
  if names.has_key(var):
    data.long_name = names[var]
    data.units = varunits[var]

#  if var == 'ZFT':
#    for ilev in range(0,nlev):
#      data[:,ilev] = data[:,ilev]*(100000./levels[ilev])**(2./7.)
#    data.id = 'dthdt_ls'
#    data.long_name = 'Potential temperature tendency from large-scale forcings'

  g.write(data)

g.close()
f.close()
