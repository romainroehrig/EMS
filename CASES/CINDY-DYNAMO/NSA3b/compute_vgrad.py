import cdms2
import MV2
import cdtime
import sys, os
import math

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value)
cdms2.setNetcdfDeflateLevelFlag(value)

Ra = 6371*1000  # rayon de la Terre (en metre)
Cp = 1004
Lv = 2.5e6

repin = './'

var2read = ['ta','hus']

dico = {}
dico['ta'] = 'temp'
dico['hus'] = 'qv'

datain = {}

for var in var2read:
  print var 
  f = cdms2.open('cindy_nsa3b.nc')
  datain[var] = f(dico[var],squeeze=1)[:,1:]
  lev = f('pp',squeeze=1)[0,1:]
  lev = cdms2.createAxis(lev)
  lev.designateLevel()
  lev.id = 'lev'
  lev.units = 'Pa'
  datain[var].setAxis(1,lev)
  f.close()
 
nt,nlev = datain[var].shape
lev = datain[var].getLevel()
time = datain[var].getTime()
tunits = time.units
datain['theta'] = datain['ta']*0.
for ilev in range(0,nlev):
  datain['theta'][:,ilev] = datain['ta'][:,ilev]*(100000./(lev[ilev]*100.))**(2./7.)     

datain['theta'].id = 'theta'

dp = MV2.zeros((nt,nlev),typecode=MV2.float32)

for ilev in range(0,nlev-1):
    dp[:,ilev] = (lev[ilev+1]-lev[ilev])*100.

dp[:,0] = (lev[1]-lev[0])*100.

dataout = {}
missing = 1.e20

for var in ['theta','hus','ta']:
  print var

  dvarp = MV2.zeros((nt,nlev),typecode=MV2.float32) + missing
  dvarp[:,1:nlev] = (datain[var][:,1:nlev] - datain[var][:,0:nlev-1])/dp[:,0:nlev-1]
  dvarp[:,0] = (datain[var][:,1] - datain[var][:,0])/dp[:,0]

  dataout['d' + var + '_dp']  = dvarp

  vv = 'd' + var + '_dp'
  dataout[vv] = MV2.masked_values(dataout[vv],missing)
  dataout[vv].missing_value = missing
  dataout[vv].id = vv
  dataout[vv].setAxis(0,time)
  dataout[vv].setAxis(1,lev)

  g = cdms2.open('{0}/{1}.nc'.format(repin,vv),'w')
  g.write(dataout[vv])
  g.close()


  if var == 'theta':
      var2 = 't'
      vv2 = 'd' + var2 + '_dp2'
      dataout[vv2] = dataout[vv]*0.
      for ilev in range(0,nlev):
        dataout[vv2][:,ilev] =  dataout[vv][:,ilev]*((lev[ilev]*100.)/100000.)**(2./7.)
      dataout[vv2].id = vv2

      g = cdms2.open('{0}/{1}.nc'.format(repin,vv2),'w')
      g.write(dataout[vv2])
      g.close()


