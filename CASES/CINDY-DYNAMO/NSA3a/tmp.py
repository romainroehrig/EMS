import cdms2, vcs

f = cdms2.open('cindy_nsa3a_extended.nc')
#g = cdms2.open('...')

variables1 = ['qv','temp','u','v']
variables2 = ['hus','ta','u','v']

namesimu = {}

namesimu['qv'] = 'hus'
namesimu['temp'] = 'ta'
namesimu['u'] = 'u'

pp = f('pp',squeeze=1)
lev = pp[0,1:]
lev =cdms2.createAxis(lev)
lev.designateLevel()
lev.id = 'level'
lev.units = 'hPa'

x =vcs.init()

for var in variables1:	

  data = f(var,squeeze=1)
  data = data[:,1:]

  data.setAxis(1,lev)

#  datasimu = f(namesimu[var])

  xyvsy = x.createxyvsy(var)
#  xyvsy.datawc_x1 = ...

  x.xyvsy(data[0],bg=1)
  x.png(var)
  x.clear()

f.close()
