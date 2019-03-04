import cdms2
import MV2
import os
import interpvertp
import config
import math

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

lforc = config.lforc
lnam1D = config.lnam1D
#ASCII2FA=config.ASCII2FA

nlev_out = config.nlev

#dirout = 'files_L' + str(nlev_out) + '_' + str(int(config.dt)) + 's/'

#---------------------------------------------------------------
# Lecture des donnees d'entree
#---------------------------------------------------------------

var2read = ['pressure','temp','qv','u','v','ps']
var2interpol = ['temp','qv','u','v']

data_in = {}

f = cdms2.open('data_input.nc')

lat = f['temp'].getLatitude()[0]
lon = f['temp'].getLongitude()[0]

t0 = f['temp'].getAxis(0)[0]
units0 = f['temp'].getAxis(0).units

attributes = {}
for att in ['tadvh','qdvh','qtadvh','uadvh','vadvh','tadvv','qadvv','qtadvv','uadvv','vadvv','tadv','qadv','uadv','vadv','trad','forc_omega','forc_w','forc_geo','nudging_t','nudging_q','nudging_u','nudging_v','surfaceForcing','z0','ustar']:
  attributes[att] = 0

for att in f.listglobal():
  attributes[att] = f.getglobal(att)	

LRAYFM = True

zorog = float(attributes['zorog'])

startDate = str(f.startDate)
year = int(startDate[0:4])
month = int(startDate[4:6])
day = int(startDate[6:8])
hour = int(startDate[8:10])
minute = int(startDate[10:12])

nb_f=0
nb_fs=0

if attributes['tadv'] == 1:
  var2read.append('tadv')	
  var2interpol.append('tadv')
  time = f('tadv').getTime()
  nb_f=nb_f+1
if attributes['qadv'] == 1:
  var2read.append('qadv')	
  var2interpol.append('qadv')
  nb_f=nb_f+1
if attributes['tadvh'] == 1:
  var2read.append('tadvh')	
  var2interpol.append('tadvh')
  time = f('tadvh').getTime()
  nb_f=nb_f+1
if attributes['qadvh'] == 1:
  var2read.append('qadvh')	
  var2interpol.append('qadvh')
  nb_f=nb_f+1
if attributes['qtadvh'] == 1:
  var2read.append('qtadvh')	
  var2interpol.append('qtadvh')
  nb_f=nb_f+1
if attributes['uadvh'] == 1:
  var2read.append('uadvh')	
  var2interpol.append('uadvh')
  nb_f=nb_f+1
if attributes['vadvh'] == 1:
  var2read.append('vadvh')	
  var2interpol.append('vadvh')
  nb_f=nb_f+1
if attributes['tadvv'] == 1:
  var2read.append('tadvv')	
  var2interpol.append('tadvv')
  nb_f=nb_f+1
if attributes['qadvv'] == 1:
  var2read.append('qadvv')	
  var2interpol.append('qadvv')
  nb_f=nb_f+1
if attributes['uadvv'] == 1:
  var2read.append('uadvv')	
  var2interpol.append('uadvv')
  nb_f=nb_f+1
if attributes['vadvv'] == 1:
  var2read.append('vadvv')	
  var2interpol.append('vadvv')
  nb_f=nb_f+1
if attributes['trad'] == 'adv':
  LRAYFM = False  	
if attributes['trad'] == 1:
  LRAYFM = False	
  var2read.append('trad')	
  var2interpol.append('trad')
  nb_f=nb_f+1
if attributes['forc_omega'] == 1:
  var2read.append('omega')	
  var2interpol.append('omega')
  time = f('omega').getTime()
  nb_f=nb_f+1
if attributes['forc_w'] == 1:
  var2read.append('w')	
  var2interpol.append('w')
  time = f('w').getTime()
  nb_f=nb_f+1
if attributes['forc_geo'] == 1:
  var2read.append('ug')	
  var2interpol.append('ug')
  var2read.append('vg')	
  var2interpol.append('vg')
  time = f('ug').getTime()
  nb_f=nb_f+2

if attributes['surfaceForcing'] == "surfaceFlux" :
  var2read.append('sfc_sens_flx')	
  var2read.append('sfc_lat_flx')	
  time_sfc = f('sfc_lat_flx').getAxis(0)
  nt_sfc = time_sfc.shape[0]
  nb_fs=nb_fs+2

if attributes['ustar'] > 0. :
  nb_fs=nb_fs+1

if attributes['nudging_u'] > 0.:
  var2read.append('u')
  var2interpol.append('u')
  nb_f=nb_f+1

if attributes['nudging_v'] > 0.:
  var2read.append('v')
  var2interpol.append('v')
  nb_f=nb_f+1

if attributes['nudging_t'] > 0.:
  var2read.append('temp')
  var2interpol.append('temp')
  time = f('temp').getTime()
  nb_f=nb_f+1

if attributes['nudging_q'] > 0.:
  var2read.append('qv')
  var2interpol.append('qv')
  nb_f=nb_f+1

var2read = set(var2read)
var2interpol = set(var2interpol)

print 'var2read =', var2read
print 'var2interpol =', var2interpol

for var in var2read:
  data_in[var] = f(var,squeeze=1)	
#  data_in[var] = f(var)

#time = f('omega').getTime()
#f.close()

#if len(data_in['w']).shape) == 1:
#time = f('w').getTime()	
#else:
#  time = data_in['w'].getTime()
nt = time.shape[0]
lev_in = data_in['pressure'].getLevel()
nlev_in = lev_in.shape[0]

if attributes['trad'] == 1:
  if attributes['tadvh'] == 1:	
    data_in['tadvh'] = data_in['tadvh'] + data_in['trad']

pres = data_in['pressure']
if len(data_in['pressure'].shape) == 1:
  pres = MV2.zeros((nt,nlev_in),typecode=MV2.float)
  for it in range(0,nt):
    pres[it,:] = data_in['pressure'][:]	 
  pres0  = MV2.zeros((1,nlev_in),typecode=MV2.float)
  pres0[0,:] = data_in['pressure'][:]

#---------------------------------------------------------------
# Calcul des niveaux pressions aux interfaces
#---------------------------------------------------------------

vah = MV2.zeros((nlev_out+1),typecode=MV2.float32)
vbh = MV2.zeros((nlev_out+1),typecode=MV2.float32)

f = open('L' + str(nlev_out) + '.dta')

for ilev in range(0,nlev_out+1):
  line = f.readline().split()
  vah[ilev] = float(line[0])
  vbh[ilev] = float(line[1])

f.close()

ppf = MV2.zeros((nt,nlev_out+1),typecode=MV2.float32)

for ilev in range(0,nlev_out+1):
  if data_in['ps'].shape is (): #nt == 1:
    ppf[:,ilev] = vah[ilev] + vbh[ilev]*data_in['ps']	  
  else:
    ppf[:,ilev] = vah[ilev] + vbh[ilev]*data_in['ps'][:]

#---------------------------------------------------------------
# Calcul des niveaux pressions au milieu des couches
#---------------------------------------------------------------

pph = MV2.zeros((nt,nlev_out),typecode=MV2.float32)
pph0 = MV2.zeros((1,nlev_out),typecode=MV2.float32)

ppf = MV2.where(ppf < 0.1,0.1,ppf)

for ilev in range(0,nlev_out):
  pph[:,ilev] = (ppf[:,ilev+1]*MV2.log(ppf[:,ilev+1])-ppf[:,ilev]*MV2.log(ppf[:,ilev]))/(ppf[:,ilev+1]-ppf[:,ilev]) - 1.
  pph0[0,ilev] = (ppf[0,ilev+1]*MV2.log(ppf[0,ilev+1])-ppf[0,ilev]*MV2.log(ppf[0,ilev]))/(ppf[0,ilev+1]-ppf[0,ilev]) - 1.

pph = MV2.exp(pph)	  
pph0 = MV2.exp(pph0)

#---------------------------------------------------------------
# Interpolation verticale vers les niveaux modeles (pph)
#---------------------------------------------------------------

data_out = {}
missing = {}
for var in var2interpol:
  if len(data_in[var].shape) == 2 :
    data_out[var],missing[var] = interpvertp.interpvertp(pres,pph,data_in[var])
  else:
    tmp = data_in[var].reshape((1,nlev_in))
    data_out[var],missing[var] = interpvertp.interpvertp(pres0,pph0,tmp)

#---------------------------------------------------------------
# Ecriture du fichier pour etat initial ARPEGE
#---------------------------------------------------------------

if lnam1D:

  g = open('nam1D_L' + str(nlev_out),'w')

  print >>g, '&NAM1D'
  print >>g, '  LMAP    = .FALSE.,'
  print >>g, '  IFLEV   = ' + str(int(nlev_out)) + ','
  print >>g, '  ZDELY   = 250000.,'
  print >>g, '  LNHDYN  = .FALSE.,'
  print >>g, '  LALAPHYS= .TRUE.,'
  print >>g, '  LREASUR = .TRUE.,'
  print >>g, '  NFORC   ='+str(int(nb_f*nt))+ ','  
  print >>g, '  NFORCS  ='+str(int(nb_fs*nt_sfc))+ ','  
  print >>g, '  LQCGRP  = .FALSE.,'
  print >>g, '  LQIGRP  = .FALSE.,'
  print >>g, '  LQRGRP  = .FALSE.,'
  print >>g, '  LQSGRP  = .FALSE.,'
  print >>g, '  LQGGRP  = .FALSE.,'
  print >>g, '  LCFGRP  = .FALSE.,'
  print >>g, '  LSRCGRP = .FALSE.,'
  print >>g, '  LTKEGRP = .FALSE.,'
  print >>g, '  IYEAR   = ' + str(int(year)) + ','
  print >>g, '  IMONTH  = ' + str(int(month)) + ','
  print >>g, '  IDAY    = ' + str(int(day)) + ','
  print >>g, '  IHH     = ' + str(int(hour)) + ','
  print >>g, '  IMIN    = ' + str(int(minute)) + ','
  print >>g, '/'

  print >>g, 'ETA'

  print >>g, 'vah'
  for ilev in range(0,nlev_out+1):
    print >>g, vah[ilev]	

  print >>g, 'vbh'
  for ilev in range(0,nlev_out+1):
    print >>g, vbh[ilev]	

  print >>g, 'ATMOSPHERE'
  print >>g, 'zorog'
  print >>g, zorog
  print >>g, 'ps (Pa)'
  if data_in['ps'].shape is ():
    tmp=MV2.log(data_in['ps']  )
  else:	  
    tmp=MV2.log(data_in['ps'][0])
  print >>g, tmp

  print >>g, 'U'
  for ilev in range(0,nlev_out):
    print >>g, data_out['u'][0,ilev]

  print >>g, 'V'
  for ilev in range(0,nlev_out):
    print >>g, data_out['v'][0,ilev]

  print >>g, 'T'
  for ilev in range(0,nlev_out):
    print >>g, data_out['temp'][0,ilev]

  print >>g, 'QV'
  for ilev in range(0,nlev_out):
    print >>g, max(0.,data_out['qv'][0,ilev])

  print >>g, ' FORCING'
  if nt == 1:
    dt = 0.
  else:  
    dt = time[1]-time[0]
  if attributes['tadv'] == 1:
    for it in range(0,nt): 
      print >>g,' T ADV ',int(it*dt) ,'s'
      for ilev in range(0,nlev_out):
        print >>g, data_out['tadv'][it,ilev]
  elif attributes['tadvh'] == 1:
    for it in range(0,nt): 
      print >>g,' T ADV ',int(it*dt) ,'s'
      for ilev in range(0,nlev_out):
        print >>g, data_out['tadvh'][it,ilev]
  if attributes['qadv'] == 1:
    for it in range(0,nt): 
      print >>g,' Qv ADV ',int(it*dt) ,'s'
      for ilev in range(0,nlev_out):
        print >>g, data_out['qadv'][it,ilev]	
  elif attributes['qadvh'] == 1:
    for it in range(0,nt): 
      print >>g,' Qv ADV ',int(it*dt) ,'s'
      for ilev in range(0,nlev_out):
        print >>g, data_out['qadvh'][it,ilev]	        
  if attributes['forc_geo'] == 1:
    for it in range(0,nt): 
      print >>g,' PFUG ',int(it*dt) ,'s'
      for ilev in range(0,nlev_out):
        print >>g, data_out['ug'][it,ilev]
    for it in range(0,nt): 
      print >>g,' PFVG ',int(it*dt),'s'
      for ilev in range(0,nlev_out):
        print >>g, data_out['vg'][it,ilev]
  if attributes['forc_w'] == 1:
    for it in range(0,nt): 
      print >>g,' W ',int(it*dt),'s'
      for ilev in range(0,nlev_out):
        print >>g, data_out['w'][it,ilev]
  if attributes['surfaceForcing'] == "surfaceFlux" :
    print >>g, 'SURF.FORC'
    for it in range(0,nt_sfc):
      print >>g, 'FCS'
      print >>g, data_in['sfc_sens_flx'][it]
    for it in range(0,nt_sfc):
      print >>g, 'FLE'
      print >>g, data_in['sfc_lat_flx'][it]
  if attributes['ustar'] >0. :
    for it in range(0,nt_sfc):
      print >>g, 'USTAR'
      print >>g, str(float(attributes['ustar']))

  for var in config.variablesAux.keys():
    print >>g, var
    if var == 'SURFZ0.FOIS.G' and attributes['z0']>0. :
        print >>g, str(9.80665*float(attributes['z0']))
    elif var == 'SURFGZ0.THERM'  and attributes['z0']>0. :
        print >>g, str(9.80665*float(attributes['z0'])/10.)
    else:
        print >>g, ' ' + str(config.variablesAux[var])

  print >>g, 'STOP'

#  os.system('cp nam1D_L' + str(nlev_out) + ' nam1D')
#  os.system(ASCII2FA + ' > ascii2fa_' + str(nlev_out) + '.log 2>&1')
#  os.system('rm -f nam1D')
#  os.system('mv 1D.file initfile_L' + str(nlev_out))


  g.close()

  g = cdms2.open('init_L'+ str(nlev_out) + '.nc','w')
  levAxis = pph[0,:] #/100.
  levAxis = cdms2.createAxis(levAxis)
  levAxis.designateLevel()
  levAxis.id = 'level'
  levAxis.units = 'Pa'
  ntAxis = time_sfc[:]
  ntAxis = cdms2.createAxis(ntAxis)
  ntAxis.designateLevel()
  ntAxis.id = 'time'
  ntAxis.units = 's'

  for var in ['u','v','temp','qv']:
    tmp = MV2.array(data_out[var][0,:],typecode=MV2.float32)
    tmp.setAxis(0,levAxis)
    tmp.id = var
    g.write(tmp)

  for var in ['sfc_sens_flx','sfc_lat_flx']:
    tmp = MV2.array(data_in[var][:],typecode=MV2.float32)
    tmp.setAxis(0,ntAxis)
    tmp.id = var
    g.write(tmp)
  #
  g.close()

