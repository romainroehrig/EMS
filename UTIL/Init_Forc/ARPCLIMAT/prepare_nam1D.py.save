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
lDEPHY = config.lDEPHY
#ASCII2FA=config.ASCII2FA

nlev_out = config.nlev

dirout = 'files_L' + str(nlev_out) + '_' + str(int(config.dt)) + 's/'

#---------------------------------------------------------------
# Lecture des donnees d'entree
#---------------------------------------------------------------

var2read = ['pressure','temp','qv','ql','qi','tke','u','v','ps']
var2interpol = ['temp','qv','ql','qi','tke','u','v','theta']

data_in = {}

f = cdms2.open('data_input.nc')

print f.listvariables()

lat = f['temp'].getLatitude()[0]
lon = f['temp'].getLongitude()[0]

t0 = f['temp'].getAxis(0)[0]
units0 = f['temp'].getAxis(0).units

attributes = {}
for att in ['tadvh','qadvh','qvadvh','qvadv','qvadvv','qtadvh','uadvh','vadvh','tadvv','qadvv','qtadvv','uadvv','vadvv','tadv','qadv','uadv','vadv','trad','forc_omega','forc_w','forc_geo','nudging_t','nudging_temp','nudging_qv','nudging_u','nudging_v','rad_temp','adv_temp','adv_qv']:
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

if attributes['tadv'] == 1:
  var2read.append('tadv')	
  var2interpol.append('tadv')
  time = f('tadv').getTime()
if attributes['adv_temp'] == 1:
  var2read.append('temp_adv')	
  var2interpol.append('temp_adv')
  time = f('temp_adv').getTime()
if attributes['qadv'] == 1:
  var2read.append('qadv')	
  var2interpol.append('qadv')
if attributes['qvadv'] == 1:
  var2read.append('qvadv')	
  var2interpol.append('qvadv')
if attributes['adv_qv'] == 1:
  var2read.append('qv_adv')	
  var2interpol.append('qv_adv')  
if attributes['tadvh'] == 1:
  var2read.append('tadvh')	
  var2interpol.append('tadvh')
  time = f('tadvh').getTime()
if attributes['qadvh'] == 1:
  var2read.append('qadvh')	
  var2interpol.append('qadvh')
if attributes['qvadvh'] == 1:
  var2read.append('qvadvh')	
  var2interpol.append('qvadvh')
if attributes['qtadvh'] == 1:
  var2read.append('qtadvh')	
  var2interpol.append('qtadvh')
if attributes['uadvh'] == 1:
  var2read.append('uadvh')	
  var2interpol.append('uadvh')
if attributes['vadvh'] == 1:
  var2read.append('vadvh')	
  var2interpol.append('vadvh')
if attributes['tadvv'] == 1:
  var2read.append('tadvv')	
  var2interpol.append('tadvv')
if attributes['qadvv'] == 1:
  var2read.append('qadvv')	
  var2interpol.append('qadvv')
if attributes['qvadvv'] == 1:
  var2read.append('qvadvv')	
  var2interpol.append('qadvv')
if attributes['uadvv'] == 1:
  var2read.append('uadvv')	
  var2interpol.append('uadvv')
if attributes['vadvv'] == 1:
  var2read.append('vadvv')	
  var2interpol.append('vadvv')
if lDEPHY:
  if attributes['rad_temp'] == 'adv':
    LRAYFM = False  	
  if attributes['rad_temp'] == 1:
    LRAYFM = False	
    var2read.append('temp_rad')
    var2interpol.append('temp_rad')    
else:
  if attributes['trad'] == 'adv':
    LRAYFM = False  	
  if attributes['trad'] == 1:
    LRAYFM = False	
    var2read.append('trad')
    var2interpol.append('trad')
if attributes['forc_omega'] == 1:
  var2read.append('omega')	
  var2interpol.append('omega')
  time = f('omega').getTime()
if attributes['forc_w'] == 1:
  var2read.append('w')	
  var2interpol.append('w')
  time = f('w').getTime()
if attributes['forc_geo'] == 1:
  var2read.append('ug')	
  var2interpol.append('ug')
  var2read.append('vg')	
  var2interpol.append('vg')
  time = f('ug').getTime()

if lDEPHY:
  if attributes['nudging_u'] > 0.:
    var2read.append('u_nudging')
    var2interpol.append('u_nudging')

  if attributes['nudging_v'] > 0.:
    var2read.append('v_nudging')
    var2interpol.append('v_nudging')

  if attributes['nudging_temp'] > 0.:
    var2read.append('temp_nudging')
    var2interpol.append('temp_nudging')
    time = f('temp_nudging').getTime()

  if attributes['nudging_qv'] > 0.:
    var2read.append('qv_nudging')
    var2interpol.append('qv_nudging')
else:
  if attributes['nudging_u'] > 0.:
    var2read.append('u')
    var2interpol.append('u')

  if attributes['nudging_v'] > 0.:
    var2read.append('v')
    var2interpol.append('v')

  if attributes['nudging_t'] > 0.:
    var2read.append('temp')
    var2interpol.append('temp')
    time = f('temp').getTime()

  if attributes['nudging_q'] > 0.:
    var2read.append('qv')
    var2interpol.append('qv')

var2read = set(var2read)
var2interpol = set(var2interpol)

print 'var2read =', var2read
print 'var2interpol =', var2interpol

for var in var2read:
    if var in f.listvariables():
        data_in[var] = f(var,squeeze=1)	
    else:
        data_in[var] = f('temp',squeeze=1)*0.
#  data_in[var] = f(var)


try:
  data_in['theta'] = f('theta',squeeze=1)
except:
    if len(data_in['temp'].shape) == 2:
        nt = data_in['temp'].shape[0]
        nlev = data_in['temp'].shape[1]
        data_in['theta'] = data_in['temp']*0.
        for it in range(0,nt):
          for ilev in range(0,nlev):
            data_in['theta'][it,ilev] = data_in['temp'][it,ilev]*(100000./data_in['pressure'][it,ilev])**(2./7.)
    elif len(data_in['temp'].shape) == 1:
        nlev = data_in['temp'].shape[0]
        data_in['theta'] = data_in['temp']*0.
        for ilev in range(0,nlev):
          data_in['theta'][ilev] = data_in['temp'][ilev]*(100000./data_in['pressure'][ilev])**(2./7.)
    else:
      print 'Shape unexpected:',data_in['theta'].shape,'for variable theta' 

#time = f('omega').getTime()
#f.close()

#if len(data_in['w']).shape) == 1:
#time = f('w').getTime()	
#else:
#  time = data_in['w'].getTime()
try:
  nt = time.shape[0]
except:
  time = data_in['temp'].getTime()
  nt = time.shape[0]

lev_in = data_in['pressure'].getLevel()
nlev_in = lev_in.shape[0]

if lDEPHY:
  if attributes['rad_temp'] == 1:
    if attributes['adv_temp'] == 1:	
      data_in['temp_adv'] = data_in['temp_adv'] + data_in['temp_rad']
else:
  if attributes['trad'] == 1:
    if attributes['tadvh'] == 1:	
      data_in['tadvh'] = data_in['tadvh'] + data_in['trad']
    if attributes['tadvv'] == 1:	
      data_in['tadvv'] = data_in['tadvv'] + data_in['trad']

pres = data_in['pressure']
if len(data_in['pressure'].shape) == 1:
  pres = MV2.zeros((nt,nlev_in),typecode=MV2.float)
  for it in range(0,nt):
    pres[it,:] = data_in['pressure'][:]	 
  pres0  = MV2.zeros((1,nlev_in),typecode=MV2.float)
  pres0[0,:] = data_in['pressure'][:]
else:
  pres0 = MV2.zeros((1,nlev_in),typecode=MV2.float)
  pres0[0,:] = data_in['pressure'][0,:]

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
  # For reproductibility with CNRM machines
  for it in range(0,nt):
    pph[it,ilev] = (ppf[it,ilev+1]*math.log(ppf[it,ilev+1])-ppf[it,ilev]*math.log(ppf[it,ilev]))/(ppf[it,ilev+1]-ppf[it,ilev]) - 1.
  pph0[0,ilev] = (ppf[0,ilev+1]*math.log(ppf[0,ilev+1])-ppf[0,ilev]*math.log(ppf[0,ilev]))/(ppf[0,ilev+1]-ppf[0,ilev]) - 1.

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
# Ecriture du fichier pour etat initial ARPEGE-Climat
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
  print >>g, '  NFORC   = 0,'
  print >>g, '  LQCGRP  = .TRUE.,'
  print >>g, '  LQIGRP  = .TRUE.,'
  print >>g, '  LQRGRP  = .FALSE.,'
  print >>g, '  LQSGRP  = .FALSE.,'
  print >>g, '  LQGGRP  = .FALSE.,'
  print >>g, '  LCFGRP  = .FALSE.,'
  print >>g, '  LSRCGRP = .FALSE.,'
  print >>g, '  LTKEGRP = .TRUE.,'
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
    print >>g, data_in['ps']	  
  else:	  
    print >>g, data_in['ps'][0]

  print >>g, 'U'
  for ilev in range(0,nlev_out):
    print >>g, data_out['u'][0,ilev]

  print >>g, 'V'
  for ilev in range(0,nlev_out):
    print >>g, data_out['v'][0,ilev]

  print >>g, 'T'
  for ilev in range(0,nlev_out):
    print >>g, data_out['temp'][0,ilev]
# Pas completement satisfaisant a ce stade...
#    if pph[0,ilev] >= 10000.:
#      print >>g, data_out['theta'][0,ilev]*(pph[0,ilev]/100000.)**(2./7.)
#    else:
#      # Pour eviter des plantages en haute atmosphere
#      print >>g, data_out['temp'][0,ilev]

  print >>g, 'QV'
  for ilev in range(0,nlev_out):
    print >>g, max(0.,data_out['qv'][0,ilev])

  print >>g, 'CLOUD_WATER'
  for ilev in range(0,nlev_out):
    print >>g, max(0.,data_out['ql'][0,ilev])

  print >>g, 'ICE_CRYSTAL'
  for ilev in range(0,nlev_out):
    print >>g, max(0.,data_out['qi'][0,ilev])

  print >>g, 'TKE'
  for ilev in range(0,nlev_out):
    print >>g, max(0.,data_out['tke'][0,ilev])


  print >>g, 'FORCING'

  for var in config.variablesAux.keys():
    print >>g, var
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

  for var in ['u','v','temp','qv','theta']:
    tmp = MV2.array(data_out[var][0,:],typecode=MV2.float32)
    tmp.setAxis(0,levAxis)
    tmp.id = var
    g.write(tmp)

  # Multiply by 1 to really do a deep copy of the array !
  # Otherwise weird things hereafter.
  tmp = MV2.array(data_out['temp'][0,:]*1.,typecode=MV2.float32)
  nlev, = levAxis.shape
  for ilev in range(0,nlev):
    tmp[ilev] = tmp[ilev]*(100000./levAxis[ilev])**(2./7.)

  tmp.setAxis(0,levAxis)
  tmp.id = 'theta2'
  g.write(tmp)  

  g.close()




#---------------------------------------------------------------
# Ecriture des forcages ARPEGE-Climat
#---------------------------------------------------------------

if lforc:

  if nt == 1:
    dt = 0.
  else:  
    dt = time[1]-time[0]	

  if lDEPHY:
    if attributes['nudging_temp'] > 0.:
      g = open(dirout + 'temp_nudging_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Temperature', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['temp_nudging'][it,ilev]	
      g.close()

    if attributes['nudging_qv'] > 0.:
      g = open(dirout + 'qv_nudging_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Specific Humidity', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['qv_nudging'][it,ilev]	
      g.close()

    if attributes['nudging_u'] > 0.:
      g = open(dirout + 'u_nudging_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Zonal Wind', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['u_nudging'][it,ilev]	
      g.close()

    if attributes['nudging_v'] > 0.:
      g = open(dirout + 'v_nudging_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Meridional Wind', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['v_nudging'][it,ilev]	
      g.close()
  else:
    if attributes['nudging_t'] > 0.:
      g = open(dirout + 'temp_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Temperature', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['temp'][it,ilev]	
      g.close()

    if attributes['nudging_q'] > 0.:
      g = open(dirout + 'qv_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Specific Humidity', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['qv'][it,ilev]	
      g.close()

    if attributes['nudging_u'] > 0.:
      g = open(dirout + 'u_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Zonal Wind', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['u'][it,ilev]	
      g.close()

    if attributes['nudging_v'] > 0.:
      g = open(dirout + 'v_profiles_L' + str(nlev_out),'w')
      for it in range(0,nt):
        print >>g, 'Meridional Wind', int(dt*it)    	  
        for ilev in range(0,nlev_out):
          print >>g, data_out['v'][it,ilev]	
      g.close()

  if attributes['forc_geo'] > 0.:
    g = open(dirout + 'ug_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Geostrophic Zonal Wind', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['ug'][it,ilev]	
    g.close()
    g = open(dirout + 'vg_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Geostrophic Meridional Wind', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['vg'][it,ilev]	
    g.close()

  if attributes['forc_omega'] == 1:
    g = open(dirout + 'omega_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Omega', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['omega'][it,ilev]	
    g.close()

  if attributes['forc_w'] == 1:
    g = open(dirout + 'w_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'W', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['w'][it,ilev]	
    g.close()

  if attributes['tadvh'] == 1:
    g = open(dirout + 'tadvh_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Horizonal Temperature Advection', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['tadvh'][it,ilev]	
    g.close()

  if attributes['qadvh'] == 1:
    g = open(dirout + 'qadvh_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Horizontal Specific Humidity Advection', int(dt*it) 
      for ilev in range(0,nlev_out):
        print >>g, data_out['qadvh'][it,ilev]	  
    g.close()

  if attributes['qvadvh'] == 1:
    g = open(dirout + 'qvadvh_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Horizontal Specific Humidity Advection', int(dt*it) 
      for ilev in range(0,nlev_out):
        print >>g, data_out['qvadvh'][it,ilev]	  
    g.close()

  if attributes['qtadvh'] == 1:
    g = open(dirout + 'qadvh_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Horizontal Specific Humidity Advection (qt)', int(dt*it) 
      for ilev in range(0,nlev_out):
        print >>g, data_out['qtadvh'][it,ilev]	  
    g.close()

  if attributes['uadvh'] == 1:
    g = open(dirout + 'uadvh_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Horizonal Zonal Wind Advection', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['uadvh'][it,ilev]	
    g.close()

  if attributes['vadvh'] == 1:
    g = open(dirout + 'vadvh_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Horizontal Meridional Wind Advection', int(dt*it) 
      for ilev in range(0,nlev_out):
        print >>g, data_out['vadvh'][it,ilev]	  
    g.close()

  if attributes['tadvv'] == 1:
    g = open(dirout + 'tadvv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Vertical Temperature Advection', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['tadvv'][it,ilev]	
    g.close()

  if attributes['qadvv'] == 1:
    g = open(dirout + 'qadvv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Vertical Specific Humidity Advection', int(dt*it)
      for ilev in range(0,nlev_out):
        print >>g, data_out['qadvv'][it,ilev]	  
    g.close()

  if attributes['qvadvv'] == 1:
    g = open(dirout + 'qvadvv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Vertical Specific Humidity Advection', int(dt*it)
      for ilev in range(0,nlev_out):
        print >>g, data_out['qvadvv'][it,ilev]	  
    g.close()

  if attributes['tadv'] == 1:
    g = open(dirout + 'tadv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Total Temperature Advection', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['tadv'][it,ilev]	
    g.close()

  if attributes['adv_temp'] == 1:
    g = open(dirout + 'temp_adv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Total Temperature Advection', int(dt*it)
      for ilev in range(0,nlev_out):
        print >>g, data_out['temp_adv'][it,ilev]
    g.close()

  if attributes['trad'] == 1:
    g = open(dirout + 'trad_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Temperature Radiative Tendency', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['trad'][it,ilev]	
    g.close()

  if attributes['rad_temp'] == 1:
    g = open(dirout + 'temp_rad_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Temperature Radiative Tendency', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['temp_rad'][it,ilev]	
    g.close()

  if attributes['qadv'] == 1:
    g = open(dirout + 'qadv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Total Specific Humidity Advection', int(dt*it)
      for ilev in range(0,nlev_out):
        print >>g, data_out['qadv'][it,ilev]	  
    g.close()

  if attributes['qvadv'] == 1:
    g = open(dirout + 'qvadv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Total Specific Humidity Advection', int(dt*it)
      for ilev in range(0,nlev_out):
        print >>g, data_out['qvadv'][it,ilev]	  
    g.close()

  if attributes['adv_qv'] == 1:
    g = open(dirout + 'qv_adv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Total Specific Humidity Advection', int(dt*it)
      for ilev in range(0,nlev_out):
        print >>g, data_out['qv_adv'][it,ilev]
    g.close()

  if attributes['uadv'] == 1:
    g = open(dirout + 'uadv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Total Zonal Wind Advection', int(dt*it)    	  
      for ilev in range(0,nlev_out):
        print >>g, data_out['uadv'][it,ilev]	
    g.close()

  if attributes['vadv'] == 1:
    g = open(dirout + 'vadv_profiles_L' + str(nlev_out),'w')
    for it in range(0,nt):
      print >>g, 'Total Meridional Wind Advection', int(dt*it)
      for ilev in range(0,nlev_out):
        print >>g, data_out['vadv'][it,ilev]	  
    g.close()

  g = open(dirout + 'ps','w')
  for it in range(0,nt):
    print >>g, 'Surface Pressure', int(dt*it)    
    if data_in['ps'].shape is ():
      print >>g, data_in['ps']
    else:	    
      print >>g, data_in['ps'][it]
  g.close()
