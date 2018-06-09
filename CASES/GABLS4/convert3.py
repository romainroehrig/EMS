import cdms2
import numpy
import MV2
import cdtime

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value)
cdms2.setNetcdfDeflateLevelFlag(value)

lat = -75.1
lon = 123.33

lat = cdms2.createAxis(MV2.array([lat,],typecode=MV2.float))
lat.designateLatitude()
lat.id = 'lat'
lat.units = 'degrees_north'

lon = cdms2.createAxis(MV2.array([lon,],typecode=MV2.float))
lon.designateLongitude()
lon.id = 'lon'
lon.units = 'degrees_east'

#variables = ['Surface_temperature','Pinterp','zo','ps','orog','ustar','qvinterp','thinterp','vinterp','uginterp','uinterp','vginterp']
variables = ['Surface_temperature','Pinterp','zo','ps','orog','qvinterp','thinterp','vinterp','uginterp','uinterp','vginterp']

data = {}
attributes = {}

f = cdms2.open('GABLS4_24h_driver_test.nc')
for var in variables:
    data[var] = f(var,squeeze=1)

for att in f.listglobal():
    attributes[att] = f.getglobal(att)
    if type(attributes[att]) == numpy.ndarray:
        attributes[att] = int(attributes[att])

f.close()

f = cdms2.open('/cnrm/tropics/user/couvreux/GABLS4_INTERCOMPARISON_LES/POUR_OLIVIER/MESONH/24h/gabls4_time_les_MESONH_stage3_zo3.nc')
data['shf'] = f('shf')[:,0]
f.close()

data['tinterp'] = data['thinterp']*(data['Pinterp']/100000.)**(2./7.)

#nlev, = data['Pinterp'].shape
#data['Pinterp2'] = data['Pinterp']*0
#data['Pinterp2'][0] = 65100.
#data['Pinterp2'][1] = data['Pinterp2'][0] + (data['Pinterp'][2]-data['Pinterp'][1])
#for ilev in range(2,nlev):
#  data['Pinterp2'][ilev] = data['Pinterp2'][ilev-1] + (data['Pinterp'][ilev]-data['Pinterp'][ilev-1])
#  if data['Pinterp2'][ilev] <= 0.:
#    data['Pinterp2'][ilev] = data['Pinterp2'][ilev-1]/2.

attributes['startDate'] = '20091211000000'
attributes['endDate'] = '20091212000000'
attributes['author'] = 'F. Couvreux, R. Roehrig'
attributes['surfaceForcing'] = 'ts'
attributes['surfaceForcingWind'] = 'z0'
attributes['case'] = 'GABLS4'
attributes['description'] = 'No subsidence/ascendance, No T & q Large-scale advection, No radiation but geostrophic wind'
attributes['forc_geo'] = attributes['for_geo']
del(attributes['for_geo'])
attributes['zorog'] = 3223 #float(data['orog'])
attributes['z0'] = 0.001 #float(data['zo'])
#attributes['ustar'] = float(data['ustar'])
attributes['trad'] = 'adv'

dico = {}
dico['u'] = 'uinterp'
dico['v'] = 'vinterp'
dico['ug'] = 'uginterp'
dico['vg'] = 'vginterp'
dico['ts'] = 'Surface_temperature'
dico['pressure'] = 'Pinterp'
dico['qv'] = 'qvinterp'
dico['temp'] = 'tinterp'
dico['ps'] = 'ps'
dico['orog'] = 'orog'
#dico['ustar'] = 'ustar'
dico['z0'] = 'zo'

var2write = ['u','v','temp','qv','ug','vg','pressure','ps','ts']
var2write0 = ['ps']

tmp = data[dico['ts']]
nt, = tmp.shape
time = tmp.getAxis(0)
time = cdms2.createAxis(time[:])
time.designateTime()
time.id = 'time'
time.units = 'seconds since 2009-12-11 00:00:0.0'

newdata = {}
for var in var2write:
  tmp = data[dico[var]]
  if var in ['ts']:
    tmp = MV2.reshape(tmp,(nt,1,1))
    tmp.setAxis(0,time)
    tmp.setAxis(1,lat)
    tmp.setAxis(2,lon)

    tmp.id = var
    tmp.long_name = data[dico[var]].longname
    tmp.units = data[dico[var]].units

    newdata[var] = tmp
    
  elif not(var in var2write0):
    nlev, = tmp.shape
    lev = tmp.getAxis(0)
    lev = cdms2.createAxis(lev[:])
    lev.designateLevel()
    lev.id = 'lev'
    lev.units = 'm'
    lev.long_name = 'Altitude'
    tmp0 = MV2.zeros((nt,nlev,1,1),typecode=MV2.float32)
    for it in range(0,nt):
      tmp0[it,:,0,0] = tmp        
    tmp0.setAxis(0,time)
    tmp0.setAxis(1,lev)
    tmp0.setAxis(2,lat)
    tmp0.setAxis(3,lon)

    tmp0.id = var
    tmp0.long_name = data[dico[var]].longname
    tmp0.units = data[dico[var]].units

    newdata[var] = tmp0
  else:
    tmp0 = MV2.zeros((nt,1,1),typecode=MV2.float32)
    for it in range(0,nt):
      tmp0[it,0,0] = tmp
    tmp0.setAxis(0,time)
    tmp0.setAxis(1,lat)
    tmp0.setAxis(2,lon)
    tmp0.id = var
    if var == 'ps':
      tmp0.long_name = 'surface pressure'
      tmp0.units = 'Pa'
    else:
      tmp0.long_name = data[dico[var]].longname
      tmp0.units = data[dico[var]].units
    newdata[var] = tmp0


tmp = MV2.zeros((nt,1,1),typecode=MV2.float32)
t0 = cdtime.comptime(2009,12,11,0,0,0)
for it in range(0,nt):
    tt = t0.add(it,cdtime.Hour)
    tmin = tt.add(-0.5,cdtime.Hour)
    tmax = tt.add(0.5,cdtime.Hour)
    toto = data['shf'](time = (tmin,tmax))
    tmp[it] = MV2.average(toto,axis=0)

tmp.setAxis(0,time)
tmp.setAxis(1,lat)
tmp.setAxis(2,lon)
tmp.id = 'sfc_sens_flx'
tmp.long_name = 'Surface sensible heat flux'
tmp.units = 'W m-2'

newdata['sfc_sens_flx'] = tmp

var2write.append('sfc_sens_flx')

tmp = tmp*0.
tmp.id = 'sfc_lat_flx'
tmp.long_name = 'Surface latent heat flux'
tmp.units = 'W m-2'

newdata['sfc_lat_flx'] = tmp

var2write.append('sfc_lat_flx')


XUSTAR = MV2.zeros(25+1,typecode=MV2.float32)
XUSTAR[1]=0.138679
XUSTAR[2]=0.127735
XUSTAR[3]=0.143907
XUSTAR[4]=0.158719
XUSTAR[5]=0.168044
XUSTAR[6]=0.173109
XUSTAR[7]=0.177049
XUSTAR[8]=0.176655
XUSTAR[9]=0.163976
XUSTAR[10]=0.102127
XUSTAR[11]=0.0809859
XUSTAR[12]=0.0889696
XUSTAR[13]=0.0941792
XUSTAR[14]=0.0975210
XUSTAR[15]=0.0974786
XUSTAR[16]=0.0956932
XUSTAR[17]=0.0960392
XUSTAR[18]=0.0950178
XUSTAR[19]=0.0951953
XUSTAR[20]=0.0988678
XUSTAR[21]=0.103377
XUSTAR[22]=0.105738
XUSTAR[23]=0.119827
XUSTAR[24]=0.129067
XUSTAR[25]=0.129067

tmp = MV2.zeros((nt,1,1),typecode=MV2.float32)
tmp[:,0,0] = XUSTAR[1:]
tmp.setAxis(0,time)
tmp.setAxis(1,lat)
tmp.setAxis(2,lon)
tmp.id = 'ustar'
tmp.long_name = 'ustar'
tmp.units = 'm s-1'

newdata['ustar'] = tmp

var2write.append('ustar')


attributes['surfaceForcing'] = 'surfaceFlux'

g = cdms2.open('GABLS4_24h_driver_FC_RR_flux_z03.nc','w')
for var in var2write:
    g.write(newdata[var])
for att in sorted(attributes.keys()):
    g.__setattr__(att,attributes[att])
g.close()
