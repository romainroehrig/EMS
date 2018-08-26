import cdms2, MV2
import cdtime

value = 0
cdms2.setNetcdfShuffleFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateFlag(value) ## where value is either 0 or 1
cdms2.setNetcdfDeflateLevelFlag(value) ## where value is a integer between 0 and 9 included

data = {}

f = open('data_create_profiles/data_received_from_francoise/new_flux_ideal.dat')

for i in range(0,3):
  f.readline()

line = f.readline()
nt = int(float(line.split()[0]))

for ifield in range(0,8):
    f.readline()
    tmp = []
    for i in range(0,20):
        line = f.readline()
        tmp = tmp + line.split()
    data[ifield] = MV2.array([float(x) for x in tmp],typecode=MV2.float)

f.close()


f = open('data_create_profiles/data_received_from_francoise/KIT_IDEAL/DATA/skin_t_idea_2days.dat')

f.readline()
tmp = []
for i in range(0,97):
    line = f.readline()
    tmp.append(line.split()[3])

ifield = 8
data[ifield] = MV2.array([float(x) for x in tmp],typecode=MV2.float)

f.close()


time = MV2.zeros(nt,typecode=MV2.float)
units = 'seconds since 1997-06-27 00:00:0.0'

for it in range(0,nt):
    tt = cdtime.comptime(int(data[1][it]),int(data[2][it]),int(data[3][it]),int(data[4][it]),int(data[5][it]))
#    print tt
    time[it] = tt.torel(units).value

time = cdms2.createAxis(time)
time.designateTime()
time.id = 'time'
time.units = units
time.calendar = 'gregorian'

var = {}
var[6] = 'sfc_lat_flx'
var[7] = 'sfc_sens_flx'
var[8] = 'ts'

for i in range(6,9):
    data[i].id = var[i]
    data[i].setAxis(0,time)
    if var[i] in ['sfc_lat_flx','sfc_sens_flx']:
      data[i].units = 'W m-2'
    if var[i] in ['ts']:
      data[i].units = 'K'

g = cdms2.open('ARMCVP_sfc_FG.nc','w')
for i in range(6,9):
    g.write(data[i])

g.close()
