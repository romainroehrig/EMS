import cdms2
import cdtime

f = cdms2.open('data_input.nc')
surfaceForcing = f.surfaceForcing
surfaceType = f.surfaceType
zorog = float(f.zorog)
startDate = str(f.startDate)
endDate = str(f.endDate)
if surfaceForcing == 'ts':
  sst = f('ts')
  lat = sst.getLatitude()[0]
  lon = sst.getLongitude()[0]
  time = sst.getTime()
elif surfaceForcing == 'surfaceFlux':
  hfls = f('sfc_lat_flx')
  hfss = f('sfc_sens_flx')
  lat = hfls.getLatitude()[0]
  lon = hfls.getLongitude()[0]
  time = hfls.getAxis(0)
f.close()

year = int(startDate[0:4])
month = int(startDate[4:6])
day = int(startDate[6:8])
hour = int(startDate[8:10])
minute = int(startDate[10:12])
t0 = cdtime.comptime(year,month,day,hour,minute)

tunits0 = 'seconds since ' + str(year) + '-' + str(month) + '-' + str(day) + ' ' + str(hour) + ':' + str(minute) + ':0.0'

seconds = minute*60 + hour*3600

year0 = int(endDate[0:4])
month0 = int(endDate[4:6])
day0 = int(endDate[6:8])
hour0 = int(endDate[8:10])
minute0 = int(endDate[10:12])
tend0 = cdtime.comptime(year0,month0,day0,hour0,minute0)

nt = time.shape[0]

ii1 = 0
ii2 = nt-1
for it in range(0,nt):
  tt = cdtime.reltime(time[it],time.units)
  if tt.torel(time.units).value == t0.torel(time.units).value:
    ii1 = it
  if tt.torel(time.units).value == tend0.torel(time.units).value:
    ii2 = it

nt0 = nt-ii1-(nt-1-ii2)

g = open('namsurf','w')

print >>g,  '&NAM_IO_OFFLINE'
print >>g,  "  CSURF_FILETYPE='LFI',"
print >>g,  "  CPGDFILE='PGD',"
print >>g,  "  CPREPFILE='PREP',"
print >>g,  "/"

print >>g,  "&NAM_PGD_SCHEMES"
if surfaceType == 'ocean' and surfaceForcing == 'ts':
  print >>g,  "  CNATURE = 'FLUX'  ,"
  print >>g,  "  CSEA    = 'SEAFLX',"
  print >>g,  "  CWATER  = 'FLUX'  ,"
  print >>g,  "  CTOWN   = 'FLUX'  ,"
if surfaceType == 'ocean' and surfaceForcing == 'surfaceFlux':
  print >>g,  "  CNATURE = 'NONE'  ,"
  print >>g,  "  CSEA    = 'FLUX' ,"
  print >>g,  "  CWATER  = 'NONE'  ,"
  print >>g,  "  CTOWN   = 'NONE'  ,"
if surfaceType == 'land' and surfaceForcing == 'surfaceFlux':
  print >>g,  "  CNATURE = 'NONE' ,"
  print >>g,  "  CSEA    = 'FLUX'  ,"
  print >>g,  "  CWATER  = 'NONE'  ,"
  print >>g,  "  CTOWN   = 'NONE'  ,"

print >>g,  "/"

print >>g,  "&NAM_PGD_GRID"
print >>g,  "  CGRID='CARTESIAN',"
print >>g,  "/"

print >>g,  "&NAM_CARTESIAN"
print >>g,  "  XLAT0 = " + str(lat) + ","
print >>g,  "  XLON0 = " + str(lon) + ","
print >>g,  "  NIMAX = 1," 
print >>g,  "  NJMAX = 4,"
print >>g,  "  XDX = 250000.,"
print >>g,  "  XDY = 250000.,"
print >>g,  "/"

print >>g,  "&NAM_FRAC"
print >>g,  "  LECOCLIMAP = F,"
if surfaceType == 'ocean' or surfaceType == 'land':
  print >>g,  "  XUNIF_SEA    = 1.,"
else:
  print >>g,  "  XUNIF_SEA    = 0.,"
print >>g,  "  XUNIF_WATER  = 0.,"
print >>g,  "  XUNIF_TOWN   = 0.,"
print >>g,  "  XUNIF_NATURE = 0.,"
#if surfaceType == 'land':
#  print >>g,  "  XUNIF_NATURE = 1.,"
#else:
#  print >>g,  "  XUNIF_NATURE = 0.,"
print >>g,  "/"

print >>g,  "&NAM_COVER"
print >>g,  "  XUNIF_COVER(1) = 1.,"
print >>g,  "/"

print >>g,  "&NAM_ZS"
print >>g,  "  XUNIF_ZS = " + str(zorog) + ","
print >>g,  "/"

print >>g,  "&NAM_PREP_SURF_ATM"
print >>g,  "  NYEAR= " + str(int(year)) + ","
print >>g,  "  NMONTH=" + str(int(month)) + ","
print >>g,  "  NDAY=" + str(int(day)) + ","
print >>g,  "  XTIME=" + str(int(seconds)) + ","
print >>g,  "/"

if surfaceType == 'ocean':
  print >>g,  "&NAM_PREP_SEAFLUX"
  if surfaceForcing == 'ts':
    print >>g,  "  XSST_UNIF=%(sst)6.2f,"%{"sst":sst[0]}
  else:
    print >>g,  "  XSST_UNIF=300.,"
  print >>g,  "  NYEAR=" + str(int(year)) + ","
  print >>g,  "  NMONTH=" + str(int(month)) + ","
  print >>g,  "  NDAY=" + str(int(day)) + ","
  print >>g,  "  XTIME=" + str(int(seconds)) + ","
  print >>g,  "/"

if surfaceForcing == 'ts':
  print >>g,  '&NAM_DATA_SEAFLUX'
  print >>g,  '  LSST_DATA = T,'
  print >>g,  '  NTIME_SST = ' + str(nt0) + ','

  for it in range(0,nt0):
    print >>g,  '  XUNIF_SST(%(ii)3.i) = %(sst)6.2f,'%{"ii": it+1, "sst": sst[it]}

  for it in range(0,nt0):
    print >>g,  "  CFTYP_SST(%(ii)3.i) = 'DIRECT',"%{"ii": it+1}

  for it in range(0,nt0):
    tt = cdtime.reltime(time[it+ii1],time.units)	
    print >>g,  "  NYEAR_SST(%(ii)3.i)=%(year)4.4i,  NMONTH_SST(%(ii)3.i)=%(month)2.2i,  NDAY_SST(%(ii)3.i)=%(day)2.2i , XTIME_SST(%(ii)3.i)=%(seconds)7.1f,"%{"ii": it+1, "year": tt.tocomp().year, "month": tt.tocomp().month , "day": tt.tocomp().day ,"seconds": tt.tocomp().hour*3600}

  print >>g,  '/'

print >>g, "&NAM_DIAG_SURFn"
print >>g, "  N2M            = 2,"
print >>g, "  LSURF_VARS     = T,"
print >>g, "  LSURF_BUDGET   = T,"
print >>g, "  LCOEF          = T,"
print >>g, "  LRAD_BUDGET    = T,"
print >>g, "  LSURF_BUDGETC  = T,"
print >>g, "  LRESET_BUDGETC = T"
print >>g,  '/'

if surfaceForcing == 'surfaceFlux':
  print >>g,  '&NAM_IDEAL_FLUX'
  print >>g,  '  NFORCF = ' + str(nt0)
  print >>g,  '  NFORCT = 2'
  print >>g,  "  CSFTQ='W/m2'"
  print >>g,  "  XSFCO2 = 0.,"
  print >>g,  "  CUSTARTYPE = 'Z0   ',"
  print >>g,  "  XUSTAR = 0.,"
  print >>g,  "  XZ0=0.01,"
  print >>g,  "  XALB   = 0.07,"
  print >>g,  "  XEMIS  = 1.,"
  print >>g,  "  XTIMET(1) = 0."
  tt = cdtime.reltime(time[ii2],time.units)
  print >>g,  "  XTIMET(2) = %(tt)6.2f,"%{"tt":tt.torel(tunits0).value}
  print >>g,  "  XTSRAD(1) = 299.2,"
  print >>g,  "  XTSRAD(2) = 299.2,"
  for it in range(0,nt0):
    tt = cdtime.reltime(time[it+ii1],time.units)	  
    print >>g, '  XTIMEF(' + str(it+1) + ') = %(tt)6.2f,'%{"tt":tt.torel(tunits0).value}
  for it in range(0,nt0):
    tt = cdtime.reltime(time[it+ii1],time.units)	  
    print >>g, '  XSFTH(' + str(it+1) + ') = %(hfss)6.2f,'%{"hfss":hfss[it+ii1,0,0]}
  for it in range(0,nt0):
    tt = cdtime.reltime(time[it+ii1],time.units)	  
    print >>g, '  XSFTQ(' + str(it+1) + ') = %(hfls)6.2f,'%{"hfls":hfls[it+ii1,0,0]}
  print >>g, '/'

g.close()
  
