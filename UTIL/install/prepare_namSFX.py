#!/usr/bin/env python
# -*- coding:UTF-8 -*-

lperf = False
if lperf:
  import time as TT 
  TT00 = TT.time()

import namelist
import cdms2, cdtime
import sys

if lperf:
    TT1 = TT.time()
    print 'FIRST THINGS:', TT1-TT00
    TT00 = TT.time()

def prep_nam_SFX(case,filecase,namref,namout=None,subcase=None):
  """
    Prepare SURFEX namelist for MUSC simulation, 
    given information in filecase,
    and from SURFEX namelist namref
  """

  if lperf:
      TT0 = TT.time()

  if namout is None:
    namout = namref + '_' + case
    if subcase is not None:
      namout = namout + '_' + subcase        

  print '-'*40
  print 'Prepare SURFEX namelist for MUSC'
  print 'case:', case, 'subcase:', subcase
  print 'Reference namelist:', namref
  print 'Output namelist:', namout

  nam2keep = ['NAM_CARTESIAN','NAM_COVER','NAM_DIAG_SURFn','NAM_FRAC','NAM_IO_OFFLINE','NAM_PGD_GRID','NAM_PGD_SCHEMES','NAM_PREP_SURF_ATM','NAM_SURF_ATM','NAM_SURF_CSTS','NAM_ZS']

  if lperf:
    TT1 = TT.time()
    print 'First Things:', TT1-TT0
    TT0 = TT.time()

  nam = namelist.readsurfex(namref)

  if lperf:
    TT1 = TT.time()
    print 'Read namelist:', TT1-TT0
    TT0 = TT.time()
#  print nam.keys()
#  print nam['NAM_SURF_CSTS']

  # Remove a few namelists
  for nn in ['NAMDIM','NAMGEM','NAMRGRI','NAMVV1','NAMRGRI','NAM_IO_SURF_ARO','NAM_OASIS','NAM_SFX_LAND_CPL','NAM_DIAG_SURF_ATMn']:
    try:      
      del(nam[nn])
    except KeyError:
      pass
    nam[nn] = {}

  # Grid
  nam['NAM_PGD_GRID'] = {}
  nam['NAM_PGD_GRID']['CGRID'] = ["'CARTESIAN'",]
  nam['NAM_CARTESIAN'] = {}
  nam['NAM_CARTESIAN']['NIMAX'] = ['1',]
  nam['NAM_CARTESIAN']['NJMAX'] = ['4',]
  nam['NAM_CARTESIAN']['XDX'] = ['250000.',]
  nam['NAM_CARTESIAN']['XDY'] = ['250000.',]

  # Surface type
  nn='NAM_PGD_SCHEMES'
  for tt in ['CNATURE','CSEA','CTOWN','CWATER']:
    nam[nn][tt] = ["'NONE'",]
  nn='NAM_FRAC'
  nam[nn]['LECOCLIMAP'] = ['F',]
  for tt in ['NATURE','SEA','TOWN','WATER']:
    nam[nn]['XUNIF_' + tt] = ['0.',]
  nn = 'NAM_COVER'
  nam[nn] = {}
  nam[nn]['XUNIF_COVER(1)'] = ['1.',]
  nn = 'NAM_ZS'
  nam[nn] = {}
  nam[nn]['XUNIF_ZS'] = ['0.',]

  # NAM_IO_OFFLINE
  nn = 'NAM_IO_OFFLINE'
  nam[nn]['CSURF_FILETYPE'] = ["'LFI'",]
  nam[nn]['CPGDFILE'] = ["'PGD'",]
  nam[nn]['CPREPFILE'] = ["'PREP'",]

  # NAM_DIAG_SURFn
  nn = 'NAM_DIAG_SURFn'
  nam[nn] = {}
  nam[nn]['N2M'] = ['2',]
  for tt in ['LSURF_VARS','LSURF_BUDGET','LCOEF','LRAD_BUDGET','LSURF_BUDGETC','LRESET_BUDGETC']:
    nam[nn][tt] = ['T',]     

  # NAM_SEAFLUXn
  nn = 'NAM_SEAFLUXn'
  try:
    del(nam[nn]['CINTERPOL_SST'])
  except KeyError:
    pass

  # NAM_SEAICEn
  nn = 'NAM_SEAICEn'
  try:
    del(nam[nn]['CINTERPOL_SIC'])
  except KeyError:
    pass

  # NAM_ISBA
  nn = 'NAM_ISBA'
  for tt in ['YCLAY','YCLAYFILETYPE','YSAND','YSANDFILETYPE','YCTI','YCTIFILETYPE','YSOC_TOP','YSOC_SUB','YSOCFILETYPE','YPERM','YPERMFILETYPE']:
    try:
      del(nam[nn][tt])
    except KeyError:
      pass


  # -----------------------------------------------------------
  # Case specific modifications in namref
  # -----------------------------------------------------------

  fin = cdms2.open(filecase)

  surfaceForcing = fin.surfaceForcing
  surfaceType = fin.surfaceType
  zorog = float(fin.zorog)
  startDate = str(fin.startDate)
  endDate = str(fin.endDate)

  if surfaceForcing == 'ts':
    sst = fin('ts')
    lat = sst.getLatitude()[0]
    lon = sst.getLongitude()[0]
    time = sst.getTime()
    zz0 = float(fin.z0)
  elif surfaceForcing == 'surfaceFlux':
    surfaceForcingWind = fin.surfaceForcingWind
    hfls = fin('sfc_lat_flx')
    hfss = fin('sfc_sens_flx')
    if surfaceForcingWind == 'ustar':
        ustar = fin('ustar')
    elif surfaceForcingWind == 'z0':
        zz0 = float(fin.z0)
    else:
      print 'surfaceForcingWind unexpected:', surfaceForcingWind
      sys.exit()
#    ts = fin('temp')[0,0]
    try:
      ts = fin('ts')
    except cdms2.error.CDMSError:
      ts = hfls*0. + fin('temp')[0,0]
    lat = hfls.getLatitude()[0]
    lon = hfls.getLongitude()[0]
    time = hfls.getAxis(0)
  
  fin.close()

  # Setting latitude and longitude
  nam['NAM_CARTESIAN']['XLAT0'] = [str(float(lat)),]
  nam['NAM_CARTESIAN']['XLON0'] = [str(float(lon)),]

  # Setting surface properties
  nn='NAM_PGD_SCHEMES'
  if surfaceType == 'ocean':
    if surfaceForcing == 'ts':
      nam[nn]['CSEA'] = ["'SEAFLX'",]
      nam2keep.append('NAM_DATA_SEAFLUX')
      nam2keep.append('NAM_PREP_SEAFLUX')
      nam2keep.append('NAM_SEAFLUXn')
    elif surfaceForcing == 'surfaceFlux':
      nam[nn]['CSEA'] = ["'FLUX'",]
      nam2keep.append('NAM_IDEAL_FLUX')
    else:
      print 'surfaceForcing unexpected:', surfaceForcing, 'for surfaceType:', surfaceType
      sys.exit()
  elif surfaceType == 'land':
    if surfaceForcing == 'surfaceFlux':
      print 'This configuration does not work:'
      print 'surfaceType =', surfaceType, 'and surfaceForcing =', surfaceForcing
      print 'surfaceType is changed to ocean'
#      sys.exit()
#      nam[nn]['CNATURE'] = ["'FLUX'",]
      nam[nn]['CSEA'] = ["'FLUX'",]
      nam2keep.append('NAM_IDEAL_FLUX')
    elif surfaceForcing == 'ts':
      nam[nn]['CNATURE'] = ["'TSZ0'",]
      nam2keep.append('NAM_ISBA')
      nam2keep.append('NAM_PREP_ISBA')
      nam2keep.append('NAM_PREP_ISBA_SNOW')
      nam2keep.append('NAM_ISBAn')
      nam2keep.append('NAM_DEEPSOIL')
      nam2keep.append('NAM_DATA_ISBA')
      nam2keep.append('NAM_DATA_TSZ0')
    else:
      print 'surfaceForcing unexpected:', surfaceForcing, 'for surfaceType:', surfaceType
      sys.exit()
  else:
    print 'surfaceType unexpected:', surfaceType
    sys.exit()

  nn='NAM_ZS'
  nam[nn]['XUNIF_ZS'] = [str(zorog),]


  # Setting starting date
  startDate = str(startDate)
  year = int(startDate[0:4])
  month = int(startDate[4:6])
  day = int(startDate[6:8])
  hour = int(startDate[8:10])
  minute = int(startDate[10:12])
  second = int(startDate[12:14])
#  second = 0
  t0 = cdtime.comptime(year,month,day,hour,minute)

  tunits0 = 'seconds since %(year)4.4i-%(month)2.2i-%(day)2.2i %(hour)2.2i:%(minute)2.2i:%(second)2.2i'%{"year":year, "month": month, "day": day, "hour": hour, "minute": minute, "second": second}

  seconds = second + minute*60 + hour*3600

  nn = 'NAM_PREP_SURF_ATM'
  nam[nn] = {}
  nam[nn]['NYEAR'] = [str(int(year)),]
  nam[nn]['NMONTH'] = [str(int(month)),]
  nam[nn]['NDAY'] = [str(int(day)),]
  nam[nn]['XTIME'] = [str(int(seconds)),]

  if surfaceType == 'ocean':
    nn='NAM_FRAC'
    nam[nn]['XUNIF_SEA'] = ['1.',]
    nn = 'NAM_PREP_SEAFLUX'
    nam[nn] = {}
    nam[nn]['NYEAR'] = [str(int(year)),]
    nam[nn]['NMONTH'] = [str(int(month)),]
    nam[nn]['NDAY'] = [str(int(day)),]
    nam[nn]['XTIME'] = [str(int(seconds)),]
    if surfaceForcing == 'ts':
      nam[nn]['XSST_UNIF'] = ['%(sst)6.2f'%{"sst":sst[0]},]
    else:
      nam[nn]['XSST_UNIF'] = ['300.',]
  elif surfaceType == 'land':
    if surfaceForcing == 'surfaceFlux':
      nn='NAM_FRAC'
      nam[nn]['XUNIF_SEA'] = ['1.',]
      nn = 'NAM_PREP_SEAFLUX'
      nam[nn] = {}
      nam[nn]['NYEAR'] = [str(int(year)),]
      nam[nn]['NMONTH'] = [str(int(month)),]
      nam[nn]['NDAY'] = [str(int(day)),]
      nam[nn]['XTIME'] = [str(int(seconds)),]
      nam[nn]['XSST_UNIF'] = ['300.',]
    else:
      nn='NAM_FRAC'
      nam[nn]['XUNIF_NATURE'] = ['1.',]
      nam[nn]['LECOCLIMAP'] = ['.TRUE.',]
      nn='NAM_COVER'
      del(nam[nn]['XUNIF_COVER(1)'])
      nam[nn]['XUNIF_COVER(6)'] = ['1.',]
      nn='NAM_ISBA'
      nam[nn]['XUNIF_CLAY'] = ['1.',]
      nam[nn]['XUNIF_SAND'] = ['0.',]
      nam[nn]['XUNIF_RUNOFFB'] = ['0.5',]
##    nam[nn]['CISBA'] = ["'DIF'",]
#    nam[nn]['CISBA'] = ["'2-L'",]
##    nam[nn]['CPEDO_FUNCTION'] = ["'CH78'",]
#    nam[nn]['CPHOTO'] = ["'NON'",]
#    nam[nn]['NPATCH'] = ['1',]
##    nam[nn]['NGROUND_LAYER'] = ['14',]
#    nam[nn]['NGROUND_LAYER'] = ['2',]
      nn = 'NAM_PREP_ISBA'
      nam[nn] = {}
      nam[nn]['NYEAR'] = [str(int(year)),]
      nam[nn]['NMONTH'] = [str(int(month)),]
      nam[nn]['NDAY'] = [str(int(day)),]
      nam[nn]['XTIME'] = [str(int(seconds)),]
      if surfaceForcing == 'ts':
        nam[nn]['XHUG_SURF'] = ['0.',]
        nam[nn]['XHUG_ROOT'] = ['0.',]
        nam[nn]['XHUG_DEEP'] = ['0.',]
        nam[nn]['XTG_SURF'] = ['%(ts)6.2f'%{"ts": sst[0]},]
        nam[nn]['XTG_ROOT'] = ['%(ts)6.2f'%{"ts": sst[0]-0.7},]
        nam[nn]['XTG_DEEP'] = ['%(ts)6.2f'%{"ts": sst[0]-0.7},]    
        nam[nn]['XHUGI_SURF'] = ['0.',]
        nam[nn]['XHUGI_ROOT'] = ['0.',]
        nam[nn]['XHUGI_DEEP'] = ['0.',]
        nam[nn]['LISBA_CANOPY']=['.FALSE.',]
#      nn = 'NAM_PREP_ISBA_SNOW'
#      nam[nn] = {}
##      nam[nn]['CSNOW'] = ["'3-L'",]
#      nam[nn]['CSNOW'] = ["'D95'",]
##      nam[nn]['NSNOW_LAYER'] = ['12',]
#      nn = 'NAM_DEEPSOIL'
#      nam[nn] = {}
#      nam[nn]['LPHYSDOMC'] = ['.FALSE.',]
#      nam[nn]['LDEEPSOIL'] = ['.FALSE.',]
#      nn = 'NAM_ISBA_SNOWn'
#      nam[nn] = {}
#      nam[nn]['LSNOWDRIFT'] = ['.TRUE.',]
#      nn = 'NAM_ISBAn'
#      nam[nn] = {}
##      nam[nn]['CC1DRY'] = ["'DEF'",]
##      nam[nn]['CSCOND'] = ["'PL98'",]
##      nam[nn]['CSOILFRZ'] = ["'LWT'",]
##      nam[nn]['CDIFSFCOND'] = ["'DEF'",]
##      nam[nn]['CSNOWRES'] = ["'RIL'",]
##      nam[nn]['CALBEDO'] = ["'CM13'",]
##      nam[nn]['CROUGH'] = ["'NONE'",]
##      nam[nn]['CCPSURF'] = ["'DRY'",]
#      nam[nn]['LGLACIER'] = ['.TRUE.',]
        nn='NAM_DATA_ISBA'
        nam[nn] = {}
        nam[nn]['NTIME'] = ['1',]
        for i in range(1,12+1):      
          nam[nn]['XUNIF_Z0(1,{0:>2})'.format(i)] = [str(zz0),]
     

  endDate = str(endDate)
  year0 = int(endDate[0:4])
  month0 = int(endDate[4:6])
  day0 = int(endDate[6:8])
  hour0 = int(endDate[8:10])
  minute0 = int(endDate[10:12])
  second0 = int(endDate[12:14])  
#  second0 = 0
  tend0 = cdtime.comptime(year0,month0,day0,hour0,minute0,second0)

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


  if surfaceForcing == 'ts':
    if surfaceType == 'ocean':
      nn = 'NAM_DATA_SEAFLUX'
      nam[nn] = {}
      nam[nn]['LSST_DATA'] = ['T',]
      nam[nn]['NTIME_SST'] = [str(nt0),]

      for it in range(0,nt0):
        nam[nn]['XUNIF_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(sst)6.2f'%{"sst": sst[it]},]

      for it in range(0,nt0):
        nam[nn]['CFTYP_SST(%(ii)4.i)'%{"ii": it+1}] = ["'DIRECT'",]

      for it in range(0,nt0):
        tt = cdtime.reltime(time[it+ii1],time.units)	
        nam[nn]['NYEAR_SST(%(ii)4.i)'%{"ii": it+1}] =['%(year)4.4i'%{"year": tt.tocomp().year},]
        nam[nn]['NMONTH_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(month)2.2i'%{"month": tt.tocomp().month},]
        nam[nn]['NDAY_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(day)2.2i'%{"day": tt.tocomp().day},]
        nam[nn]['XTIME_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(seconds)7.1f'%{"seconds": tt.tocomp().hour*3600},]
    elif surfaceType == 'land':
      nn = 'NAM_DATA_TSZ0'
      nam[nn] = {}
      nam[nn]['NTIME'] = [str(nt0-1),]

      for it in range(0,nt0-1):
        nam[nn]['XUNIF_DTS(%(ii)4.i)'%{"ii": it+1}] = ['%(sst)6.2f'%{"sst": sst[it+1]-sst[it]},]
        nam[nn]['XUNIF_DHUGRD(%(ii)4.i)'%{"ii": it+1}] = ['0.',]


  if surfaceForcing == 'surfaceFlux':
    nn ='NAM_IDEAL_FLUX'
    nam[nn] = {}
    nam[nn]['NFORCF'] = [str(nt0),]
#    nam[nn]['NFORCT'] = ['2',]
    nam[nn]['NFORCT'] = [str(nt0),]
    nam[nn]['CSFTQ'] = ["'W/m2'",]
    nam[nn]['XSFCO2'] = ['0.',]
    if surfaceForcingWind == 'ustar':
      nam[nn]['CUSTARTYPE'] = ["'USTAR'",]
    elif surfaceForcingWind == 'z0':
      nam[nn]['CUSTARTYPE'] = ["'Z0'",]
      nam[nn]['XZ0'] = [str(zz0),]
    nam[nn]['XALB'] = ['0.07',]
    nam[nn]['XEMIS']  = ['1.',]
#    nam[nn]['XTIMET(1)'] = ['0.',]
#    tt = cdtime.reltime(time[ii2],time.units)
#    nam[nn]['XTIMET(2)'] = ['%(tt)6.2f'%{"tt":tt.torel(tunits0).value},]
#    nam[nn]['XTSRAD(1)'] = [str(ts),]
#    nam[nn]['XTSRAD(2)'] = [str(ts),]
    for it in range(0,nt0):
      tt = cdtime.reltime(time[it+ii1],time.units)	  
      nam[nn]['XTIMET(%(ii)4.i)'%{"ii": it+1}] = ['%(tt)6.2f'%{"tt":tt.torel(tunits0).value},]
    for it in range(0,nt0):
      tt = cdtime.reltime(time[it+ii1],time.units)	  
      nam[nn]['XTSRAD(%(ii)4.i)'%{"ii": it+1}] = ['%(ts)6.6f'%{"ts":ts[it+ii1,0,0]},]
    for it in range(0,nt0):
      tt = cdtime.reltime(time[it+ii1],time.units)	  
      nam[nn]['XTIMEF(%(ii)4.i)'%{"ii": it+1}] = ['%(tt)6.6f'%{"tt":tt.torel(tunits0).value},]
    for it in range(0,nt0):
      tt = cdtime.reltime(time[it+ii1],time.units)	  
      nam[nn]['XSFTH(%(ii)4.i)'%{"ii": it+1}] = ['%(hfss)6.6f'%{"hfss":hfss[it+ii1,0,0]},]
    for it in range(0,nt0):
      tt = cdtime.reltime(time[it+ii1],time.units)	  
      nam[nn]['XSFTQ(%(ii)4.i)'%{"ii": it+1}] = ['%(hfls)6.6f'%{"hfls":hfls[it+ii1,0,0]},]
    if surfaceForcingWind == 'ustar':
      for it in range(0,nt0):
        tt = cdtime.reltime(time[it+ii1],time.units)	  
        nam[nn]['XUSTAR(%(ii)4.i)'%{"ii": it+1}] = ['%(ustar)6.6f'%{"ustar":ustar[it+ii1,0,0]},]


  if lperf:
    TT1 = TT.time()
    print 'Update namelist:', TT1-TT0
    TT0 = TT.time()
  # -----------------------------------------------------------
  # Final writing
  # -----------------------------------------------------------


  namnew = {}
  for namin in nam.keys():
    if namin in nam2keep:
      namnew[namin] = nam[namin]        
  namelist.writesurfex(namnew,namout)

  if lperf:
    TT1 = TT.time()
    print 'Writing namelist:', TT1-TT0
    TT0 = TT.time()

  print '-'*40

  if lperf:
    TT1 = TT.time()
    print 'Total:', TT1-TT00

