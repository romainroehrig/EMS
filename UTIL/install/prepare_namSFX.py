#!/usr/bin/env python
# -*- coding:UTF-8 -*-

lperf = False
if lperf:
    import time as TT 
    TT00 = TT.time()

import os
repEMS = os.getenv('REP_EMS')

import sys
sys.path.append('{0}/DEPHY-SCM/utils/'.format(repEMS))

import netCDF4 as nc

from Case import Case

import namelist

lverbose = False

if lperf:
    TT1 = TT.time()
    print 'FIRST THINGS:', TT1-TT00
    TT00 = TT.time()

def prep_nam_SFX(case,subcase,filecase,namref,namout=None):
    """
    Prepare SURFEX namelist for MUSC simulation, 
    given information in filecase,
    and from SURFEX namelist namref
    """

    if lperf:
        TT0 = TT.time()

    if namout is None:
        namout = '{0}_{1}_{2}'.format(namref,case,subcase)

    print '-'*40
    print 'Prepare SURFEX namelist for MUSC'
    print 'case:', case, 'subcase:', subcase
    print 'Reference namelist:', namref
    print 'Output namelist:', namout

    nam2keep = ['NAM_CARTESIAN','NAM_COVER','NAM_DIAG_SURFn','NAM_FRAC','NAM_IO_OFFLINE','NAM_IO_SURF_ARO','NAM_PGD_GRID','NAM_PGD_SCHEMES','NAM_PREP_SURF_ATM','NAM_SURF_ATM','NAM_SURF_CSTS','NAM_ZS']

    if lperf:
        TT1 = TT.time()
        print 'First Things:', TT1-TT0
        TT0 = TT.time()

    nam = namelist.readsurfex(namref)

    if lperf:
        TT1 = TT.time()
        print 'Read namelist:', TT1-TT0
        TT0 = TT.time()

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

    # NAM_IO_SURF_ARO
    nn = 'NAM_IO_SURF_ARO'
    nam[nn] = {}

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

    case = Case('{0}/{1}'.format(case,subcase))
    case.read('data_input.nc')

    if lverbose:
         case.info()

    attributes = case.attributes

    lat = float(case.lat)
    lon = float(case.lon)

    surfaceForcing = attributes['surfaceForcing']
    surfaceType = attributes['surfaceType']
    zorog = attributes['zorog']
    startDate = case.startDate
    year = int(startDate[0:4])
    month = int(startDate[4:6])
    day = int(startDate[6:8])
    hour = int(startDate[8:10])
    minute = int(startDate[10:12])
    second = int(startDate[12:14])    
    seconds = hour*3600.+minute*60.+second

    if surfaceForcing == 'ts':
        ts = case.variables['ts'].data
        time = case.variables['ts'].time
        if surfaceType == 'land':
            zz0 = attributes['z0']
        try:
            alb = attributes['alb']
            lalb = True
        except:
            lalb = False
        try:
            lrce = attributes['RCE']
        except:
            lrce = False
        try: # Used only in case lrce
            lminSfcWind = attributes['minSurfaceWind'] >= 0.
            minSfcWind = attributes['minSurfaceWind']
        except:
            lminSfcWind = False
            minSfcWind = 1. 
    elif surfaceForcing == 'surfaceFlux':
        surfaceForcingWind = attributes['surfaceForcingWind']
        hfls = case.variables['sfc_lat_flx'].data
        hfss = case.variables['sfc_sens_flx'].data
        if surfaceForcingWind == 'ustar':
            ustar = case.variables['ustar']
        elif surfaceForcingWind == 'z0':
            zz0 = attributes['z0']
        else:
            print 'surfaceForcingWind unexpected:', surfaceForcingWind
            sys.exit()
        try:
            ts = case.variables['ts'].data
        except KeyError:
            ts = hfls*0. + case.variables['temp'].data[0,0,0,0]
    
        time = case.variables['sfc_lat_flx'].time

    nt, = time.data.shape
    dates = nc.num2date(time.data,time.units,calendar='gregorian',only_use_python_datetimes=True)

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
#           sys.exit()
#           nam[nn]['CNATURE'] = ["'FLUX'",]
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
            nam[nn]['XSST_UNIF'] = ['%(ts)6.2f'%{"ts":ts[0]},]
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
            if case == 'GABLS4':
                nam[nn]['XUNIF_COVER(6)'] = ['1.',] # Permanent snow and ice 
            else:
                nam[nn]['XUNIF_COVER(4)'] = ['1.',] # Bare land
            nn='NAM_ISBA'
            nam[nn]['XUNIF_CLAY'] = ['1.',]
            nam[nn]['XUNIF_SAND'] = ['0.',]
            nam[nn]['XUNIF_RUNOFFB'] = ['0.5',]
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
                nam[nn]['XTG_SURF'] = ['%(ts)6.2f'%{"ts": ts[0]},]
                nam[nn]['XTG_ROOT'] = ['%(ts)6.2f'%{"ts": ts[0]-0.7},]
                nam[nn]['XTG_DEEP'] = ['%(ts)6.2f'%{"ts": ts[0]-0.7},]    
                nam[nn]['XHUGI_SURF'] = ['0.',]
                nam[nn]['XHUGI_ROOT'] = ['0.',]
                nam[nn]['XHUGI_DEEP'] = ['0.',]
                nam[nn]['LISBA_CANOPY']=['.FALSE.',]
                nn='NAM_DATA_ISBA'
                nam[nn] = {}
                nam[nn]['NTIME'] = ['1',]
                for i in range(1,12+1):      
                    nam[nn]['XUNIF_Z0(1,{0:>2})'.format(i)] = [str(zz0),]
                nn='NAM_ISBA'
                nam[nn] = {}
                nam[nn]['CISBA'] = ["'2-L'",]
                nam[nn]['CPHOTO'] = ["'NON'",]
                nam[nn]['NGROUND_LAYER'] = ['2',]
                nam[nn]['NPATCH'] = ['1',]
                nam[nn]['XUNIF_CLAY'] = ['1.',]
                nam[nn]['XUNIF_RUNOFFB'] = ['0.5',]
                nam[nn]['XUNIF_SAND'] = ['0.',]
                #nn = 'NAM_ISBAn'
                #nam[nn] = {}
                #nam[nn]['LGLACIER'] = ['.TRUE.',]
                nn = 'NAM_PREP_ISBA_SNOW'
                nam[nn] = {}
                nam[nn]['CSNOW'] = ["'D95'",]
     

    if surfaceForcing == 'ts':
        if surfaceType == 'ocean':
            nn = 'NAM_DATA_SEAFLUX'
            nam[nn] = {}
            nam[nn]['LSST_DATA'] = ['T',]
            nam[nn]['NTIME_SST'] = [str(nt),]

            for it in range(0,nt):
                nam[nn]['XUNIF_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(ts)6.2f'%{"ts": ts[it]},]

            for it in range(0,nt):
                nam[nn]['CFTYP_SST(%(ii)4.i)'%{"ii": it+1}] = ["'DIRECT'",]

            for it in range(0,nt):
                nam[nn]['NYEAR_SST(%(ii)4.i)'%{"ii": it+1}] =['%(year)4.4i'%{"year": dates[it].year},]
                nam[nn]['NMONTH_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(month)2.2i'%{"month": dates[it].month},]
                nam[nn]['NDAY_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(day)2.2i'%{"day": dates[it].day},]
                nam[nn]['XTIME_SST(%(ii)4.i)'%{"ii": it+1}] = ['%(seconds)7.1f'%{"seconds": dates[it].hour*3600+dates[it].minute*60+dates[it].second},]

            if lalb:
                nam['NAM_SEAFLUXn']['CSEA_ALB'] = ["'UNIF'",]
                nn = 'NAM_SURF_CSTS'
                nam[nn]['XALBCOEF_TA96'] = [str(alb),]
                nam[nn]['XALBSCA_WAT'] = [str(alb),]
                nam[nn]['XALBWAT'] = [str(alb),]

            if lrce:
                nn = 'NAM_SURF_ATM'
                nam[nn]['LALDTHRES'] = ['.TRUE.',]
                nam[nn]['XCISMIN'] = ['0.',]
                if lminSfcWind:
                    nam[nn]['XVMODMIN'] = [str(minSfcWind),]
        
        elif surfaceType == 'land':
            nn = 'NAM_DATA_TSZ0'
            nam[nn] = {}
            nam[nn]['NTIME'] = [str(nt),]

            for it in range(0,nt-1):
                nam[nn]['XUNIF_DTS(%(ii)4.i)'%{"ii": it+1}] = ['%(dts)6.3f'%{"dts": ts[it+1]-ts[it]},]
                nam[nn]['XUNIF_DHUGRD(%(ii)4.i)'%{"ii": it+1}] = ['0.',]
            nam[nn]['XUNIF_DTS(%(ii)4.i)'%{"ii": nt}] = ['%(dts)6.3f'%{"dts": ts[nt-1]-ts[nt-2]},]
            nam[nn]['XUNIF_DHUGRD(%(ii)4.i)'%{"ii": nt}] = ['0.',]


    if surfaceForcing == 'surfaceFlux':
        nn ='NAM_IDEAL_FLUX'
        nam[nn] = {}
        nam[nn]['NFORCF'] = [str(nt),]
        nam[nn]['NFORCT'] = [str(nt),]
        nam[nn]['CSFTQ'] = ["'W/m2'",]
        nam[nn]['XSFCO2'] = ['0.',]
        if surfaceForcingWind == 'ustar':
            nam[nn]['CUSTARTYPE'] = ["'USTAR'",]
        elif surfaceForcingWind == 'z0':
            nam[nn]['CUSTARTYPE'] = ["'Z0'",]
            nam[nn]['XZ0'] = [str(zz0),]
        nam[nn]['XALB'] = ['0.07',]
        nam[nn]['XEMIS']  = ['1.',]
        for it in range(0,nt):
            nam[nn]['XTIMET(%(ii)4.i)'%{"ii": it+1}] = ['%(tt)6.2f'%{"tt":time.data[it]},]
        for it in range(0,nt):
            nam[nn]['XTSRAD(%(ii)4.i)'%{"ii": it+1}] = ['%(ts)6.6f'%{"ts":ts[it,0,0]},]
        for it in range(0,nt):
            nam[nn]['XTIMEF(%(ii)4.i)'%{"ii": it+1}] = ['%(tt)6.6f'%{"tt":time.data[it]},]
        for it in range(0,nt):
            nam[nn]['XSFTH(%(ii)4.i)'%{"ii": it+1}] = ['%(hfss)6.6f'%{"hfss":hfss[it,0,0]},]
        for it in range(0,nt):
            nam[nn]['XSFTQ(%(ii)4.i)'%{"ii": it+1}] = ['%(hfls)6.6f'%{"hfls":hfls[it,0,0]},]
        if surfaceForcingWind == 'ustar':
            for it in range(0,nt):
                nam[nn]['XUSTAR(%(ii)4.i)'%{"ii": it+1}] = ['%(ustar)6.6f'%{"ustar":ustar[it,0,0]},]


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

