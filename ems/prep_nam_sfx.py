#!/usr/bin/env python
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import os
import logging
logger = logging.getLogger(__name__)

import netCDF4 as nc

from ems.dephycf.Case import Case
from ems.namelist import readsurfex, writesurfex

lverbose = logger.getEffectiveLevel() == logging.DEBUG

def prep_nam_sfx(ncfile, namin, namout='namsurf', sfxfmt='LFI'):
    """
    Prepare SURFEX namelist for MUSC simulation, 
    given information in filecase,
    and from SURFEX namelist namin
    """

    logger.info('-'*40)
    logger.info('Prepare SURFEX namelist for MUSC')
    logger.info('Case file: ' +  ncfile)
    logger.info('Reference namelist: ' + namin)
    logger.info('Output namelist: ' + namout)
    logger.info('Output format for PGD/PREP: ' + sfxfmt)

    nam2keep = ['NAM_CARTESIAN', 'NAM_COVER', 'NAM_DIAG_SURFN', 'NAM_FRAC', 'NAM_IO_OFFLINE',
                'NAM_IO_SURF_ARO', 'NAM_PGD_GRID', 'NAM_PGD_SCHEMES', 'NAM_PREP_SURF_ATM',
                'NAM_SURF_ATM', 'NAM_SURF_CSTS', 'NAM_ZS']

    nam = readsurfex(namin)

    # Remove a few namelists
    for nn in ['NAMDIM', 'NAMGEM', 'NAMRGRI', 'NAMVV1', 'NAMRGRI', 'NAM_IO_SURF_ARO',
               'NAM_OASIS', 'NAM_SFX_LAND_CPL', 'NAM_DIAG_SURF_ATMN']:
        try:      
            del(nam[nn])
        except KeyError:
            pass
        nam[nn] = {}

    # Grid
    nam['NAM_PGD_GRID'] = {}
    nam['NAM_PGD_GRID']['CGRID'] = ["'CARTESIAN'"]
    nam['NAM_CARTESIAN'] = {}
    nam['NAM_CARTESIAN']['NIMAX'] = ['1']
    nam['NAM_CARTESIAN']['NJMAX'] = ['4']
    nam['NAM_CARTESIAN']['XDX'] = ['250000.']
    nam['NAM_CARTESIAN']['XDY'] = ['250000.']

    # Surface type
    nn='NAM_PGD_SCHEMES'
    if not nn in nam:
        nam[nn] = {}
    for tt in ['CNATURE', 'CSEA', 'CTOWN', 'CWATER']:
        nam[nn][tt] = ["'NONE'"]
    nn='NAM_FRAC'
    if nn not in nam:
        nam[nn] = {}
    nam[nn]['LECOCLIMAP'] = ['F']
    for tt in ['NATURE', 'SEA', 'TOWN', 'WATER']:
        nam[nn]['XUNIF_' + tt] = ['0.']
    nn = 'NAM_COVER'
    nam[nn] = {}
    nam[nn]['XUNIF_COVER(1)'] = ['1.']
    nn = 'NAM_ZS'
    nam[nn] = {}
    nam[nn]['XUNIF_ZS'] = ['0.']

    # NAM_IO_OFFLINE
    nn = 'NAM_IO_OFFLINE'
    if not nn in nam:
        nam[nn] = {}
    if sfxfmt == 'FA':
        nam[nn]['LFAGMAP'] = ['T']
        nam[nn]['CSURF_FILETYPE'] = ["'FA   '"]
    elif sfxfmt == 'LFI':
        nam[nn]['CSURF_FILETYPE'] = ["'LFI'"]
    else:
        raise ValueError('Unexpected value for Surfex PGD/PREP format: ' + sfxfmt)
    nam[nn]['CPGDFILE'] = ["'PGD'"]
    nam[nn]['CPREPFILE'] = ["'PREP'"]

    # NAM_IO_SURF_ARO
    nn = 'NAM_IO_SURF_ARO'
    nam[nn] = {}

    # NAM_DIAG_SURFn
    nn = 'NAM_DIAG_SURFN'
    nam[nn] = {}
    nam[nn]['N2M'] = ['2']
    for tt in ['LSURF_VARS', 'LSURF_BUDGET', 'LCOEF', 'LRAD_BUDGET', 'LSURF_BUDGETC', 'LRESET_BUDGETC']:
        nam[nn][tt] = ['T']     

    # NAM_SEAFLUXn
    nn = 'NAM_SEAFLUXN'
    try:
        del(nam[nn]['CINTERPOL_SST'])
    except KeyError:
        pass

    # NAM_SEAICEn
    nn = 'NAM_SEAICEN'
    try:
        del(nam[nn]['CINTERPOL_SIC'])
    except KeyError:
        pass

    # NAM_ISBA
    nn = 'NAM_ISBA'
    for tt in ['YCLAY', 'YCLAYFILETYPE', 'YSAND', 'YSANDFILETYPE',
               'YCTI', 'YCTIFILETYPE',
               'YSOC_TOP', 'YSOC_SUB', 'YSOCFILETYPE',
               'YPERM', 'YPERMFILETYPE']:
        try:
            del(nam[nn][tt])
        except KeyError:
            pass


    # -----------------------------------------------------------
    # Case specific modifications in namin
    # -----------------------------------------------------------

    case = Case('tmp')
    case.read(ncfile)

    if lverbose:
         case.info()

    attributes = case.attributes

    lat = case.variables['lat'].data[0]
    lon = case.variables['lon'].data[0]

    surfaceForcing = attributes['surface_forcing_temp']
    surfaceType = attributes['surface_type']
    zorog = case.variables['orog'].data[0]
    startDate = case.start_date
    year = startDate.year
    month = startDate.month
    day = startDate.day
    hour = startDate.hour
    minute = startDate.minute
    second = startDate.second
    seconds = hour*3600.+minute*60.+second

    if surfaceForcing == 'ts':
        tsinit = case.variables['ts'].data
        tsforc = case.variables['ts_forc'].data
        time = case.variables['ts_forc'].time
        if surfaceType in ['land','landice']:
            zz0 = case.variables['z0'].data[0]
        try:
            alb = case.variables['alb'].data[0]
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
    elif surfaceForcing == 'surface_flux':
        surfaceForcingWind = attributes['surface_forcing_wind']
        hfls = case.variables['hfls'].data
        hfss = case.variables['hfss'].data
        if surfaceForcingWind == 'ustar':
            ustar = case.variables['ustar']
        elif surfaceForcingWind == 'z0':
            zz0 = case.variables['z0'].data[0]
        else:
            raise RuntimeError('surfaceForcingWind unexpected: ' + surfaceForcingWind)

        try:
            tsforc = case.variables['ts_forc'].data
        except KeyError:
            tsforc = hfls*0. + case.variables['ta'].data[0,0]
    
        time = case.variables['hfls'].time

    nt, = time.data.shape
    dates = nc.num2date(time.data,time.units,calendar='gregorian')#,only_use_python_datetimes=True)

    # Setting latitude and longitude
    nam['NAM_CARTESIAN']['XLAT0'] = [str(float(lat))]
    nam['NAM_CARTESIAN']['XLON0'] = [str(float(lon))]

    # Setting surface properties
    nn='NAM_PGD_SCHEMES'
    if surfaceType == 'ocean':
        if surfaceForcing == 'ts':
            nam[nn]['CSEA'] = ["'SEAFLX'"]
            nam2keep.append('NAM_DATA_SEAFLUX')
            nam2keep.append('NAM_PREP_SEAFLUX')
            nam2keep.append('NAM_SEAFLUXN')
        elif surfaceForcing == 'surface_flux':
            nam[nn]['CSEA'] = ["'FLUX'"]
            nam2keep.append('NAM_IDEAL_FLUX')
        else:
            raise RuntimeError('surfaceForcing unexpected: ' + surfaceForcing + ' for surfaceType: ' + surfaceType)
    elif surfaceType in ['land','landice']:
        if surfaceForcing == 'surface_flux':
            logger.warning('This configuration does not work:')
            logger.warning('surfaceType = ' + surfaceType + ' and surfaceForcing = ' + surfaceForcing)
            logger.warning('=> surfaceType is changed to ocean')
#           nam[nn]['CNATURE'] = ["'FLUX'"]
            nam[nn]['CSEA'] = ["'FLUX'"]
            nam2keep.append('NAM_IDEAL_FLUX')
        elif surfaceForcing == 'ts':
            nam[nn]['CNATURE'] = ["'TSZ0'"]
            nam2keep.append('NAM_ISBA')
            nam2keep.append('NAM_PREP_ISBA')
            nam2keep.append('NAM_PREP_ISBA_SNOW')
            nam2keep.append('NAM_ISBAN')
            nam2keep.append('NAM_DEEPSOIL')
            nam2keep.append('NAM_DATA_ISBA')
            nam2keep.append('NAM_DATA_TSZ0')
        else:
            raise RuntimeError('surfaceForcing unexpected: ' + surfaceForcing + ' for surfaceType: ' + surfaceType)
    else:
        raise RuntimeError('surfaceType unexpected: ' + surfaceType)

    nn='NAM_ZS'
    nam[nn]['XUNIF_ZS'] = [str(zorog)]


    nn = 'NAM_PREP_SURF_ATM'
    nam[nn] = {}
    nam[nn]['NYEAR'] = [str(int(year))]
    nam[nn]['NMONTH'] = [str(int(month))]
    nam[nn]['NDAY'] = [str(int(day))]
    nam[nn]['XTIME'] = [str(int(seconds))]

    if surfaceType == 'ocean':
        nn='NAM_FRAC'
        nam[nn]['XUNIF_SEA'] = ['1.']
        nn = 'NAM_PREP_SEAFLUX'
        nam[nn] = {}
        nam[nn]['NYEAR'] = [str(int(year))]
        nam[nn]['NMONTH'] = [str(int(month))]
        nam[nn]['NDAY'] = [str(int(day))]
        nam[nn]['XTIME'] = [str(int(seconds))]
        if surfaceForcing == 'ts':
            nam[nn]['XSST_UNIF'] = ['%(ts)6.2f'%{"ts":tsforc[0]}]
        else:
            nam[nn]['XSST_UNIF'] = ['300.']
    elif surfaceType in ['land','landice']:
        if surfaceForcing == 'surface_flux':
            nn='NAM_FRAC'
            nam[nn]['XUNIF_SEA'] = ['1.']
            nn = 'NAM_PREP_SEAFLUX'
            nam[nn] = {}
            nam[nn]['NYEAR'] = [str(int(year))]
            nam[nn]['NMONTH'] = [str(int(month))]
            nam[nn]['NDAY'] = [str(int(day))]
            nam[nn]['XTIME'] = [str(int(seconds))]
            nam[nn]['XSST_UNIF'] = ['300.']
        else:
            nn='NAM_FRAC'
            nam[nn]['XUNIF_NATURE'] = ['1.']
            nam[nn]['LECOCLIMAP'] = ['.TRUE.']
            nn='NAM_COVER'
            del(nam[nn]['XUNIF_COVER(1)'])
            if surfaceType == 'landice':
                nam[nn]['XUNIF_COVER(6)'] = ['1.'] # Permanent snow and ice 
            elif surfaceType == 'land':
                nam[nn]['XUNIF_COVER(4)'] = ['1.'] # Bare land
            else:
                raise ValueError("For this case (surfaceForcing != 'surfaceFlux' on land), the land_type must be defined")
            nn='NAM_ISBA'
            nam[nn]['XUNIF_CLAY'] = ['1.']
            nam[nn]['XUNIF_SAND'] = ['0.']
            nam[nn]['XUNIF_RUNOFFB'] = ['0.5']
            nn = 'NAM_PREP_ISBA'
            nam[nn] = {}
            nam[nn]['NYEAR'] = [str(int(year))]
            nam[nn]['NMONTH'] = [str(int(month))]
            nam[nn]['NDAY'] = [str(int(day))]
            nam[nn]['XTIME'] = [str(int(seconds))]
            if surfaceForcing == 'ts':
                nam[nn]['XHUG_SURF'] = ['0.']
                nam[nn]['XHUG_ROOT'] = ['0.']
                nam[nn]['XHUG_DEEP'] = ['0.']
                nam[nn]['XTG_SURF'] = ['%(ts)6.2f'%{"ts": tsforc[0]}]
                nam[nn]['XTG_ROOT'] = ['%(ts)6.2f'%{"ts": tsforc[0]-0.7}]
                nam[nn]['XTG_DEEP'] = ['%(ts)6.2f'%{"ts": tsforc[0]-0.7}]    
                nam[nn]['XHUGI_SURF'] = ['0.']
                nam[nn]['XHUGI_ROOT'] = ['0.']
                nam[nn]['XHUGI_DEEP'] = ['0.']
                nam[nn]['LISBA_CANOPY']=['.FALSE.']
                nn='NAM_DATA_ISBA'
                nam[nn] = {}
                nam[nn]['NTIME'] = ['1']
                for i in range(1,12+1):      
                    nam[nn]['XUNIF_Z0(1,{0:>2})'.format(i)] = [str(zz0)]
                nn='NAM_ISBA'
                nam[nn] = {}
                nam[nn]['CISBA'] = ["'2-L'"]
                nam[nn]['CPHOTO'] = ["'NON'"]
                nam[nn]['NGROUND_LAYER'] = ['2']
                nam[nn]['NPATCH'] = ['1']
                nam[nn]['XUNIF_CLAY'] = ['1.']
                nam[nn]['XUNIF_RUNOFFB'] = ['0.5']
                nam[nn]['XUNIF_SAND'] = ['0.']
                #nn = 'NAM_ISBAN'
                #nam[nn] = {}
                #nam[nn]['LGLACIER'] = ['.TRUE.']
                nn = 'NAM_PREP_ISBA_SNOW'
                nam[nn] = {}
                nam[nn]['CSNOW'] = ["'D95'"]
     

    if surfaceForcing == 'ts':
        if surfaceType == 'ocean':
            nn = 'NAM_DATA_SEAFLUX'
            nam[nn] = {}
            nam[nn]['LSST_DATA'] = ['T']
            nam[nn]['NTIME_SST'] = [str(nt)]

            for it in range(nt):
                nam[nn]['XUNIF_SST(%(ii)4.i)'%{"ii": it + 1}] = ['%(ts)6.2f'%{"ts": tsforc[it]}]

            for it in range(nt):
                nam[nn]['CFTYP_SST(%(ii)4.i)'%{"ii": it + 1}] = ["'DIRECT'"]

            for it in range(nt):
                nam[nn]['NYEAR_SST(%(ii)4.i)'%{"ii": it + 1}] =['%(year)4.4i'%{"year": dates[it].year}]
                nam[nn]['NMONTH_SST(%(ii)4.i)'%{"ii": it + 1}] = ['%(month)2.2i'%{"month": dates[it].month}]
                nam[nn]['NDAY_SST(%(ii)4.i)'%{"ii": it + 1}] = ['%(day)2.2i'%{"day": dates[it].day}]
                nam[nn]['XTIME_SST(%(ii)4.i)'%{"ii": it + 1}] = ['%(seconds)7.1f'%{"seconds": dates[it].hour * 3600 + dates[it].minute * 60 + dates[it].second}]

            if lalb:
                nam['NAM_SEAFLUXN']['CSEA_ALB'] = ["'UNIF'"]
                nn = 'NAM_SURF_CSTS'
                nam[nn]['XALBCOEF_TA96'] = [str(alb)]
                nam[nn]['XALBSCA_WAT'] = [str(alb)]
                nam[nn]['XALBWAT'] = [str(alb)]

            if lrce:
                nn = 'NAM_SURF_ATM'
                nam[nn]['LALDTHRES'] = ['.TRUE.']
                nam[nn]['XCISMIN'] = ['0.']
                if lminSfcWind:
                    nam[nn]['XVMODMIN'] = [str(minSfcWind)]
        
        elif surfaceType in ['land','landice']:
            nn = 'NAM_DATA_TSZ0'
            nam[nn] = {}
            nam[nn]['NTIME'] = [str(nt)]

            for it in range(nt-1):
                nam[nn]['XUNIF_DTS(%(ii)4.i)'%{"ii": it + 1}] = ['%(dts)6.3f'%{"dts": tsforc[it + 1] - tsforc[it]}]
                nam[nn]['XUNIF_DHUGRD(%(ii)4.i)'%{"ii": it + 1}] = ['0.']
            nam[nn]['XUNIF_DTS(%(ii)4.i)'%{"ii": nt}] = ['%(dts)6.3f'%{"dts": tsforc[nt - 1] - tsforc[nt - 2]}]
            nam[nn]['XUNIF_DHUGRD(%(ii)4.i)'%{"ii": nt}] = ['0.']


    if surfaceForcing == 'surface_flux':
        nn ='NAM_IDEAL_FLUX'
        nam[nn] = {}
        nam[nn]['NFORCF'] = [str(nt)]
        nam[nn]['NFORCT'] = [str(nt)]
        nam[nn]['CSFTQ'] = ["'W/m2'"]
        nam[nn]['XSFCO2'] = ['0.']
        if surfaceForcingWind == 'ustar':
            nam[nn]['CUSTARTYPE'] = ["'USTAR'"]
        elif surfaceForcingWind == 'z0':
            nam[nn]['CUSTARTYPE'] = ["'Z0'"]
            nam[nn]['XZ0'] = [str(zz0)]
        nam[nn]['XALB'] = ['0.07']
        nam[nn]['XEMIS']  = ['1.']
        for it in range(nt):
            nam[nn]['XTIMET(%(ii)4.i)'%{"ii": it + 1}] = ['%(tt)6.2f'%{"tt": time.data[it]}]
        for it in range(nt):
            nam[nn]['XTSRAD(%(ii)4.i)'%{"ii": it + 1}] = ['%(ts)6.6f'%{"ts": tsforc[it]}]
        for it in range(nt):
            nam[nn]['XTIMEF(%(ii)4.i)'%{"ii": it + 1}] = ['%(tt)6.6f'%{"tt": time.data[it]}]
        for it in range(nt):
            nam[nn]['XSFTH(%(ii)4.i)'%{"ii": it + 1}] = ['%(hfss)6.6f'%{"hfss": hfss[it]}]
        for it in range(nt):
            nam[nn]['XSFTQ(%(ii)4.i)'%{"ii": it + 1}] = ['%(hfls)6.6f'%{"hfls": hfls[it]}]
        if surfaceForcingWind == 'ustar':
            for it in range(nt):
                nam[nn]['XUSTAR(%(ii)4.i)'%{"ii": it+1}] = ['%(ustar)6.6f'%{"ustar": ustar[it]}]

    # -----------------------------------------------------------
    # Final writing
    # -----------------------------------------------------------

    namnew = {}
    for namin in nam.keys():
        if namin in nam2keep:
            namnew[namin] = nam[namin]

    writesurfex(namnew,namout)

    logger.info('-'*40)
