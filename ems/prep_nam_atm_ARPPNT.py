#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
Namelist for ARPEGE 46t1, atmospheric part
"""

import os
import logging
logger = logging.getLogger(__name__)

import math
import netCDF4 as nc

from ems.dephycf.Case import Case
from ems.namelist import readarp, writearp

lverbose = logger.getEffectiveLevel() == logging.DEBUG

def prep_nam_atm(ncfile, namin, timestep, namout='namarp', lsurfex=False):
    """
    Prepare ARPEGE namelist for MUSC simulation, 
    given information in filecase,
    and from ARPEGE namelist namin
    """

    logger.info('-'*40)
    logger.info('Prepare ARPEGE namelist for MUSC')
    logger.info('Case netCDF file:' + ncfile)
    logger.info('Reference namelist: ' + namin)
    logger.info('Output namelist: ' + namout)

    nam = readarp(namin)

    # -----------------------------------------------------------
    # Some general modification in namin
    # -----------------------------------------------------------

    for namin in nam.keys():
        for param in nam[namin].keys():
            tmp = enumerate(nam[namin][param])
            for i, obj in tmp:  
                if obj == '.T.': nam[namin][param][i] = '.TRUE.'
                if obj == '.F.': nam[namin][param][i] = '.FALSE.'
                if obj == '__MP_TYPE__': nam[namin][param][i] = '2'
                if obj == '__NTASKS__': nam[namin][param][i] = '1'
                if obj == '__MBX_SIZE__': nam[namin][param][i] = '2048000000'
                if obj == '__LOPT_SCALAR__': nam[namin][param][i] = '.TRUE.'
                if obj == '__NCOMBFLEN__': nam[namin][param][i] = '1800000'
    
    #Replace some strings by suitable values
    nam['NAMIO_SERV']['NPROC_IO'] = ['0']
    for k in ('NPRGPEW', 'NPRGPNS', 'NPROC', 'NPRTRV', 'NPRTRW'):
        nam['NAMPAR0'][k] = ['1']
    nam['NAMPAR1']['NSTROUT'] = ['1']
    nam['NAMPAR1']['NSTRIN'] = ['1']
    
    #Deactivate SLHD and COMAD
    for k in ('LSLHD_GFL', 'LSLHD_SPD', 'LSLHD_SVD', 'LSLHD_T', 'LSLHD_W',
              'LCOMADH', 'LCOMADV', 'LCOMAD_GFL', 'LCOMAD_SP', 'LCOMAD_SPD',
              'LCOMAD_SVD', 'LCOMAD_T', 'LCOMAD_W'):
        nam['NAMDYNA'][k] = ['.FALSE.']
    for k in nam['NAMGFL'].keys():
        if k.endswith('%LSLHD') or k.endswith('%LCOMAD'):
            nam['NAMGFL'][k] = ['.FALSE.']

    #Deactivate predictor-corrector
    nam['NAMDYNA']['LPC_CHEAP'] = ['.FALSE.']
    nam['NAMDYNA']['LPC_FULL'] = ['.FALSE.']
    nam['NAMDYN']['NSITER'] = ['0']

    #Deactivate NH dyn
    nam['NAMCT0']['LNHEE'] = ['.FALSE.'] #because required input fields are absent
    
    #Deactivate MPI
    nam['NAMPAR0']['LMPOFF'] = ['.TRUE.']
    
    #Deactivate spectral nudging
    nam['NEMELBC0A']['LESPCPL'] = ['FALSE']
    
    #Deactivate fullpos
    nam['NAMCT0']['NFPOS'] = ['0']
    nam['NAMCT1']['N1POS'] = ['0']
    
    #Remove GRIB API use ???
    nam['NAMCT0']['LGRIB_API'] = ['.FALSE.']
    
    #Insure LELAM
    nam['NAMARG']['LELAM'] = ['.TRUE.']
    
    #Control moisture convergence computation for French deep convection scheme (ARPEGE physics)
    nam['NAMDYN']['NCOMP_CVGQ'] = ['2']

    #grid-point / spectral   
    nam['NAMGFL']['YQ_NL%LSP'] = ['.FALSE.']
    nam['NAMGFL']['YQ_NL%LGP'] = ['.TRUE.']

    #Other mod
    for k in list(nam['NAMPAR1'].keys()):
        if k.replace(' ', '') == 'NDISTIO(12)':
            del nam['NAMPAR1'][k] #gfortran does not like this option (info from P. Marguinaud)
    for k in ('LADVF', 'LIMPF', 'LQMPD', 'LQMT', 'LQMVD'):
        nam['NAMDYN'][k] = ['.FALSE.']

    # Aerosols and Ozone
    #nam['NAMPHY']['LAEROSEA'] = ['.FALSE.']
    #nam['NAMPHY']['LAEROLAN'] = ['.FALSE.']
    #nam['NAMPHY']['LAEROSOO'] = ['.FALSE.']
    #nam['NAMPHY']['LAERODES'] = ['.FALSE.']

    #nam['NAMPHY']['LOZONE'] = ['.FALSE.']
    #nam['NAMPHY']['LO3ABC'] = ['.FALSE.']
    #nam['NAMPHY']['LO3FL'] = ['.FALSE.']

    #nam['NAMPHY']['LRAYFM'] = ['.FALSE.']
    nam['NAMPHY']['LEDR'] = ['.FALSE.',]

    #nam['NAMPHY']['LRELAXT'] = ['.FALSE.']
    #nam['NAMPHY']['LRELAXW'] = ['.FALSE.']

    # Various update

    # Empty a few namelists
    for nn in ['NAMFPC', 'NAMFPD', 'NAMLSFORC', 'NAMDYNA', 'NAMFPSC2_DEP']:
        del(nam[nn])
        nam[nn] = {}

    # Add a few namelists
    for nn in []:
        nam[nn] = {}      

    # Update NAMGFL
    nn = 'NAMGFL'
    nam[nn]['NGFL_FORC'] = ['0']
    nam[nn]['YCVGQ_NL%LCDERS'] = ['.TRUE.']
    nam[nn]['YCVGQ_NL%LSP'] = ['.FALSE.']      
    nam[nn]['YCVGQ_NL%LGP'] = ['.TRUE.']
    nam[nn]['YQ_NL%LSP'] = ['.FALSE.']      
    nam[nn]['YQ_NL%LQM'] = ['.FALSE.']
    nam[nn]['YQ_NL%LGP'] = ['.TRUE.']
    nam[nn]['YQ_NL%NCOUPLING'] = ['0']
    nam[nn]['YQ_NL%NREQIN'] = ['1']
    for var in ['L', 'I', 'R', 'S', 'TKE']:
        nam[nn]['Y' + var + '_NL%LGP'] = ['.TRUE.']
        nam[nn]['Y' + var + '_NL%LGPINGP'] = ['.TRUE.']
        nam[nn]['Y' + var + '_NL%LPHY'] = ['.FALSE.']
        nam[nn]['Y' + var + '_NL%NCOUPLING'] = ['0']
        nam[nn]['Y' + var + '_NL%LADV'] = ['.TRUE.']
        nam[nn]['Y' + var + '_NL%LREQOUT'] = ['.TRUE.']
        nam[nn]['Y' + var + '_NL%LT1'] = ['.TRUE.']
        nam[nn]['Y' + var + '_NL%LQM'] = ['.TRUE.']
        if var in ['L', 'I', 'TKE']:
            nam[nn]['Y' + var + '_NL%NREQIN'] = ['1']
        else:
            nam[nn]['Y' + var + '_NL%NREQIN'] = ['0']

    # Update due to MUSC/ALADIN config
    nn = 'NAMDYN'

    # Update NAMDIM
    nn = 'NAMDIM'

    # Update NAMFA
    if 'NVGRIB' in nam['NAMFA'].keys(): del(nam['NAMFA']['NVGRIB'])

    # Empty NAMDDH
    nam['NAMDDH'] = {}

    # Update NAMCT1
    nn = 'NAMCT1'
    del(nam[nn])
    nam[nn] = {}
    nam[nn]['LRFILAF'] = ['.FALSE.']
    nam[nn]['N1POS'] = ['0']
    nam[nn]['N1RES'] = ['0']
    nam[nn]['N1SFXHIS'] = ['0']

    # Update NAMCT0
    nn = 'NAMCT0'

    # Update NAMXFU
    nn = 'NAMXFU'
    for param in nam[nn].keys():
        if param[0] == 'L': nam[nn][param] = ['.FALSE.']

    # Update NAMARG
    nn = 'NAMARG' 
    nam[nn]['CNMEXP'] = ["'ARPE'"]

    # -----------------------------------------------------------
    # Case specific modifications in namin
    # -----------------------------------------------------------

    case = Case('tmp')
    case.read(ncfile)

    if lverbose:
         case.info()

    attributes = case.attributes

    # Yet undefined in DEPHY format
    attributes['adv_ua'] = 0
    attributes['adv_va'] = 0

    lat = case.variables['lat'].data[0]
    lon = case.variables['lon'].data[0]

    # Setting latitude and longitude
    nam['NEMGEO']['RLAT_ACAD'] = [str(float(lat))]
    nam['NEMGEO']['RLON_ACAD'] = [str(float(lon))]

    # Setting starting date
    startDate = case.start_date
    hour = startDate.hour
    minute = startDate.minute
    second = startDate.second

    # Determine NSTOP
    endDate = case.end_date
    tmp = endDate-startDate
    tmp = tmp.total_seconds()/3600
    NSTOP = 'h' + str(int(tmp))
    logger.debug('NSTOP: ' + NSTOP)

    nam['NAMRIP']['TSTEP'] = [str(timestep)]
    nam['NAMRIP']['CSTOP'] = ["'{0}'".format(NSTOP)]
    #nam['NAMRIP']['NINDAT'] = [startDate.strftime('%Y%m%d')]
    #nam['NAMRIP']['NSSSSS'] = [str(int(hour * 3600 + minute * 60 + second))]


    # Case with no radiation or radiation included in temperature advection
    if attributes['radiation'] in ['off', 'tend']:
        nam['NAMPHY']['LRAYFM'] = ['.FALSE.']
        nam['NAERAD']['LRRTM'] =  ['.FALSE.']
        nam['NAERAD']['LSRTM'] =  ['.FALSE.']
        nam['NAERAD']['NSW'] = ['6']
        nam['NAERAD']['NOZOCL'] = ['2']

    # MUSC Forcing
    nn = 'NAMLSFORC'
    #del(nam[nn])
    if nn not in nam.keys():
        nam[nn] = {}

    i = 0
    j = 0

    timein = case.variables['pa_forc'].time
    nt_f = timein.length
    if nt_f <= 1:
        dt = 0.
    else:  
        dt = timein.data[1]-timein.data[0]

    for param in ['LGEOST_UV_FRC', 'LQV_ADV_FRC', 'LT_ADV_FRC', 'LUV_ADV_FRC',
                  'LSW_FRC', 'LSOMEGA_FRC',
                  'LUV_NUDG', 'LT_NUDG', 'LQV_NUDG']:
        nam[nn][param] = ['.FALSE.']
    nam[nn]['LMUSCLFA'] = ['.TRUE.']

    # Important: the following must in the same order as in prep_init_forc_atm 
    # (consistently with nam1D)

    if attributes['adv_ua'] == 1 or attributes['adv_va'] == 1:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LUV_ADV_FRC'] = ['.TRUE.']
        logger.error('ERROR: Case not yet coded for adv_ua/adv_va = 1')
        raise NotImplementedError('U/V advection not yet validated')

    if attributes['adv_ta'] == 1 or attributes['radiation'] == 'tend':
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LT_ADV_FRC'] = ['.TRUE.']
        nam[nn]['NT_ADV_DEB'] = [str(1 + i * nt_f)]
        nam[nn]['NT_ADV_NUM'] = [str(nt_f)]
        i += 1
        for it in range(nt_f):
            nam[nn]['NL_T_ADV_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

    if attributes['adv_qv'] == 1:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LQV_ADV_FRC'] = ['.TRUE.']
        nam[nn]['NQV_ADV_DEB'] = [str(1 + i * nt_f)]
        nam[nn]['NQV_ADV_NUM'] = [str(nt_f)]
        i += 1
        for it in range(0, nt_f):
            nam[nn]['NL_QV_ADV_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

    if attributes['surface_forcing_wind'] == 'z0':
        if not(lsurfex):
            nam[nn]['RZ0_FORC'] = [str(case.variables['z0'].data[0])]

    if attributes['surface_forcing_temp'] == 'surface_flux':
        if not(lsurfex):
            if 'ts' in case.variables:
                nam[nn]['RTS_FORC'] = [str(case.variables['ts'].data[0])]
            elif 'tskin' in case.variables:
                nam[nn]['RTS_FORC'] = [str(case.variables['tskin'].data[0])]
            else:
                nam[nn]['RTS_FORC'] = ['300.',]

    if attributes['forc_geo'] == 1:
        nam['NAMCT0']['LSFORC']=['.TRUE.']
        nam[nn]['LGEOST_UV_FRC'] = ['.TRUE.']
        W = 7.2921e-5
        nam['NAMLSFORC']['RCORIO_FORC'] = [str(2. * W * math.sin(lat * math.pi / 180))]
        if not(lsurfex):
            nam[nn]['RZ0_FORC'] = [str(case.variables['z0'].data[0])] # Useful ?
        nam[nn]['NGEOST_U_DEB'] = [str(1 + nt_f * i)]
        nam[nn]['NGEOST_U_NUM'] = [str(nt_f)]
        i += 1
        nam[nn]['NGEOST_V_DEB'] = [str(1 + nt_f * i)] 
        nam[nn]['NGEOST_V_NUM'] = [str(nt_f)]
        i += 1
        for it in range(nt_f):
            nam[nn]['NL_GEOST_UV_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

    if attributes['forc_wa'] == 1:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LSW_FRC'] = ['TRUE']
        nam[nn]['NLSW_DEB'] = [str(1 + i * nt_f)]
        nam[nn]['NLSW_NUM'] = [str(nt_f)]
        i += 1
        for it in range(0, nt_f):
            nam[nn]['NL_LSW_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

    elif attributes['forc_wap'] == 1:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LSOMEGA_FRC'] = ['.TRUE.']
        nam[nn]['NLSOMEGA_DEB'] = [str(1 + i * nt_f)]
        nam[nn]['NLSOMEGA_NUM'] = [str(nt_f)]
        i += 1
        for it in range(0, nt_f):
            nam[nn]['NL_LSOMEGA_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

    if attributes['nudging_ua'] > 0. or attributes['nudging_va'] > 0.:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LUV_NUDG'] = ['.TRUE.']
        nam[nn]['NU_NUDG_DEB'] = [str(1 + i * nt_f)]
        nam[nn]['NUV_NUDG_NUM'] = [str(nt_f)]
        i += 1
        nam[nn]['NV_NUDG_DEB'] = [str(1 + i * nt_f)]
        i += 1
        for it in range(nt_f):
            nam[nn]['NL_UV_NUDG_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

        nam[nn]['RELAX_TAUU'] = [str(float(attributes['nudging_ua']))]
        nam['NAMTOPH']['ETRELAXU'] = [str(float(attributes['pa_nudging_ua']))]
        logger.warning('Nudging not yet fully validated')

    if attributes['nudging_ta'] > 0.:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LT_NUDG'] = ['.TRUE.']
        nam[nn]['NT_NUDG_DEB'] = [str(1 + i * nt_f)]
        nam[nn]['NT_NUDG_NUM'] = [str(nt_f)]
        i += 1
        for it in range(nt_f):
            nam[nn]['NL_T_NUDG_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

        nam[nn]['RELAX_TAUT'] = [str(float(attributes['nudging_ta']))]
        nam['NAMTOPH']['ETRELAXT'] = [str(float(attributes['pa_nudging_ta']))]
        logger.warning('Nudging not yet fully validated')

    if attributes['nudging_qv'] > 0. :
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LQV_NUDG'] = ['.TRUE.']
        nam[nn]['NQV_NUDG_DEB'] = [str(1 + i * nt_f)]
        nam[nn]['NQV_NUDG_NUM'] = [str(nt_f)]
        i += 1
        for it in range(nt_f):
            nam[nn]['NL_QV_NUDG_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

        nam[nn]['RELAX_TAUQ'] = [str(float(attributes['nudging_qv']))]
        nam['NAMTOPH']['ETRELAXQ'] = [str(float(attributes['pa_nudging_qv']))]
        logger.warning('Nudging not yet fully validated')

    nam['NAMGFL']['NGFL_FORC'] = [str(int(nt_f * i))]

    # Do we use SURFEX or not 
    if lsurfex: 
        nam["NAMARPHY"]['LMSE'] = [".TRUE."]
        nam["NAMCT0"]['LSFORCS'] = ['.FALSE.']
    else:
        nam["NAMARPHY"]['LMSE'] = [".FALSE."]
        if attributes['surface_forcing_temp'] == 'surface_flux' :
            nam["NAMCT0"]['LSFORCS'] = ['.TRUE.']
            nam[nn]["NSH_FORC_DEB"] = [str(int(1 + j * nt_f))]
            nam[nn]["NSH_FORC_NUM"] = [str(nt_f)]
            j += 1
            nam[nn]["NLH_FORC_DEB"] = [str(int(1 + j * nt_f))]
            nam[nn]["NLH_FORC_NUM"] = [str(nt_f)]
            j += 1
            nam[nn]["NTS_FORC_DEB"] = [str(int(1 + j * nt_f))]
            nam[nn]["NTS_FORC_NUM"] = [str(nt_f)]
            for it in range(nt_f):
               nam[nn]['NL_SH_ADV_TIME(   ' + str(int(it + 1)) + " )"] = [str(int(dt * it))]
               nam[nn]['NL_LH_ADV_TIME(   ' + str(int(it + 1)) + " )"] = [str(int(dt * it))]
               nam[nn]['NL_TS_ADV_TIME(   ' + str(int(it + 1)) + " )"] = [str(int(dt * it))]
            nam['NAMPHYDS']['NSFORC'] = [str(int((j + 1) * nt_f))]



    # -----------------------------------------------------------
    # Final writing
    # -----------------------------------------------------------

    writearp(nam,namout)

    logger.info('-'*40)

    return NSTOP
