#!/usr/bin/env python
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
Namelist for AROME before 46t1, atmospheric part
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
    logger.info('Prepare AROME namelist for MUSC')
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
                if obj == 'NDPROC': nam[namin][param][i] = '1'
                if obj == 'NBPROC': nam[namin][param][i] = '1'
                if obj == 'NCPROC': nam[namin][param][i] = '1'
                if obj == 'NBPROC_IO': nam[namin][param][i] = '1'

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
    for var in ['L', 'I', 'R', 'S', 'G', 'TKE']:
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

    nam[nn]['NGFL_EZDIAG'] = ['23']
    for i in range(1, 24):
        nam[nn]["YEZDIAG_NL(" + str(i) + ")%CNAME"] = ["'EZDIAG{0:0>2}'".format(i)]

    # Update due to MUSC/ALADIN config
    nn = 'NAMDYN'
    nam[nn]['LADVF'] = ['.FALSE.']
    nam[nn]['NSITER'] = ['0']    

    # Update NAMDIM
    nn = 'NAMDIM'
    try:
        del(nam[nn])
    except KeyError:
        pass      
    nam[nn] = {}
    nam[nn]['NPROMA'] = ['50']    

    # Update NAMDYNA
    nn='NAMDYNA'
    try:
        del(nam[nn])
    except KeyError:
        pass      
    nam[nn] = {}
    nam[nn]['NDLNPR'] = ['0']

    # Update NAMIO_SERV
    nn='NAMIO_SERV'
    nam[nn]['NIO_SERV_BUF_MAXSIZE'] = ['0']
    nam[nn]['NPROCESS_LEVEL'] = ['0']
    nam[nn]['NPROC_IO'] = ['0']

    # Update NAMCT1
    nn = 'NAMCT1'
    del(nam[nn])
    nam[nn] = {}
    nam[nn]['LRFILAF'] = ['.FALSE.']
    nam[nn]['N1HIS'] = ['0']
    nam[nn]['N1ISP'] = ['0']
    nam[nn]['N1POS'] = ['1']
    nam[nn]['N1RES'] = ['0']
    nam[nn]['N1SDI'] = ['0']
    nam[nn]['N1SFXHIS'] = ['0']

    # Update NAMCT0
    nn = 'NAMCT0'
    nam[nn] = {}
    nam[nn]['LOPT_SCALAR'] = ['.TRUE.']
    nam[nn]['LSCREEN_OPENMP'] = ['.FALSE.']
    nam[nn]['LTWOTL'] = ['.TRUE.']
    nam[nn]['LNHDYN'] = ['.FALSE.']
    nam[nn]['LPC_FULL'] = ['.FALSE.']
    nam[nn]['LPC_NESC'] = ['.FALSE.']
    nam[nn]['LPC_NESCT'] = ['.FALSE.']
    nam[nn]['LPC_NESCV'] = ['.FALSE.']
    nam[nn]['LPC_CHEAP'] = ['.FALSE.']
    nam[nn]['LSPRT'] = ['.TRUE.']
    nam[nn]['LAROME'] = ['.TRUE.']
    nam[nn]['CNPPATH'] = ["'.'"]
    nam[nn]['LREGETA'] = ['.TRUE.']
    nam[nn]['CFPATH'] = ["'ICMSH'"]
    nam[nn]['LFDBOP'] = ['.FALSE.']
    nam[nn]['LFBDAP'] = ['.TRUE.']
    nam[nn]['LSFORC'] = ['.TRUE.']
    nam[nn]['NFRHIS'] = ['1']
    nam[nn]['NFPOS'] = ['0']
    nam[nn]['NFRPOS'] = ['1']
    nam[nn]['NFRISP'] = ['1']
    nam[nn]['NFRSDI'] = ['1']
    nam[nn]['NHISTS(0)'] = ['1']
    nam[nn]['NPRINTLEV'] = ['2']    

    # Update NAMXFU
    nn = 'NAMXFU'
    for param in nam[nn].keys():
        if param[0] == 'L': nam[nn][param] = ['.FALSE.']

    # Update NAMPAR1
    nn = 'NAMPAR1'
    del(nam[nn])
    nam[nn]={}
    nam[nn]['LEQ_REGIONS'] = ['.FALSE.']
    nam[nn]['LSLONDEM'] = ['.TRUE.']
    nam[nn]['LSPLIT'] = ['.FALSE.']
    nam[nn]['LSYNC_SLCOM'] = ['.TRUE.']
    nam[nn]['LSYNC_TRANS'] = ['.TRUE.']
    nam[nn]['L_GATHERV_WRGP'] = ['.FALSE.']
    nam[nn]['NCOMBFLEN'] = ['1800000']
    nam[nn]['NSTRIN'] = ['100']
    nam[nn]['NSTROUT'] = ['1']    

    # Update NAMPARAR
    nn='NAMPARAR'
    nam[nn]['CMF_CLOUD'] = ["'DIRE'"]
    nam[nn]['CMF_UPDRAFT'] = ["'EDKF'"]
    nam[nn]['LMIXUV'] = ['.TRUE.']
    nam[nn]['NSWB_MNH'] = ['6']

    # Update NAMARG
    nn = 'NAMARG' 
    nam[nn]['CNMEXP'] = ["'ARPE'"]

    # Update NAMCFU
    nn = 'NAMCFU'
    for param in nam[nn].keys():
        nam[nn][param] = [".FALSE."]

    # Update NEMELBC0A
    nn = 'NEMELBC0A'
    nam[nn]['LESPCPL'] = [".FALSE."]

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

    nam['NAMARG']['TSTEP'] = [str(timestep)]
    nam['NAMARG']['CUSTOP'] = [NSTOP]
    nam['NAMRIP']['NINDAT'] = [startDate.strftime('%Y%m%d')]
    nam['NAMRIP']['NSSSSS'] = [str(int(hour * 3600 + minute * 60 + second))]


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

    timein = case.variables['pressure_forc'].time
    nt_f = timein.length
    if nt_f <= 1:
        dt = 0.
    else:  
        dt = timein.data[1]-timein.data[0]

    for param in ['LGEOST_UV_FRC', 'LQV_ADV_FRC', 'LT_ADV_FRC']:
        nam[nn][param] = ['.FALSE.']
    nam[nn]['LMUSCLFA'] = ['.TRUE.']

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

    if attributes['adv_ua'] == 1 or attributes['adv_va'] == 1:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LUV_ADV_FRC'] = ['.TRUE.']
        logger.error('ERROR: Case not yet coded for adv_ua/adv_va = 1')
        raise ValueError

    if attributes['forc_wap'] == 1:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LSOMEGA_FRC'] = ['.TRUE.']
        logger.error('ERROR: Case not yet coded for forc_wap = 1')
        raise ValueError

    if attributes['forc_wa'] == 1:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LSW_FRC'] = ['TRUE']
        logger.error('ERROR: Case not yet coded for forc_wa = 1')
        raise ValueError

    if attributes['forc_geo'] == 1:
        nam['NAMCT0']['LSFORC']=['.TRUE.']
        nam[nn]['LGEOST_UV_FRC'] = ['.TRUE.']
        W = 7.2921e-5
        nam['NAMLSFORC']['RCORIO_FORC'] = [str(2. * W * math.sin(lat * math.pi / 180))]
        nam[nn]['RZ0_FORC'] = ['0.035']
        nam[nn]['NGEOST_U_DEB'] = [str(1 + nt_f * i)]
        nam[nn]['NGEOST_U_NUM'] = [str(nt_f)]
        i += 1
        nam[nn]['NGEOST_V_DEB'] = [str(1 + nt_f * i)] 
        nam[nn]['NGEOST_V_NUM'] = [str(nt_f)]
        i += 1
        for it in range(nt_f):
            nam[nn]['NL_GEOST_UV_TIME(   ' + str(int(it + 1)) + ' )'] = [str(int(dt * it))]

    if attributes['nudging_ua'] > 0. or attributes['nudging_va'] > 0.:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LUV_NUDG'] = ['.TRUE.']
        nam[nn]['RELAX_TAUU'] = [str(float(attributes['nudging_ua']))]
        nam['NAMTOPH']['ETRELAXU'] = [str(float(attributes['pa_nudging_ua']))]
        logger.error('ERROR: Case not yet coded for nudging_ua/va > 0')
        raise ValueError

    if attributes['nudging_ta'] > 0.:
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LT_NUDG'] = ['.TRUE.']
        nam[nn]['RELAX_TAUT'] = [str(float(attributes['nudging_ta']))]
        nam['NAMTOPH']['ETRELAXT'] = [str(float(attributes['pa_nudging_ta']))]
        logger.error('ERROR: Case not yet coded for nudging_ta > 0')
        raise ValueError

    if attributes['nudging_qv'] > 0. :
        nam['NAMCT0']['LSFORC'] = ['.TRUE.']
        nam[nn]['LQV_NUDG'] = ['.TRUE.']
        nam[nn]['RELAX_TAUQ'] = [str(float(attributes['nudging_qv']))]
        nam['NAMTOPH']['ETRELAXQ'] = [str(float(attributes['pa_nudging_qv']))]
        logger.error('ERROR: Case not yet coded for nudging_qv > 0')
        raise ValueError

    if attributes['surface_forcing_ts'] == 'surface_flux' :
        nam["NAMCT0"]['LSFORCS'] = ['.TRUE.']
        nam[nn]["NSH_FORC_DEB"] = [str(int(1 + j * nt_f))]
        nam[nn]["NSH_FORC_NUM"] = [str(nt_f)]
        j += 1
        nam[nn]["NLH_FORC_DEB"] = [str(int(1 + j * nt_f))]
        nam[nn]["NLH_FORC_NUM"] = [str(nt_f)]
        for it in range(nt_f):
           nam[nn]['NL_SH_ADV_TIME(   ' + str(int(it + 1)) + " )"] = [str(int(dt * it))]
           nam[nn]['NL_LH_ADV_TIME(   ' + str(int(it + 1)) + " )"] = [str(int(dt * it))]
        nam["NAMPHYDS"]['NSFORC'] = [str(int(2 * nt_f))]

    nam['NAMGFL']['NGFL_FORC'] = [str(int(nt_f * i))]

    # Do we use SURFEX or not 
    if lsurfex: 
        nam["NAMARPHY"]['LMSE'] = [".TRUE."]
    else:
        nam["NAMARPHY"]['LMSE'] = [".FALSE."]


    # -----------------------------------------------------------
    # Final writing
    # -----------------------------------------------------------

    writearp(nam,namout)

    logger.info('-'*40)

    return NSTOP
