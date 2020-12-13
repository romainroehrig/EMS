#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
repEMS = os.getenv('REP_EMS')

import sys
sys.path.append('{0}/DEPHY-SCM/utils/'.format(repEMS))

import math

import netCDF4 as nc

from Case import Case

import namelist

lverbose = False

def prep_nam_ATM(case,subcase,filecase,namref,timestep,NSTOP,namout=None):
    """
    Prepare ARPEGE-Climat namelist for MUSC simulation, 
    given information in filecase,
    and from ARPEGE-Climat namelist namref
    """

    if namout is None:
        namout = '{0}_{1}_{2}'.format(namref,case,subcase)

    print '-'*40
    print 'Prepare ARPEGE-Climat namelist for MUSC'
    print 'case:', case, 'subcase:', subcase
    print 'Reference namelist:', namref
    print 'Output namelist:', namout

    nam = namelist.readarp(namref)

    # -----------------------------------------------------------
    # Some general modification in namref
    # -----------------------------------------------------------

    for namin in nam.keys():
        for param in nam[namin].keys():
            tmp = enumerate(nam[namin][param])
            for i, obj in tmp:  
                if obj == '.T.': nam[namin][param][i] = '.TRUE.'
                if obj == '.F.': nam[namin][param][i] = '.FALSE.'

    # Aerosols and Ozone
    nam['NAMPHY']['LAEROSUL'] = ['.FALSE.',]
    nam['NAMPHY']['LAEROVOL'] = ['.FALSE.',]

    nam['NAMPHY']['LOZONE'] = ['.FALSE.',]

    nam['NAMPHY']['LRELAXT'] = ['.FALSE.',]
    nam['NAMPHY']['LRELAXW'] = ['.FALSE.',]

    # Various update
    nam['NAMMCC']['LMCC03'] = ['.FALSE.',]
    for param in ['NPRGPEW','NPRGPNS','NPROC','NPRTRV','NPRTRW']:
        nam['NAMPAR0'][param] = ['1',]
    nam['NAMPAR1']['LEQ_REGIONS'] = ['.FALSE.',]
    for param in ['NSTRIN','NSTROUT']:
        nam['NAMPAR1'][param] = ['1',]

    # Empty a few namelists
    for nn in ['NAMFPC','NAMAFN','NAMDPHY','NAMVV1','NAMRGRI']:
        del(nam[nn])
        nam[nn] = {}

    # Add a few namelists
    for nn in ['NAMDIMO','NEMELBC0A','NEMELBC0B']:
        nam[nn] = {}      

    # Update NAMGFL
    nn = 'NAMGFL'
    nam[nn]['NGFL_FORC'] = ['0',]
    nam[nn]['YCVGQ_NL%LCDERS'] = ['.TRUE.',]
    nam[nn]['YCVGQ_NL%LSP'] = ['.TRUE.',]
    for param in nam[nn].keys():
        if param[-6:] == 'NREQIN': 
            nam[nn][param] = ['0',]
        if 'YL_NL%NREQIN' in nam[nn].keys():
            nam[nn]['YL_NL%NREQIN'] = ['1',]
        if 'YI_NL%NREQIN' in nam[nn].keys():    
            nam[nn]['YI_NL%NREQIN'] = ['1',]
        if 'YTKE_NL%NREQIN' in nam[nn].keys():    
            nam[nn]['YTKE_NL%NREQIN'] = ['1',]

    # Update due to MUSC/ALADIN config
    nn = 'NAMDYN'
    nam[nn]['LADVF'] = ['.FALSE.',]
    nam[nn]['LSETTLS'] = ['.FALSE.',]
    nam[nn]['NCOMP_CVGQ'] = ['1',]
    try:
        del(nam[nn]['NSREFDH'])
        del(nam[nn]['TSTEP'])
    except KeyError:
        pass      
    nam[nn]['VMAX1'] = ['100.',]
    nam[nn]['VMAX2'] = ['120.',]

    # Update NAMDIM
    nn = 'NAMDIM'
    try:
        del(nam[nn])
    except KeyError:
        pass      
    nam[nn] = {}
    nam[nn]['NPROMA'] = ['-4',]

    # Update NAMCT1
    nn = 'NAMCT1'
    del(nam[nn])
    nam[nn] = {}
    nam[nn]['LRFILAF'] = ['.FALSE.',]

    # Update NAMCT0
    nn = 'NAMCT0'
    nam[nn]['LAPRXPK'] = ['.FALSE.',]
    nam[nn]['LCALLSFX'] = ['.TRUE.',]
    nam[nn]['LCORWAT'] = ['.FALSE.',]
    nam[nn]['LELAM'] = ['.TRUE.',]
    nam[nn]['LFPOS'] = ['.FALSE.',]
    nam[nn]['LREGETA'] = ['.TRUE.',]
    nam[nn]['LRPLANE'] = ['.TRUE.',]
    nam[nn]['LSFORC'] = ['.TRUE.',]
    nam[nn]['LVERTFE'] = ['.FALSE.',]
    nam[nn]['LSFXORO'] = ['.FALSE.',]
    tmp = nam[nn].keys()
    for param in tmp:
        if param[0] == 'N':
            del(nam[nn][param])

    nam[nn]['NCONF'] = ['1',]
    nam[nn]['NFRHIS'] = ['1',]
    nam[nn]['NFRPOS'] = ['1',]
    nam[nn]['NFRSDI'] = ['1',]
    nam[nn]['NFRSFXHIS'] = ['1',]
    nam[nn]['NHISTS(0)'] = ['1',]
    nam[nn]['NPRINTLEV'] = ['2',]

    # Update NAMXFU
    nn = 'NAMXFU'
    nam[nn]['NFRRAZ'] = ['1',]


    # -----------------------------------------------------------
    # Case specific modifications in namref
    # -----------------------------------------------------------

    case = Case('{0}/{1}'.format(case,subcase))
    case.read('data_input.nc')

    if lverbose:
         case.info()

    attributes = case.attributes

    # Yet undefined in DEPHY format
    attributes['adv_u'] = 0
    attributes['adv_v'] = 0

    lat = float(case.lat)
    lon = float(case.lon)

    # Setting latitude and longitude
    nam['NEMGEO']['RLAT_ACAD'] = [str(float(lat)),]
    nam['NEMGEO']['RLON_ACAD'] = [str(float(lon)),]

    # Setting starting date
    startDate = case.startDate
    year = int(startDate[0:4])
    month = int(startDate[4:6])
    day = int(startDate[6:8])
    hour = int(startDate[8:10])
    minute = int(startDate[10:12])
    second = int(startDate[12:14])
    nam['NAMRIP']['NINDAT'] = [startDate[0:8],]
    nam['NAMRIP']['NSSSSS'] = [str(int(hour*3600+minute*60+second)),]


    # Case with no radiation or radiation included in temperature advection
    if attributes['rad_temp'] in [1,'adv']:
        nam['NAMPHY']['LRAYFM'] = ['.FALSE.',]
        nam['NAERAD']['LRRTM'] =  ['.FALSE.',]
        nam['NAERAD']['NSW'] = ['2',]      

    # MUSC Forcing
    nn = 'NAMLSFORC'
    del(nam[nn])
    nam[nn] = {}
    for param in ['LFIXRAD','LGEOST_UV_FRC','LNOWINDTEND','LQV_ADV_FRC','LQV_NUDG','LSOMEGA_FRC','LSW_FRC','LT_ADV_FRC','LT_NUDG','LUV_ADV_FRC','LUV_NUDG']:
        nam[nn][param] = ['.FALSE.',]
    for param in ['LMUSCLFA','LSPS_FRC']:
        nam[nn][param] = ['.TRUE.',]
    for param in ['NGEOST_U_NUM','NGEOST_V_NUM','NLSOMEGA_NUM','NLSW_NUM','NQV_ADV_NUM','NQV_NUDG','NT_ADV_NUM','NT_NUDG','NU_NUDG','NV_NUDG']:
        nam[nn][param] = ['-1',]

    if attributes['adv_temp'] == 1 or attributes['rad_temp'] == 1:
        nam[nn]['LT_ADV_FRC'] = ['.TRUE.',]
    if attributes['adv_qv'] == 1:
        nam[nn]['LQV_ADV_FRC'] = ['.TRUE.',]
    if attributes['adv_u'] == 1 or attributes['adv_v'] == 1:
        nam[nn]['LUV_ADV_FRC'] = ['.TRUE.',]      
    if attributes['forc_omega'] == 1:
        nam[nn]['LSOMEGA_FRC'] = ['.TRUE.',]
    if attributes['forc_w'] == 1:
        nam[nn]['LSW_FRC'] = ['TRUE',]
    if attributes['forc_geo'] == 1:
        nam[nn]['LGEOST_UV_FRC'] = ['.TRUE.',]
        W=7.2921e-5
        nam['NAMLSFORC']['RCORIO_FORC'] = [str(2.*W*math.sin(lat*math.pi/180)),]
    if attributes['nudging_u'] > 0. or attributes['nudging_v'] > 0.:
        nam[nn]['LUV_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUU'] = [str(float(attributes['nudging_u'])),]
        nam['NAMTOPH']['ETRELAXU'] = [str(float(attributes['p_nudging_u'])),]
    if attributes['nudging_temp'] > 0.:
        nam[nn]['LT_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUT'] = [str(float(attributes['nudging_temp'])),]
        nam['NAMTOPH']['ETRELAXT'] = [str(float(attributes['p_nudging_temp'])),]      
    if attributes['nudging_qv'] > 0. :
        nam[nn]['LQV_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUQ'] = [str(float(attributes['nudging_qv'])),]
        nam['NAMTOPH']['ETRELAXQ'] = [str(float(attributes['p_nudging_qv'])),]      

    if attributes.has_key('RCE') and attributes['RCE'] == 1:
        nam['NAMAQUAMF'] = {}
        nam['NAMCT0']['LRCE'] = ['.TRUE.',]
        if lDEPHY:
            trad = 'rad_temp'
        else:
            trad = 'trad'
        if not(attributes[trad] in [1,'adv']):
            nam['NAMRIP']['RANGLE'] = [str(float(attributes['zangle'])),]
            nam['NAMSCEN']['RI0'] = [str(float(attributes['I0'])),]
            nn = 'NAMCLDP'
            if attributes.has_key('CCN'):
                tmp = math.log(attributes['CCN']/1.e6)/math.log(10)
            else:
                tmp = 2.
            nam[nn]['RCCNCST'] = [str(tmp),]
            nam[nn]['RCCNOM'] = ['0.',]
            nam[nn]['RCCNSS'] = ['0.',]
            nam[nn]['RCCNSU'] = ['0.',]
            nn = 'NAERAD'
            GHG = {}
            GHG['CO2'] = 348.
            GHG['CH4'] = 1650.
            GHG['N2O'] = 306.
            GHG['CFC11'] = 0.
            GHG['CFC12'] = 0.
            coefs = {}
            coefs['CO2'] = 1.e-6    # ppmv
            coefs['CH4'] = 1.e-9    # ppbv
            coefs['N2O'] = 1.e-9    # ppbv
            coefs['CFC11'] = 1.e-12 # pptv
            coefs['CFC12'] = 1.e-12 # pptv
            for g in GHG.keys():
                if attributes.has_key(g):
                    nam[nn]['RC'+g] = [str(float(attributes[g])*coefs[g]),]
                else: 
                    nam[nn]['RC'+g] = [str(GHG[g]*coefs[g]),]

    # -----------------------------------------------------------
    # Final writing
    # -----------------------------------------------------------

    namelist.writearp(nam,namout)

    print '-'*40
