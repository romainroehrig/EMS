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


def prep_nam_ATM(case,subcase,filecase,namref,timestep,NSTOP,namout=None,lsurfex=False):
    """
    Prepare AROME namelist for MUSC simulation, 
    given information in filecase,
    and from AROME namelist namref
    """

    if namout is None:
        namout = '{0}_{1}_{2}'.format(namref,case,subcase)

    print '-'*40
    print 'Prepare AROME namelist for MUSC'
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
                if obj == '__MP_TYPE__': nam[namin][param][i] = '2'
                if obj == '__NTASKS__': nam[namin][param][i] = '1'
                if obj == '__MBX_SIZE__': nam[namin][param][i] = '2048000000'
                if obj == '__LOPT_SCALAR__': nam[namin][param][i] = '.TRUE.'
                if obj == '__NCOMBFLEN__': nam[namin][param][i] = '1800000'

    # Aerosols and Ozone
    nam['NAMPHY']['LOZONE'] = ['.FALSE.',]
    nam['NAMPHY']['LRAYFM'] = ['.FALSE.',]
    nam['NAMPHY']['LO3ABC'] = ['.FALSE.',]
    nam['NAMPHY']['LAEROSEA'] = ['.FALSE.',]
    nam['NAMPHY']['LAEROLAN'] = ['.FALSE.',]
    nam['NAMPHY']['LAEROSOO'] = ['.FALSE.',]
    nam['NAMPHY']['LAERODES'] = ['.FALSE.',]
    nam['NAMPHY']['LEDR'] = ['.FALSE.',]
    nam['NAMPHY']['LO3FL'] = ['.FALSE.',]

    # Empty a few namelists
    for nn in ['NAMFPC','NAMFPD','NAMLSFORC','NAMDYNA','NAMFPSC2_DEP']:
      del(nam[nn])
      nam[nn] = {}

    # Add a few namelists
    for nn in []:
      nam[nn] = {}      

    # Update NAMGFL
    #fin = cdms2.open(filecase)
    #time = fin('temp').getTime()
    nn='NAMGFL'
    del(nam[nn])
    nam[nn] = {}
    #nt = time.shape[0]
    for param in nam[nn].keys():
        if param[-6:] == 'NREQIN': nam[nn][param] = ['0',]
        if param[-7:] == 'LREQOUT': nam[nn][param] = ['.FALSE.']
    for name in ['TKE','S','R','Q','L','I','G','CVGQ',]:
        if name == 'CVGQ' : 
            nam[nn]['Y'+name+'_NL%LCDERS'] = ['.FALSE.',]
            nam[nn]['Y'+name+'_NL%LSP'] = ['.FALSE.',]      
            nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
        elif name == 'Q': 
            nam[nn]['Y'+name+'_NL%LSP'] = ['.FALSE.',]      
            nam[nn]['Y'+name+'_NL%LQM'] = ['.FALSE.',]
            nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
            nam[nn]['Y'+name+'_NL%NREQIN'] = ['1',]            
        elif name in ['L','I','R','S','G']:
            nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LPHY'] = ['.FALSE.',]
            nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
            nam[nn]['Y'+name+'_NL%NREQIN'] = ['0',]
            nam[nn]['Y'+name+'_NL%LADV'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LREQOUT'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LT1'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LQM'] = ['.TRUE.',]
        elif name == 'TKE': 
            nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LADV'] = ['.FALSE.',]
            nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LQM'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LREQOUT'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%LT1'] = ['.TRUE.',]
            nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
            nam[nn]['Y'+name+'_NL%NREQIN'] = ['-1',]
            nam[nn]['Y'+name+'_NL%REFVALI'] = ['0.000001',]
        else:
            print 'ERROR: case unexpected:', name
            raise ValueError
    nam[nn]['NGFL_EZDIAG'] = ['4',]
    for i in range(1,5):
        nam[nn]["YEZDIAG_NL(" + str(i) + ")%CNAME"] = ["'EZDIAG0{0}'".format(i),]

    # Update due to MUSC/ALADIN config
    nn = 'NAMDYN'

    # Update NAMDIM
    nn = 'NAMDIM'

    # Update NAMCT1
    nn='NAMCT1'
    del(nam[nn])
    nam[nn] = {}
    nam[nn]['LRFILAF'] = ['.FALSE.',]
    nam[nn]['N1POS']=['0',]
    nam[nn]['N1RES']=['0',]
    nam[nn]['N1SFXHIS']=['0',]

    # Update NAMCT0
    nn = 'NAMCT0'

    # Update NAMXFU
    nn =  'NAMXFU'
    for param in nam[nn].keys():
        if param[0] == 'L': nam[nn][param] = ['.FALSE.',]

    # Update NAMARG
    nn = 'NAMARG' 
    nam[nn]['CNMEXP'] = ["'ARPE'",]

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
  
    nam['NAMRIP']['TSTEP'] = [str(timestep),]
    nam['NAMRIP']['CSTOP'] = ["'"+NSTOP+"'",]
#    nam['NAMRIP']['NINDAT'] = [startDate[0:8],]
#    nam['NAMRIP']['NSSSSS'] = [str(int(hour*3600+minute*60+second)),]


    # Case with no radiation or radiation included in temperature advection
    nam['NAERAD']['LRRTM'] =  ['.FALSE.',]
    if attributes['rad_temp'] in [1,'adv']:
        nam['NAMPHY']['LRAYFM'] = ['.FALSE.',]
        nam['NAERAD']['LSRTM'] =  ['.FALSE.',]
        nam['NAERAD']['NSW'] = ['6',]
        nam['NAERAD']['NOZOCL'] = ['2',]

    # MUSC Forcing
    nn = 'NAMLSFORC'
    del(nam[nn])
    nam[nn] = {}

    i=0 # compteur
    j=0 # compteur

    timein = case.variables['pressure_forc'].time
    nt_f = timein.length
  
    if nt_f <= 1:
        dt = 0.
    else:  
        dt = timein.data[1]-timein.data[0]
      
    for param in ['LGEOST_UV_FRC','LQV_ADV_FRC','LT_ADV_FRC']:
        nam[nn][param] = ['.FALSE.',]
    nam[nn]['LMUSCLFA'] = ['.TRUE.',]
    for param in ['NGEOST_U_NUM','NGEOST_V_NUM','NQV_ADV_NUM','NT_ADV_NUM']:
        nam[nn][param] = ['-1',]

    if attributes['adv_temp'] == 1 or attributes['rad_temp'] == 1:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LT_ADV_FRC'] = ['.TRUE.',]
        nam[nn]['NT_ADV_DEB'] = [str(1+i*nt_f),]
        nam[nn]['NT_ADV_NUM'] = [str(nt_f),]
        i=i+1
        for it in range(0,nt_f):
            nam[nn]['NL_T_ADV_TIME(   '+str(int(it+1))+' )']=[str(int(dt*it)),]

    if attributes['adv_qv'] == 1:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LQV_ADV_FRC'] = ['.TRUE.',]
        nam[nn]['NQV_ADV_DEB'] = [str(1+i*nt_f),]
        nam[nn]['NQV_ADV_NUM'] = [str(nt_f),]
        i=i+1
        for it in range(0,nt_f):
            nam[nn]['NL_QV_ADV_TIME(   '+str(int(it+1))+' )']=[str(int(dt*it)),]

    if attributes['adv_u'] == 1 or attributes['adv_v'] == 1:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LUV_ADV_FRC'] = ['.TRUE.',]
        print 'ERROR: Case not yet coded for adv_u/adv_v = 1'
        raise ValueError

    if attributes['forc_omega'] == 1:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LSOMEGA_FRC'] = ['.TRUE.',]
        print 'ERROR: Case not yet coded for forc_omega = 1'
        raise ValueError

    if attributes['forc_w'] == 1:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LSW_FRC'] = ['TRUE',]
        print 'ERROR: Case not yet coded for forc_w = 1'
        raise ValueError

    if attributes['forc_geo'] == 1:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LGEOST_UV_FRC'] = ['.TRUE.',]
        W=7.2921e-5
        nam[nn]['RCORIO_FORC'] = [str(2.*W*math.sin(lat*math.pi/180)),]
        nam[nn]['RZ0_FORC'] = ['0.035',]
        nam[nn]['NGEOST_U_DEB']=[str(1+nt_f*i),]
        nam[nn]['NGEOST_U_NUM']=[str(nt_f),]
        i=i+1
        nam[nn]['NGEOST_V_DEB']=[str(1+nt_f*i),] 
        nam[nn]['NGEOST_V_NUM']=[str(nt_f),]
        i=i+1
        for it in range(0,nt_f):
            nam[nn]['NL_GEOST_UV_TIME(   '+str(int(it+1))+' )']=[str(int(dt*it)),]

    if attributes['nudging_u'] > 0. or attributes['nudging_v'] > 0.:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LUV_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUU'] = [str(float(attributes['nudging_u'])),]
        nam['NAMTOPH']['ETRELAXU'] = [str(float(attributes['p_nudging_u'])),]
        print 'ERROR: Case not yet coded for nudging_u/v > 0'
        raise ValueError


    if attributes['nudging_temp'] > 0.:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LT_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUT'] = [str(float(attributes['nudging_temp'])),]
        nam['NAMTOPH']['ETRELAXT'] = [str(float(attributes['p_nudging_temp'])),]
        print 'ERROR: Case not yet coded for nudging_temp > 0'
        raise ValueError

    if attributes['nudging_qv'] > 0.:
        nam["NAMCT0"]['LSFORC']=['.TRUE.',]
        nam[nn]['LQV_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUQ'] = [str(float(attributes['nudging_qv'])),]
        nam['NAMTOPH']['ETRELAXQ'] = [str(float(attributes['p_nudging_qv'])),]
        print 'ERROR: Case not yet coded for nudging_qv > 0'
        raise ValueError


    if attributes['surfaceForcing'] == "surfaceFlux" :
        nam["NAMCT0"]['LSFORCS']=['.TRUE.',]
        nam[nn]["NSH_FORC_DEB"]=[str(int(1+j*nt_f)),]
        nam[nn]["NSH_FORC_NUM"]=[str(nt_f),]
        j=j+1
        nam[nn]["NLH_FORC_DEB"]=[str(int(1+j*nt_f)),]
        nam[nn]["NLH_FORC_NUM"]=[str(nt_f),]
        for it in range(0,nt_f):
           nam[nn]['NL_SH_ADV_TIME(   '+str(int(it+1))+" )"]=[str(int(dt*it)),]
           nam[nn]['NL_LH_ADV_TIME(   '+str(int(it+1))+" )"]=[str(int(dt*it)),]
        nam["NAMPHYDS"]['NSFORC']=[str(int(2*nt_f)),]

    nam['NAMGFL']['NGFL_FORC'] = [str(int(nt_f*i)),]



    # Do we use SURFEX or not 
    if lsurfex : 
        nam["NAMARPHY"]['LMSE'] = [".TRUE.",]
    else:
        nam["NAMARPHY"]['LMSE'] = [".FALSE.",]

    # -----------------------------------------------------------
    # Final writing
    # -----------------------------------------------------------

    namelist.writearp(nam,namout)

    print '-'*40
