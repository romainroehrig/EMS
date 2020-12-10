#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os

import namelist
import cdms2
import math

if not(os.getenv('LDEPHY') is None):
  lDEPHY = True
else:
  lDEPHY = False


def prep_nam_ATM(case,filecase,namref,timestep,NSTOP,namout=None,subcase=None):
  """
    Prepare ARPEGE-Climat namelist for MUSC simulation, 
    given information in filecase,
    and from ARPEGE-Climat namelist namref
  """

  if namout is None:
    namout = namref + '_' + case
    if subcase is not None:
      namout = namout + '_' + subcase        

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
    if param[-6:] == 'NREQIN': nam[nn][param] = ['0',]
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
  try:
    del(nam['NAMDIM'])
  except KeyError:
    pass      
  nam['NAMDIM'] = {}
  nam['NAMDIM']['NPROMA'] = ['-4',]

  # Update NAMCT1
  del(nam['NAMCT1'])
  nam['NAMCT1'] = {}
  nam['NAMCT1']['LRFILAF'] = ['.FALSE.',]

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
  nam['NAMXFU']['NFRRAZ'] = ['1',]


  # -----------------------------------------------------------
  # Case specific modifications in namref
  # -----------------------------------------------------------

  fin = cdms2.open(filecase)

  attributes = {}
  if lDEPHY:
    for att in ['adv_temp','adv_qv','adv_u','adv_v','rad_temp','forc_omega','forc_w','forc_geo','nudging_temp','nudging_qv','nudging_u','nudging_v']:
      attributes[att] = 0

    for att in ['p_nudging_temp','p_nudging_qv','p_nudging_u','p_nudging_v']:
      attributes[att] = 110000.       
  else:
    for att in ['tadvh','qadvh','qvadv','qvadvh','qvadvv','qtadvh','uadvh','vadvh','tadvv','qadvv','qtadvv','uadvv','vadvv','tadv','qadv','uadv','vadv','trad','forc_omega','forc_w','forc_geo','nudging_t','nudging_q','nudging_u','nudging_v']:
      attributes[att] = 0

    for att in ['p_nudging_t','p_nudging_q','p_nudging_u','p_nudging_v']:
      attributes[att] = 110000.     

  for att in fin.listglobal():
    attributes[att] = fin.getglobal(att)




  # Setting latitude and longitude
  tmp = fin('temp')
  lat = tmp.getLatitude()[0]
  lon = tmp.getLongitude()[0]
  nam['NEMGEO']['RLAT_ACAD'] = [str(float(lat)),]
  nam['NEMGEO']['RLON_ACAD'] = [str(float(lon)),]

  # Setting starting date
  startDate = str(fin.startDate)
  year = int(startDate[0:4])
  month = int(startDate[4:6])
  day = int(startDate[6:8])
  hour = int(startDate[8:10])
  minute = int(startDate[10:12])
  second = int(startDate[12:14])
#  second = 0
  nam['NAMRIP']['NINDAT'] = [startDate[0:8],]
  nam['NAMRIP']['NSSSSS'] = [str(int(hour*3600+minute*60+second)),]


  # Case with no radiation or radiation included in temperature advection
  if attributes['rad_temp'] in [1,'adv']:
    nam['NAMPHY']['LRAYFM'] = ['.FALSE.',]
    nam['NAERAD']['LRRTM'] =  ['.FALSE.',]
    nam['NAERAD']['NSW'] = ['2',]

  # MUSC Forcing
  nn = 'NAMLSFORC'
  for param in ['LFIXRAD','LGEOST_UV_FRC','LNOWINDTEND','LQV_ADV_FRC','LQV_NUDG','LSOMEGA_FRC','LSW_FRC','LT_ADV_FRC','LT_NUDG','LUV_ADV_FRC','LUV_NUDG']:
    nam[nn][param] = ['.FALSE.',]
  for param in ['LMUSCLFA','LSPS_FRC']:
    nam[nn][param] = ['.TRUE.',]
  #nam[nn]['LMUSCLFA'] = ['.FALSE.',]
  for param in ['NGEOST_U_NUM','NGEOST_V_NUM','NLSOMEGA_NUM','NLSW_NUM','NQV_ADV_NUM','NQV_NUDG','NT_ADV_NUM','NT_NUDG','NU_NUDG','NV_NUDG']:
    nam[nn][param] = ['-1',]

  if lDEPHY:
    if attributes['adv_temp'] == 1 or attributes['rad_temp'] == 1:
        nam[nn]['LT_ADV_FRC'] = ['.TRUE.',]    
    if attributes['adv_qv'] == 1:
        nam[nn]['LQV_ADV_FRC'] = ['.TRUE.',]
    if attributes['adv_u'] == 1 or attributes['adv_v'] == 1:
      nam[nn]['LUV_ADV_FRC'] = ['.TRUE.',]
        
  else:
    if attributes['tadv'] == 1 or attributes['tadvh'] == 1 or attributes['tadvv'] == 1 or attributes['trad'] == 1:
        nam[nn]['LT_ADV_FRC'] = ['.TRUE.',]
    if attributes['qadv'] == 1 or attributes['qadvh'] == 1 or attributes['qtadvh'] == 1 or attributes['qadvv'] == 1 or attributes['qvadv'] == 1 or attributes['qvadvh'] == 1 or attributes['qvadvv'] == 1 :
        nam[nn]['LQV_ADV_FRC'] = ['.TRUE.',]
    if attributes['uadvh'] == 1 or attributes['uadvh'] == 1 or attributes['uadvv'] == 1 or attributes['vadvv'] == 1:
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
  if lDEPHY:
    if attributes['nudging_temp'] > 0.:
        nam[nn]['LT_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUT'] = [str(float(attributes['nudging_temp'])),]
        nam['NAMTOPH']['ETRELAXT'] = [str(float(attributes['p_nudging_temp'])),]
    if attributes['nudging_qv'] > 0. :
        nam[nn]['LQV_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUQ'] = [str(float(attributes['nudging_qv'])),]
        nam['NAMTOPH']['ETRELAXQ'] = [str(float(attributes['p_nudging_qv'])),]      
  else:
    if attributes['nudging_t'] > 0.:
        nam[nn]['LT_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUT'] = [str(float(attributes['nudging_t'])),]
        nam['NAMTOPH']['ETRELAXT'] = [str(float(attributes['p_nudging_t'])),]      
    if attributes['nudging_q'] > 0. :
        nam[nn]['LQV_NUDG'] = ['.TRUE.',]
        nam[nn]['RELAX_TAUQ'] = [str(float(attributes['nudging_q'])),]
        nam['NAMTOPH']['ETRELAXQ'] = [str(float(attributes['p_nudging_q'])),]

  if attributes.has_key('RCE') and attributes['RCE'] == 1:
      nam['NAMAQUAMF'] = {}
      nam['NAMCT0']['LRCE'] = ['.TRUE.',]
      if not(attributes['rad_temp'] in [1,'adv']):
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
