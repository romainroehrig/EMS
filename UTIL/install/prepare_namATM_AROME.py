#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import namelist
import cdms2
import math


def prep_nam_ATM(case,filecase,namref,timestep,NSTOP,namout=None,subcase=None,lsurfex=False):

  """
    Prepare AROME namelist for MUSC simulation, 
    given information in filecase,
    and from AROME namelist namref
  """

  if namout is None:
    namout = namref + '_' + case
    if subcase is not None:
      namout = namout + '_' + subcase        

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
        if obj == 'NDPROC': nam[namin][param][i] = '1'
        if obj == 'NBPROC': nam[namin][param][i] = '1'
        if obj == 'NCPROC': nam[namin][param][i] = '1'
        if obj == 'NBPROC_IO': nam[namin][param][i] = '1'

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
  fin = cdms2.open(filecase)
  time = fin('temp').getTime()
  nn='NAMGFL'
  del(nam[nn])
  nam[nn] = {}
  nt = time.shape[0]
  for param in nam[nn].keys():
    if param[-6:] == 'NREQIN': nam[nn][param] = ['0',]
    if param[-7:] == 'LREQOUT': nam[nn][param] = ['.FALSE.']
  nam[nn]['NGFL_EZDIAG'] = ['23',]
  for it in range(1,24):
      nam[nn]['YEZDIAG_NL('+str(it)+')%CNAME'] = ["'EZDIAG"+str(it).zfill(2)+"'",]
  for name in ['TKE','S','R','Q','L','I','G','CVGQ',]:
      if name == 'CVGQ' : 
        nam[nn]['Y'+name+'_NL%LCDERS'] = ['.FALSE.',]
        nam[nn]['Y'+name+'_NL%LSP'] = ['.FALSE.',]      
        nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
      elif name == 'G':
        nam[nn]['Y'+name+'_NL%LPT'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
        nam[nn]['Y'+name+'_NL%NREQIN'] = ['0',]
      elif name == 'I':
        nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LPHY'] = ['.FALSE.',]
        nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
        nam[nn]['Y'+name+'_NL%NREQIN'] = ['0',]
        nam[nn]['Y'+name+'_NL%LADV'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LREQOUT'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LT1'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LQM'] = ['.TRUE.',]
      elif name == 'L':
        nam[nn]['Y'+name+'_NL%LPHY'] = ['.FALSE.',]
        nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LADV'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LQM'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LREQOUT'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LT1'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
        nam[nn]['Y'+name+'_NL%NREQIN'] = ['0',]
      elif name == 'Q': 
        nam[nn]['Y'+name+'_NL%LSP'] = ['.FALSE.',]      
        nam[nn]['Y'+name+'_NL%LQM'] = ['.FALSE.',]
        nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
        nam[nn]['Y'+name+'_NL%NREQIN'] = ['1',]
      elif name == 'R': 
        nam[nn]['Y'+name+'_NL%LPHY'] = ['.FALSE.',]
        nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LQM'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LREQOUT'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LT1'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
        nam[nn]['Y'+name+'_NL%NREQIN'] = ['0',]
        nam[nn]['Y'+name+'_NL%LADV'] = ['.TRUE.',]
      elif name == 'S': 
        nam[nn]['Y'+name+'_NL%LPHY'] = ['.FALSE.',]
        nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LADV'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LGPINGP'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LQM'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LREQOUT'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%LT1'] = ['.TRUE.',]
        nam[nn]['Y'+name+'_NL%NCOUPLING'] = ['0',]
        nam[nn]['Y'+name+'_NL%NREQIN'] = ['0',]
      else: 
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
  
  # Update due to MUSC/ALADIN config
  nn = 'NAMDYN'
  nam[nn]['LADVF'] = ['.FALSE.',]
  nam[nn]['NSITER'] = ['0',]

  # Update NAMDIM
  nn='NAMDIM'
  try:
    del(nam[nn])
  except KeyError:
    pass      
  nam[nn] = {}
  nam[nn]['NPROMA'] = ['50',]

  # Update NAMDYNA
  nn='NAMDYNA'
  try:
    del(nam[nn])
  except KeyError:
    pass      
  nam[nn] = {}
  nam[nn]['NDLNPR'] = ['0',]

# Update NAMIO_SERV
  nn='NAMIO_SERV'
  nam[nn]['NIO_SERV_BUF_MAXSIZE'] = ['0']
  nam[nn]['NPROCESS_LEVEL'] = ['0']
  nam[nn]['NPROC_IO'] = ['0']

  # Update NAMCT1
  nn='NAMCT1'
  del(nam[nn])
  nam[nn] = {}
  nam[nn]['LRFILAF'] = ['.FALSE.',]
  nam[nn]['N1HIS']=['0',]
  nam[nn]['N1ISP']=['0',]
  nam[nn]['N1POS']=['1',]
  nam[nn]['N1RES']=['0',]
  nam[nn]['N1SDI']=['0',]
  nam[nn]['N1SFXHIS']=['0',]

  # Update NAMCT0
  nn = 'NAMCT0'
  nam['NAMCT0'] = {}
  nam[nn]['LOPT_SCALAR']=['.TRUE.',]
  nam[nn]['LSCREEN_OPENMP']=['.FALSE.',]
  nam[nn]['LTWOTL']=['.TRUE.',]
  nam[nn]['LNHDYN']=['.FALSE.',]
  nam[nn]['LPC_FULL']=['.FALSE.',]
  nam[nn]['LPC_NESC']=['.FALSE.',]
  nam[nn]['LPC_NESCT']=['.FALSE.',]
  nam[nn]['LPC_NESCV']=['.FALSE.',]
  nam[nn]['LPC_CHEAP']=['.FALSE.',]
  nam[nn]['LSPRT']=['.TRUE.',]
  nam[nn]['LAROME']=['.TRUE.',]
  nam[nn]['CNPPATH']=["'.'",]
  nam[nn]['LREGETA']=['.TRUE.',]
  nam[nn]['CFPATH']=["'ICMSH'",]
  nam[nn]['LFDBOP']=['.FALSE.',]
  nam[nn]['LFBDAP']=['.TRUE.',]
  nam[nn]['LSFORC']=['.TRUE.',]
  nam[nn]['NFRHIS'] = ['1',]
  nam[nn]['NFPOS'] = ['0',]
  nam[nn]['NFRPOS'] = ['1',]
  nam[nn]['NFRISP'] = ['1',]
  nam[nn]['NFRSDI'] = ['1',]
  nam[nn]['NHISTS(0)'] = ['1',]
  nam[nn]['NPRINTLEV'] = ['2',]

  # Update NAMXFU
  nn =  'NAMXFU'
  for param in nam[nn].keys():
    if param[0] == 'L': nam[nn][param] = ['.FALSE.',]

# Update NAMPAR1
  nn = 'NAMPAR1'
  del(nam[nn])
  nam[nn]={}
  nam[nn]['LEQ_REGIONS']=['.FALSE.',]
  nam[nn]['LSLONDEM']=['.TRUE.',]
  nam[nn]['LSPLIT']=['.FALSE.',]
  nam[nn]['LSYNC_SLCOM']=['.TRUE.',]
  nam[nn]['LSYNC_TRANS']=['.TRUE.',]
  nam[nn]['L_GATHERV_WRGP']=['.FALSE.',]
  nam[nn]['NCOMBFLEN']=['1800000',]
  nam[nn]['NSTRIN']=['100',]
  nam[nn]['NSTROUT']=['1',]

# Update NAMPARAR
  nn='NAMPARAR'
  nam[nn]['CMF_CLOUD']=["'DIRE'",]
  nam[nn]['CMF_UPDRAFT']=["'EDKF'",]
  nam[nn]['LMIXUV']=['.TRUE.',]
  nam[nn]['NSWB_MNH']=['6',]

# Update NAMARG

  nn = 'NAMARG'
  nam[nn]['CNMEXP'] = ["'ARPE'",]
  nam[nn]['CUSTOP'] = ["'"+NSTOP+"'",]
  nam[nn]['TSTEP'] = [str(timestep),]

# Update NAMCFU

  nn = 'NAMCFU'
  for param in nam[nn].keys():
    nam[nn][param] = [".FALSE.",]

# Update NEMELBC0A

  nn = 'NEMELBC0A'
  nam[nn]['LESPCPL'] = [".FALSE.",]


  # -----------------------------------------------------------
  # Case specific modifications in namref
  # -----------------------------------------------------------

  attributes = {}
  for att in ['tadvh','qdvh','qtadvh','uadvh','vadvh','tadvv','qadvv','qtadvv','uadvv','vadvv','tadv','qadv','uadv','vadv','trad','forc_omega','forc_w','forc_geo','nudging_t','nudging_q','nudging_u','nudging_v','surfaceForcing','z0','ustar']:
    attributes[att] = 0

  for att in ['p_nudging_t','p_nudging_q','p_nudging_u','p_nudging_v']:
    attributes[att] = 110000.     
  for att in fin.listglobal():
    attributes[att] = fin.getglobal(att)

  if attributes['surfaceForcing'] == "surfaceFlux" :
    time_sfc = fin('sfc_sens_flx').getAxis(0)
    nt_sfc = time_sfc.shape[0]

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
  nam['NAERAD']['LRRTM'] =  ['.FALSE.',]
  if attributes['trad'] in [1,'adv']:
    nam['NAMPHY']['LRAYFM'] = ['.FALSE.',]
    nam['NAERAD']['LSRTM'] =  ['.FALSE.',]
    nam['NAERAD']['NSW'] = ['6',]
    nam['NAERAD']['NOZOCL'] = ['2',]

  # MUSC Forcing
  nn = 'NAMLSFORC'
  del(nam[nn])
  nam[nn] = {}

  i=0 # compter
  j=0 # compter
  
  if nt == 1:
      dt = 0.
  else:  
      dt = time[1]-time[0]
      
  #for param in ['LFIXRAD','LGEOST_UV_FRC','LNOWINDTEND','LQV_ADV_FRC','LQV_NUDG','LSOMEGA_FRC','LSW_FRC','LT_ADV_FRC','LT_NUDG','LUV_ADV_FRC','LUV_NUDG']:
  for param in ['LGEOST_UV_FRC','LQV_ADV_FRC','LT_ADV_FRC']:
    nam[nn][param] = ['.FALSE.',]
  nam[nn]['LMUSCLFA'] = ['.TRUE.',]
  for param in ['NGEOST_U_NUM','NGEOST_V_NUM','NQV_ADV_NUM','NT_ADV_NUM']:
    nam[nn][param] = ['-1',]

  if attributes['tadv'] == 1 or attributes['tadvh'] == 1 or attributes['tadvv'] == 1:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LT_ADV_FRC'] = ['.TRUE.',]
      nam[nn]['NT_ADV_DEB'] = [str(1+i*nt),]
      nam[nn]['NT_ADV_NUM'] = [str(nt),]
      i=i+1
      for it in range(0,nt):
           nam[nn]['NL_T_ADV_TIME(   '+str(int(it+1))+' )']=[str(int(dt*it)),]
  if attributes['qadv'] == 1 or attributes['qadvh'] == 1 or attributes['qtadvh'] == 1 or attributes['qadvv'] == 1:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LQV_ADV_FRC'] = ['.TRUE.',]
      nam[nn]['NQV_ADV_DEB'] = [str(1+i*nt),]
      nam[nn]['NQV_ADV_NUM'] = [str(nt),]
      i=i+1
      for it in range(0,nt):
           nam[nn]['NL_QV_ADV_TIME(   '+str(int(it+1))+' )']=[str(int(dt*it)),]
  if attributes['uadvh'] == 1 or attributes['uadvh'] == 1 or attributes['uadvv'] == 1 or attributes['vadvv'] == 1:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LUV_ADV_FRC'] = ['.TRUE.',]
  if attributes['forc_omega'] == 1:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LSOMEGA_FRC'] = ['.TRUE.',]
  if attributes['forc_geo'] == 1:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LGEOST_UV_FRC'] = ['.TRUE.',]
      W=7.2921e-5
      nam[nn]['RCORIO_FORC'] = [str(2.*W*math.sin(lat*math.pi/180)),]
      nam[nn]['NGEOST_U_DEB']=[str(1+nt*i),]
      nam[nn]['NGEOST_U_NUM']=[str(nt),]
      i=i+1
      nam[nn]['NGEOST_V_DEB']=[str(1+nt*i),] 
      nam[nn]['NGEOST_V_NUM']=[str(nt),]
      i=i+1
      for it in range(0,nt):
        nam[nn]['NL_GEOST_UV_TIME(   '+str(int(it+1))+' )']=[str(int(dt*it)),]
  if attributes['forc_w'] == 1:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LSW_FRC'] = ['TRUE',]  
      nam[nn]['NLSW_DEB'] = [str(1+i*nt),]
      nam[nn]['NLSW_NUM'] = [str(nt),]
      i=i+1
      for it in range(0,nt):
        nam[nn]['NL_LSW_TIME(   '+str(int(it+1))+' )']=[str(int(dt*it)),]      
  if attributes['nudging_u'] > 0. or attributes['nudging_v'] > 0.:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LUV_NUDG'] = ['.TRUE.',]
      nam[nn]['RELAX_TAUU'] = [str(float(attributes['nudging_u'])),]
      nam['NAMTOPH']['ETRELAXU'] = [str(float(attributes['p_nudging_u'])),]
  if attributes['nudging_t'] > 0.:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LT_NUDG'] = ['.TRUE.',]
      nam[nn]['RELAX_TAUT'] = [str(float(attributes['nudging_t'])),]
      nam['NAMTOPH']['ETRELAXT'] = [str(float(attributes['p_nudging_t'])),]
  if attributes['nudging_q'] > 0.:
      nam["NAMCT0"]['LSFORC']=['.TRUE.',]
      nam[nn]['LQV_NUDG'] = ['.TRUE.',]
      nam[nn]['RELAX_TAUQ'] = [str(float(attributes['nudging_q'])),]
      nam['NAMTOPH']['ETRELAXQ'] = [str(float(attributes['p_nudging_q'])),]
  if attributes['surfaceForcing'] == "surfaceFlux" :
      nam["NAMCT0"]['LSFORCS']=['.TRUE.',]
      if attributes['z0'] > 0.:
        nam[nn]['RZ0_FORC'] = [str(float(attributes['z0'])),]
      else:
        nam[nn]['RZ0_FORC'] = ['0.035',]
      nam[nn]["NSH_FORC_DEB"]=[str(int(1+j*nt_sfc)),]
      nam[nn]["NSH_FORC_NUM"]=[str(nt_sfc),]
      j=j+1
      nam[nn]["NLH_FORC_DEB"]=[str(int(1+j*nt_sfc)),]
      nam[nn]["NLH_FORC_NUM"]=[str(nt_sfc),]
      if nt_sfc == 1:
        dt_sfc = 0.
      else:  
        dt_sfc = time_sfc[1]-time_sfc[0]
      for it in range(0,nt_sfc):
         nam[nn]['NL_SH_ADV_TIME(   '+str(int(it+1))+" )"]=[str(int(dt_sfc*it)),]
         nam[nn]['NL_LH_ADV_TIME(   '+str(int(it+1))+" )"]=[str(int(dt_sfc*it)),]
      nam["NAMPHYDS"]['NSFORC']=[str(int(2*nt_sfc)),]

  nam['NAMGFL']['NGFL_FORC'] = [str(int(nt*i)),]



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
