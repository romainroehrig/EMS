#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import logging
logger = logging.getLogger(__name__)

def readarp(fin):
  """
    Read atmospheric namelist in fin and return a dictionary
  """

  f = open(fin)
  lines = f.readlines()
  f.close()

  namelist = {}

  for line in lines:
    tmp = line.strip()
    if tmp[0] == '&':
      nam_tmp = tmp[1:].strip().upper()
      namelist[nam_tmp] = {}
    elif tmp[0] == '/':
      pass
    elif tmp[0].strip() == '':
      pass	     
    else:
      toto = line.split('=')
      if len(toto) == 2:
        tmp = list(toto[1].strip().split(','))
        tmp1 = [x.strip() for x in tmp if x != '']
        param = toto[0].strip().upper()
        namelist[nam_tmp][param] = tmp1 #toto[1].split(',')[0].strip()
      elif len(toto) == 1:
        tmp = toto[0].strip().split(',')[0]
        namelist[nam_tmp][param].append(tmp)
      else:
        logger.error('case unexpected:', line)
        raise ValueError

  return namelist

def writearp(nam,fout):
  """
   Write atmospheric namelist in file fout
  """

  f = open(fout,'w')

  for nn in sorted(nam.keys()):
    f.write('&' + nn + '\n')
    for param in sorted(nam[nn].keys()):
      if param in ['CFP2DF(1)','CFP3DF(1)','CFPCFU(1)','CFPPHY(1)','CFPXFU(1)']:
        tmp = '  ' + param + '='	 
        for i,val in enumerate(nam[nn][param]):
          if i == 0:
            tmp = tmp + val + ',\n'
          else:
            tmp = tmp + '    ' + val + ',\n'
        tmp = tmp[:-1]
      else:
        tmp = '  ' + param + '='	    
        for val in nam[nn][param]:
          tmp = tmp + val + ','
      f.write(tmp + '\n')	 
     
    f.write('/\n')


  f.close()

def readsurfex(fin):
  """
    Read namelist SURFEX in fin and return a dictionary
  """

  f = open(fin)
  lines = f.readlines()
  f.close()

  namelist = {}

  for line in lines:
    tmp0 = line.strip().split('=')
    #print tmp0
    tmp = line.strip()
    #print tmp
    if len(tmp0) == 1:
      if tmp[0] == '&':
        nam_tmp = tmp[1:].strip().upper()
        namelist[nam_tmp] = {}
      elif tmp[0] == '/':
        pass
      elif tmp[0].strip() == '':
        pass	     
      else:
        toto = line.split('=')
        if len(toto) == 2:
          tmp = list(toto[1].strip().split(','))
          tmp1 = [x.strip() for x in tmp if x != '']
          param = toto[0].strip().upper()
          namelist[nam_tmp][param] = tmp1 #toto[1].split(',')[0].strip()
        elif len(toto) == 1:
          tmp = toto[0].strip().split(',')[0]
          namelist[nam_tmp][param].append(tmp)
        else:
          logger.error('case unexpected 1:', line)
          raise ValueError
    else:
      #print tmp0[0]
      tmp1 = line.strip().split()
      if tmp1[0][0] == '&':
        nam_tmp = tmp1[0][1:].strip().upper()
        namelist[nam_tmp] = {}
        n = len(nam_tmp)
        #print nam_tmp,n,line[n+1:]
        toto = line[n+1:].split('=')
        if len(toto) == 2:
          tmp = list(toto[1].strip().split(','))
          tmp1 = [x.strip() for x in tmp if x != '']
          param = toto[0].strip().upper()
          namelist[nam_tmp][param] = tmp1
        elif len(toto) == 1:
          tmp = toto[0].strip().split(',')[0]
          namelist[nam_tmp][param].append(tmp)
        else:
          logger.error('case unexpected 2:', line)
          raise ValueError
      else:
        toto = line.split('=')
        if len(toto) == 2:
          tmp = list(toto[1].strip().split(','))
          tmp1 = [x.strip() for x in tmp if x != '']
          param = toto[0].strip().upper()
          namelist[nam_tmp][param] = tmp1 #toto[1].split(',')[0].strip()
        elif len(toto) == 1:
          tmp = toto[0].strip().split(',')[0]
          namelist[nam_tmp][param].append(tmp)
        else:
          logger.error('case unexpected 3:', line)
          raise ValueError

  return namelist


def writesurfex(nam,fout):
  """
   Write namelist SURFEX in file fout
  """

  f = open(fout,'w')

  for nn in sorted(nam.keys()):
    f.write('&' + nn + '\n')
    for param in sorted(nam[nn].keys()):
      if len(param) <= 16:	    
        tmp = '  ' + param + ' '*(16-len(param)) +'= '	    
      else:
        tmp = '  ' + param + ' = '
      for val in nam[nn][param]:
#        print nn, param, val
        tmp = tmp + val + ','
      f.write(tmp + '\n')
    f.write('/\n')

  f.close()
