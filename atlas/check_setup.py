#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os, sys
sys.path = ['./config/','./util/'] + sys.path
import importlib

import cdtime
import matplotlib as plt # for colormaps

import plotMUSC
import diag_simu

import cases
import references
import simulations
import variables_info

lplotTS = True
lplotInit = True

try:
  case = sys.argv[1]
except IndexError:
  print 'Use this program as check_setup.py CASE [SUBCASE]'
  sys.exit()
except:
  raise

try:
  subcases = [sys.argv[2],]
except IndexError:
  print 'You did not provide any subcase for {0}'.format(case)
  print 'You can do it using the following command: check_setup.py CASE [SUBCASE]'
  print 'All subcases for {0} will be performed, namely:'.format(case), cases.subcases[case]
  subcases = cases.subcases[case]
except:
  raise

if os.path.isfile('./config/config_{0}.py'.format(case)):
  config = importlib.import_module('config_{0}'.format(case))
else:
  print './config/config_{0}.py does not exist'.format(case)
  sys.exit()

error = {}

tmin = config.tmin
tmax = config.tmax

names = variables_info.names
units = variables_info.units
coefs = variables_info.coefs

plotTS = config.plotTS
plotInitGV = config.plotInitGV
plotInitLL = config.plotInitLL

for subcase in subcases:

  fin = {}
  varnames = {}
  coefsimu = {}
  lines = {}
  for k in simulations.sim2plot:

    if not(os.path.isfile(simulations.files[case][subcase][k])):
      print '{0} does not exist'.format(simulations.files[case][subcase][k])
      sys.exit()

    fin[k] = simulations.files[case][subcase][k]
    varnames[k] = simulations.varnames[k]
    coefsimu[k] = simulations.coefs[k]
    lines[k] = simulations.lines[k]

  for k in references.files[case][subcase].keys():

    if not(os.path.isfile(references.files[case][subcase][k])):
      print '{0} does not exist'.format(references.files[case][subcase][k])
      sys.exit()

    fin[k] = references.files[case][subcase][k]
    varnames[k] = references.varnames[case][subcase][k]
    coefsimu[k] = references.coefs[case][subcase][k]
    lines[k] = references.lines[case][subcase][k]

  repout = './atlas/{0}/{1}/{2}/PNG/'.format(simulations.name_atlas,case,subcase)
  if not(os.path.exists(repout)):
    os.makedirs(repout)

  reptex = './atlas/{0}/{1}/{2}/TEX_init/'.format(simulations.name_atlas,case,subcase)
  if not(os.path.exists(reptex)):
    os.makedirs(reptex)


  ###########################################################################
  # timeseries

  if lplotTS:
    def plotloc(var,ymin=None,ymax=None):
 
      print 'plot timeseries:', var
   
      plotdico = {}
      for vv in ['dtlabel','xname']:
        plotdico[vv] = plotTS[vv]    
      plotdico['tmin'] = tmin
      plotdico['tmax'] = tmax
      plotdico['units'] = units[var]
      plotdico['title'] = '{0} ({1})'.format(names[var],units[var])
      if ymin is not None:
        plotdico['ymin'] = ymin
      if ymax is not None:
        plotdico['ymax'] = ymax
      plotdico['yname'] = units[var]
      plotdico['lines'] = plotMUSC.getlines(fin,lines)
      plotdico['namefig'] = '{0}/TS_{1}.png'.format(repout,var)

      varloc,coefloc = plotMUSC.getvarnames(var,fin,varnames,coefsimu,coef=coefs[var])
      plotMUSC.plot_timeseries(fin,varloc,coef=coefloc,error=error,**plotdico)

    for var in ['tsurf','shf','lhf','ustar']:
        if plotTS['var2plot'].has_key(var):
          plotloc(var,**plotTS['var2plot'][var])
        else:
          plotloc(var)

  ###########################################################################
  # Initial profiles - global view
  
  if lplotInit:
    def plotloc(var,xmin=None,xmax=None,lev='zf',init=False):

      print 'plot init global view:', var
    
      plotdico = {}
      for vv in ['ymin','ymax','yname','levunits']:
        plotdico[vv] = plotInitGV[vv]
      plotdico['lev'] = lev
      plotdico['units'] = units[var]
      plotdico['title'] = '{0} ({1}) - First timestep'.format(names[var],units[var]) 
      if xmin is not None:
        plotdico['xmin'] = xmin
      if xmax is not None:
        plotdico['xmax'] = xmax
      plotdico['xname'] = units[var]
      plotdico['lines'] = plotMUSC.getlines(fin,lines)
      plotdico['namefig'] = '{0}/initGV_{1}.png'.format(repout,var)

      varloc,coefloc = plotMUSC.getvarnames(var,fin,varnames,coefsimu,coef=coefs[var])
      plotMUSC.plot_profile(fin,varloc,coef=coefloc,t0=True,error=error,**plotdico)

    for var in plotInitGV['var2plot'].keys():
      plotloc(var,**plotInitGV['var2plot'][var])

  ###########################################################################
  # Initial profiles - zoom on the lowest layers

    def plotloc(var,xmin=None,xmax=None,lev='zf',init=False):

      print 'plot init zoom:', var
    
      plotdico = {}
      for vv in ['ymin','ymax','yname','levunits']:
        plotdico[vv] = plotInitLL[vv]    
      plotdico['lev'] = lev
      plotdico['units'] = units[var]
      plotdico['title'] = '{0} ({1}) - First timestep'.format(names[var],units[var]) 
      if xmin is not None:
        plotdico['xmin'] = xmin
      if xmax is not None:
        plotdico['xmax'] = xmax
      plotdico['xname'] = units[var]
      plotdico['lines'] = plotMUSC.getlines(fin,lines)
      plotdico['namefig'] = '{0}/initLL_{1}.png'.format(repout,var,case)

      varloc,coefloc = plotMUSC.getvarnames(var,fin,varnames,coefsimu,coef=coefs[var])
      plotMUSC.plot_profile(fin,varloc,coef=coefloc,t0=True,error=error,**plotdico)

    for var in plotInitLL['var2plot'].keys():
      plotloc(var,**plotInitLL['var2plot'][var])

  ###########################################################################
  # synthesis of encoutered errors

  for k in error.keys():
    print '-'*40
    print 'For dataset {0}, we got en error with the following variables:'.format(k)
    print '--', error[k]

  ###########################################################################
  # latex

  os.system('rm -f {0}/*'.format(reptex))
  os.system('cp tex_template/template_check_setup.tex {0}/check_setup.tex'.format(reptex))
  cwd = os.getcwd()
  os.chdir(reptex)
  os.system('pdflatex -interaction=nonstopmode check_setup.tex')
  os.system('cp check_setup.pdf ../../../check_setup_{0}_{1}.pdf'.format(case,subcase))
  os.chdir(cwd)

