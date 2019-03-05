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


lplot2D = True
lplot2Dbias = True
lplotTS = True
lplotInit = False
lplotAvgP = True

try:
  case = sys.argv[1]
except IndexError:
  print 'Use this program as atlas.py CASE [SUBCASE]'
  sys.exit()
except:
  raise

try:
  subcases = [sys.argv[2],]
except IndexError:
  print 'You did not provide any subcase for {0}'.format(case)
  print 'You can do it using the following command: atlas.py CASE [SUBCASE]'
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

var2compute = config.var2compute
if not(os.path.exists('tmp')):
  os.makedirs('tmp')

names = variables_info.names
units = variables_info.units
coefs = variables_info.coefs

plot2D = config.plot2D
try:
  plot2Dbias = config.plot2Dbias
except AttributeError:
  lplot2Dbias = False
except:
  raise
plotTS = config.plotTS
plotInitGV = config.plotInitGV
plotInitLL = config.plotInitLL
plotAvgP = config.plotAvgP

for subcase in subcases:

  fin = {}
  varnames = {}
  coefsimu = {}
  lines = {}
  for k in simulations.sim2plot:

    if not(os.path.isfile(simulations.files[case][subcase][k])):
      print '{0} does not exist'.format(simulations.files[case][subcase][k])
      sys.exit()

    fin[k] = 'tmp/{0}_{1}_{2}.nc'.format(k,case,subcase)
    diag_simu.prepare(simulations.files[case][subcase][k],fin[k],var2compute)
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

  reptex = './atlas/{0}/{1}/{2}/TEX/'.format(simulations.name_atlas,case,subcase)
  if not(os.path.exists(reptex)):
    os.makedirs(reptex)


###########################################################################
# 2D plots

  if lplot2D:
    def plotloc(var,levels=None,extend='neither',lev='zh',firstwhite=False,cmap=plt.cm.RdBu.reversed()):

      print 'plot2D:', var    

      plotdico = {}
      for vv in ['ymin','ymax','yname','levunits','dtlabel','xname','lgrid','figsize']:
        try:
            plotdico[vv] = plot2D[vv]    
        except KeyError:
            pass
        except:
            raise           
      plotdico['lev'] = lev
      plotdico['tmin'] = tmin
      plotdico['tmax'] = tmax
      plotdico['minmax'] = True
      plotdico['units'] = units[var]
      plotdico['title'] = '{0} ({1})'.format(names[var],units[var])
      if levels is not None:
        plotdico['levels'] = levels
      plotdico['extend'] = extend
      plotdico['firstwhite'] = firstwhite
      plotdico['cmap'] = cmap
      plotdico['namefig'] = {}
      for sim in fin.keys():
        plotdico['namefig'][sim] = '{0}/2D_{2}_{1}.png'.format(repout,sim,var)

      varloc,coefloc = plotMUSC.getvarnames(var,fin,varnames,coefsimu,coef=coefs[var])
      plotMUSC.plot2D(fin,varloc,coef=coefloc,error=error,**plotdico)


    for var in plot2D['var2plot'].keys():
      #print var, plot2D['var2plot'][var]
      plotloc(var,**plot2D['var2plot'][var])

###########################################################################
# 2D bias plots

  if lplot2Dbias:
    def plotloc(var,levels=None,extend='neither',lev='zh',firstwhite=False,cmap=plt.cm.RdBu.reversed(),lbias=True,refdataset=None):

      print 'plot2Dbias:', var    

      plotdico = {}
      for vv in ['ymin','ymax','yname','levunits','dtlabel','xname','lgrid','figsize']:
        try:
            plotdico[vv] = plot2Dbias[vv]    
        except KeyError:
            pass
        except:
            raise           
      plotdico['lev'] = lev
      plotdico['tmin'] = tmin
      plotdico['tmax'] = tmax
      plotdico['minmax'] = True
      plotdico['units'] = units[var]
      plotdico['title'] = '{0} ({1}) - Bias'.format(names[var],units[var])
      if levels is not None:
        plotdico['levels'] = levels
      plotdico['extend'] = extend
      plotdico['firstwhite'] = firstwhite
      plotdico['cmap'] = cmap
      plotdico['lbias'] = lbias
      plotdico['refdataset'] = refdataset
      plotdico['namefig'] = {}
      for sim in fin.keys():
        plotdico['namefig'][sim] = '{0}/2Dbias_{2}_{1}.png'.format(repout,sim,var)

      varloc,coefloc = plotMUSC.getvarnames(var,fin,varnames,coefsimu,coef=coefs[var])
      plotMUSC.plot2D(fin,varloc,coef=coefloc,error=error,**plotdico)


    for var in plot2Dbias['var2plot'].keys():
      #print var, plot2D['var2plot'][var]
      plotloc(var,**plot2Dbias['var2plot'][var])

  ###########################################################################
  # timeseries

  if lplotTS:
    def plotloc(var,ymin=None,ymax=None):
 
      print 'plot timeseries:', var
   
      plotdico = {}
      for vv in ['dtlabel','xname','lgrid','figsize']:
        try:
            plotdico[vv] = plotTS[vv]    
        except KeyError:
            pass
        except:
            raise             
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

    for var in plotTS['var2plot'].keys():
      #print var, plotTS['var2plot'][var]
      plotloc(var,**plotTS['var2plot'][var])

  ###########################################################################
  # Initial profiles - global view
  
  if lplotInit:
    def plotloc(var,xmin=None,xmax=None,lev='zf',init=False):

      print 'plot init global view:', var
    
      plotdico = {}
      for vv in ['ymin','ymax','yname','levunits','lgrid','figsize']:
        try:
            plotdico[vv] = plotInitGV[vv]    
        except KeyError:
            pass
        except:
            raise           
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
      #print var, plotInitGV['var2plot'][var]
      plotloc(var,**plotInitGV['var2plot'][var])

  ###########################################################################
  # Initial profiles - zoom on the lowest layers

    def plotloc(var,xmin=None,xmax=None,lev='zf',init=False):

      print 'plot init zoom:', var
    
      plotdico = {}
      for vv in ['ymin','ymax','yname','levunits','lgrid','figsize']:
        try:
            plotdico[vv] = plotInitLL[vv]    
        except KeyError:
            pass
        except:
            raise            
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
      #print var, plotInitLL['var2plot'][var]
      plotloc(var,**plotInitLL['var2plot'][var])

  ###########################################################################
  # Average profiles over time slices

  if lplotAvgP:
    for avg in plotAvgP.keys():

      def plotloc(var,xmin=None,xmax=None,lev='zf',init=False,lbias=False,refdataset=None):
   
        print 'plot profiles {0}: {1}'.format(avg,var)
    
        plotdico = {}
        for vv in ['tmin','tmax','ymin','ymax','yname','levunits','lgrid','figsize']:
          try:
              plotdico[vv] = plotAvgP[avg][vv]       
          except KeyError:
              pass
          except:
              raise
        plotdico['lev'] = lev
        plotdico['init'] = init
        plotdico['units'] = units[var]
        plotdico['title'] = '{0} ({1}) - {2}'.format(names[var],units[var],plotAvgP[avg]['title']) 
        if xmin is not None:
          plotdico['xmin'] = xmin
        if xmax is not None:
          plotdico['xmax'] = xmax
        plotdico['xname'] = units[var]
        plotdico['lines'] = plotMUSC.getlines(fin,lines)
        plotdico['lbias'] = lbias
        plotdico['refdataset']=refdataset
        plotdico['namefig'] = '{0}/AvgP_{2}_{1}.png'.format(repout,var,avg)

        varloc,coefloc = plotMUSC.getvarnames(var,fin,varnames,coefsimu,coef=coefs[var])
        plotMUSC.plot_profile(fin,varloc,coef=coefloc,error=error,**plotdico)

      for var in plotAvgP[avg]['var2plot'].keys():
        #print var, plotAvgP[avg]['var2plot'][var]
        plotloc(var,**plotAvgP[avg]['var2plot'][var])

  ###########################################################################
  # synthesis of encoutered errors

  for k in error.keys():
    print '-'*40
    print 'For dataset {0}, we got en error with the following variables:'.format(k)
    print '--', error[k]

  ###########################################################################
  # latex

  os.system('rm -f {0}/*'.format(reptex))
  os.system('cp tex_template/template_atlas_{1}.tex {0}/'.format(reptex,case))
  cwd = os.getcwd()
  os.chdir(reptex)
  os.system('sed s/XXXX/{0}/g template_atlas_{1}.tex > atlas_{1}.tex'.format(simulations.sim2plot[0],case))
  os.system('rm -f template_atlas_{0}.tex'.format(case))
  os.system('pdflatex -interaction=nonstopmode atlas_{0}.tex > atlas_{0}.log 2>&1'.format(case))
  os.system('cp atlas_{0}.pdf ../../../atlas_{0}_{1}.pdf'.format(case,subcase))
  os.chdir(cwd)

