#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import cdtime
import references
import simulations
import diag_simu
import os,sys

import cdms2
import MV2
import matplotlib.pyplot as plt

import math
import numpy as np
from scipy.interpolate import interp1d

def plot_metric(ax,data,metrics2combine,coef=1.,xlabel=[],ylabel='',ylim=None,yticks=None,plotlegend=False,colors=None):
  nmetrics = len(metrics2combine)
  for i in range(0,nmetrics):
    mm = metrics2combine[i]
    for k in data.keys():
      xx = [i+0.5,]
      if not(data[k][mm] is None):
        yy = [data[k][mm]*coef,]
      else:
        yy = [1.e20,]
      if colors is None:
        ax.plot(xx,yy,color=colors[k],marker='o',linestyle='',label=k)
      else:
        ax.plot(xx,yy,color=colors[k],marker='o',linestyle='',label=k)

  ax.set_xlim((0,nmetrics+0.5))
  ax.set_xticks([0.5+i for i in range(0,nmetrics)])
  ax.set_xticklabels(xlabel)
  if not(ylim is None): ax.set_ylim(ylim)
  if not(yticks is None): ax.set_yticks(yticks)
  ax.set_ylabel(ylabel) 

  if not(ylim is None): ax.spines['left'].set_bounds(ylim[0],ylim[1])
  ax.spines['right'].set_visible(False)
  ax.spines['top'].set_visible(False)
  if nmetrics == 1:
    ax.spines['bottom'].set_visible(False)
  else:
    ax.spines['bottom'].set_bounds(0.5,nmetrics-0.5)

  ax.yaxis.set_ticks_position('left')
  ax.xaxis.set_ticks_position('bottom')

  if plotlegend:
      ax.legend()



def f_rmse(ref,sim,zmin,zmax):

  zf_ref = np.array(ref[0])
  var_ref = np.array(ref[1])
  zf_sim = np.array(sim[0])
  var_sim = np.array(sim[1])

  f = interp1d(zf_sim,var_sim,fill_value='extrapolate')
  var_sim_interp = f(zf_ref)

  nlev, = zf_ref.shape
  rmse = 0.
  nn = 0
  for ilev in range(0,nlev):
      if zf_ref[ilev] >= zmin and zf_ref[ilev] <= zmax:
          nn = nn+1
          rmse = rmse + (var_sim_interp[ilev]-var_ref[ilev])*(var_sim_interp[ilev]-var_ref[ilev])
          #print zf_ref[ilev],var_sim_interp[ilev]-var_ref[ilev]

  if nn > 0:
    rmse = rmse/nn
    rmse = math.sqrt(rmse)
  else:
    print 'problem'
    sys.exit()

  return rmse


case = 'BOMEX'

var2compute = ['zcb','zct','theta_0_500','qv_0_500','max_cf']
if not(os.path.exists('tmp')):
    os.makedirs('tmp')

fin = {}
lines = {}
for k in simulations.sim2plot:
    fin[k] = 'tmp/{0}_{1}.nc'.format(k,case)
    diag_simu.prepare(simulations.files[case][k],fin[k],var2compute)
    lines[k] = simulations.lines[case][k]

for k in ['LES']: #references.files[case].keys():
    fin[k] = references.files[case][k]
    lines[k] = references.lines[case][k]

repout = './atlas/{0}/{1}/METRICS/'.format(simulations.name_atlas,case)
if not(os.path.exists(repout)):
    os.makedirs(repout)


colors = {}
for k in lines.keys():
    colors[k] = lines[k][0]

# Computing metrics hour 7
metrics = {}

tmin = cdtime.comptime(1969,6,24,0)
t1 = tmin.add(7,cdtime.Hour)
t2 = tmin.add(8,cdtime.Hour)

for k in fin.keys():
  metrics[k] = {}
  f = cdms2.open(fin[k])
  # Simple average between t1 and t2
  for var in ['zcb','zct','cc','max_cf','theta_0_500','qv_0_500','lwp']:
    if var in f.listvariables():
      tmp = f(var,time=(t1,t2))
      tmp = MV2.average(tmp,axis=0)
    else:
      tmp = None
  
    metrics[k][var] = tmp

  # for computing RMSE of profiles 
  for var  in ['theta','qv']:      
    if var in f.listvariables():
      tmp1 = f('zf',time=(t1,t2))
      if len(tmp1.shape) == 2:
       tmp1 = MV2.average(tmp1,axis=0)
      tmp2 = f(var,time=(t1,t2))
      tmp2 = MV2.average(tmp2,axis=0)    
    else:
      tmp1 = None
      tmp2 = None
  
    metrics[k][var + '_rmse'] = tmp1,tmp2

  f.close()

# Computing RMSE
kref = 'LES'
zmin = 0
zmax = 2000.
for var in ['theta','qv']:
  for k in simulations.sim2plot:
    rmse = f_rmse(metrics[kref][var+'_rmse'],metrics[k][var+'_rmse'],zmin,zmax)
    metrics[k][var+'_rmse'] = rmse
  for k in ['LES']: #references.files[case].keys():
    metrics[k][var+'_rmse'] = None


# Making the plot

fig, ax = plt.subplots(1,7,figsize=(15,5))

fig.suptitle('BOMEX metrics for 7th hour')

# Potential temperature

plot_metric(ax[0],metrics,['theta_0_500',],\
        xlabel=['0-500m',],\
        ylabel='Potential temperature (K)',\
        ylim=(297,302),\
        yticks=[297+i for i in range(0,6)],\
        colors=colors)

# Specific humidity

plot_metric(ax[1],metrics,['qv_0_500',],\
        coef=1000.,\
        xlabel=['0-500m',],\
        ylabel='Specific humidity (g kg$^{-1}$)',\
        ylim=(10,20),\
        yticks=[10+i*2 for i in range(0,6)],\
        colors=colors)

# Cloud fraction

plot_metric(ax[2],metrics,['cc','max_cf'],\
        coef=100.,\
        xlabel=['Surface', 'Max'],\
        ylabel='Cloud fraction (%)',\
        ylim=(0,30),\
        yticks=[i*5 for i in range(0,7)],\
        colors=colors)

# Cloud base/top height

plot_metric(ax[3],metrics,['zcb','zct'],\
        coef=1/1000.,\
        xlabel=['Cloud\n base', 'Cloud\n top'],\
        ylabel='Cloud base/top height (km)',\
        ylim=(0,2.5),\
        yticks=[i*0.5 for i in range(0,6)],\
        colors=colors)

# LWP

plot_metric(ax[4],metrics,['lwp',],\
        coef=1000.,\
        xlabel=['LWP',],\
        ylabel='Liquid water path (g m$^{-2}$)',\
        ylim=(0,15),\
        yticks=[i*5 for i in range(0,4)],\
        colors=colors)

# Theta RMSE

plot_metric(ax[5],metrics,['theta_rmse',],\
        xlabel=['RMSE\n 0-2000m',],\
        ylabel='Potential temperature RMSE (K)',\
        ylim=(0,2),\
        yticks=[i*0.2 for i in range(0,11)],\
        colors=colors)

# qv RMSE

plot_metric(ax[6],metrics,['qv_rmse',],\
        coef=1000.,\
        xlabel=['RMSE\n 0-2000m',],\
        ylabel='Specific humidity RMSE (g kg$^{-1}$)',\
        ylim=(0,2),\
        yticks=[i*0.2 for i in range(0,11)],\
        colors=colors,
        plotlegend=True)

plt.savefig('{0}/{1}_metrics_hour7.png'.format(repout,case))

plt.close()
