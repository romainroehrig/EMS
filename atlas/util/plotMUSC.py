import sys
import cdms2
import MV2
import cdtime
import numpy as np
import numpy.ma as ma
import math

import time as TT

import plotutils

verbose = False
lperf = False

missing = 1.e20

def plot_timeseries(filein,varname,coef=None,units='',tmin=None,tmax=None,dtlabel='1h',error=None,**kwargs):
    """
       Do a timeseries plot of varname for several MUSC files
    """

    data = {}
    time = {}
    if coef is None:
      coef = {}
      for k in filein.keys():
        coef[k] = 1.

    for k in filein.keys():
      f = cdms2.open(filein[k])
      try:
        data[k] = f(varname[k],squeeze=1)*coef[k]
        time[k] = data[k].getTime()
        kref = k
      except cdms2.error.CDMSError as e:
        data[k] = None  
        time[k] = None
        if verbose:
          print 'Variable {2} probably unknown in dataset {0} (file={1})'.format(k,filein[k],varname[k])
          print 'Raised error: cdms2.error.CDMSError', e
        if error is not None:
          if isinstance(error,dict):
            if error.has_key(k):
              error[k].append(varname[k])
            else:
              error[k] = [varname[k],]
          else:
            print 'type of error unexpected:', type(error)
            print 'error should be a dictionnary'
            sys.exit()
      except:
        raise
      f.close()

    for k in filein.keys():
      if data[k] is None:
        del(data[k])
        del(time[k])
#        data[k] = data[kref] + 1.e20  
#        time[k] = data[k].getTime()

    timeref = time[kref]

    if tmin is None:
        tmin = cdtime.reltime(timeref[0],timeref.units)
        print tmin.tocomp()
    if tmax is None:
        tmax = cdtime.reltime(timeref[-1],timeref.units)

    tt = []
    tlabels = []

    tminloc = tmin.tocomp()
    if dtlabel == '1h':
      tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

      t0 = tmin0.add(0,cdtime.Hour)
      while t0.cmp(tmax) <= 0:
        if t0.cmp(tmin) >= 0: 
          tt.append(t0.torel(timeref.units).value)
          tlabels.append('{0}'.format(t0.hour))
        t0 = t0.add(1,cdtime.Hour)
    elif dtlabel == '2h':
      tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

      t0 = tmin0.add(0,cdtime.Hour)
      while t0.cmp(tmax) <= 0:
        if t0.cmp(tmin) >= 0: 
          tt.append(t0.torel(timeref.units).value)
          tlabels.append('{0}'.format(t0.hour))
        t0 = t0.add(2,cdtime.Hour)
    elif dtlabel == '6h':
      tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

      t0 = tmin0.add(0,cdtime.Hour)
      while t0.cmp(tmax) <= 0:
        if t0.cmp(tmin) >= 0: 
          tt.append(t0.torel(timeref.units).value)
          tlabels.append('{0}'.format(t0.hour))
        t0 = t0.add(6,cdtime.Hour)  
    elif dtlabel == '10d':
      tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

      t0 = tmin0.add(0,cdtime.Hour)
      while t0.cmp(tmax) <= 0:
        if t0.cmp(tmin) >= 0: 
          tt.append(t0.torel(timeref.units).value)
          tlabels.append('{0}/{1}'.format(t0.month,t0.day))
        if t0.day == 1:
            t0 = cdtime.comptime(t0.year,t0.month,10,0)
        elif t0.day == 10:
            t0 = cdtime.comptime(t0.year,t0.month,20,0)
        elif t0.day == 20:
            if t0.month == 12:
              t0 = cdtime.comptime(t0.year+1,1,1,0)
            else:
              t0 = cdtime.comptime(t0.year,t0.month+1,1,0)
        else:
            print 't0 unexpected',t0
    else:
      print 'dtlabel={} not coded yet'.format(dtlabel)
      sys.exit()

    tlabels = tt,tlabels

    for k in data.keys(): #filein.keys():
      nt, = time[k].shape
      for it in range(0,nt):
         time[k][it] = cdtime.reltime(time[k][it],time[k].units).torel(timeref.units).value
      time[k] = np.array(time[k][:])
      data[k] = np.array(data[k][:])
      data[k] = np.ma.masked_where(data[k][:] == missing,data[k][:])

    plotutils.plot1D(time,data,\
            xmin = tmin.torel(timeref.units).value,\
            xmax = tmax.torel(timeref.units).value,\
            xlabels=tlabels,\
            **kwargs)


def plot_profile(filein,varname,lines=None,coef=None,units='',lev=None,levunits='km',tt=None,tmin=None,tmax=None,init=False,t0=False,lbias=False,refdataset=None,error=None,**kwargs):
    """
       Do a profile plot of varname for several MUSC files
    """

    if lperf:
      TT0 = TT.time()

    data = {}
    level = {}
    if coef is None:
      coef = {}
      for k in filein.keys():
        coef[k] = 1.
    
    if lev is None:
      lev = {}
      for k in filein.keys():
        lev[k] = 'zf'
    if isinstance(lev,str):
      levloc = lev
      lev = {}
      for k in filein.keys():
        lev[k] = levloc

    for i,k in enumerate(filein.keys()):
#      print i, k, filein[k]
      f = cdms2.open(filein[k])

      try:
        time = f[varname[k]].getTime()
#      print time
        if tmin is None:
          tmin = cdtime.reltime(time[0],time.units)
        if tmax is None:
          tmax = cdtime.reltime(time[-1],time.units)

        if t0:
          tmin = cdtime.reltime(time[0],time.units)
          tmax = cdtime.reltime(time[1],time.units)
          tmax = tmax.add(-1.,cdtime.Second)

#      print tmin.tocomp(), tmax.tocomp()

        if tt is not None:
          if isinstance(tt,int):
            tmin = cdtime.reltime(time[tt],time.units)
            tmax = cdtime.reltime(time[tt],time.units)
          else:
            if tt.cmp(tmin) <= 0:
              print 'tt={0} is lower than tmin={1}'.formtat(tt.tocomp(),tmin.tocomp())
            if tt.cmp(tmax) > 0:
              print 'tt={0} is greatet than tmax={1}'.formtat(tt.tocomp(),tmax.tocomp())
            nt, = time.shape
            mini = 1.e20
            ii = 1.e20
            ttloc = tt.torel(time.units)
            for it in range(0,nt):
              tloc = cdtime.reltime(time[it],time.units)
              tmp = math.fabs(ttloc.value-tloc.value)
              if tmp < mini:
                mini = tmp
                ii = it
#        dt = time[1]-time[0]
#        if time.units[:5] == 'hours':
#          tmin = tt.add(-dt/2.,cdtime.Hour)
#          tmax = tt.add(dt/2.,cdtime.Hour)
#        elif time.units[:7] == 'seconds':
#          tmin = tt.add(-dt/2.,cdtime.Second)
#          tmax = tt.add(dt/2.,cdtime.Second)

        data[k] = MV2.average(f(varname[k],time=(tmin,tmax))*coef[k],axis=0)
        if lev[k] == 'zf':
          level[k] = f(lev[k])
        elif lev[k] == 'zh':
          try:
            level[k] = f(lev[k])
          except:
            try:
              level[k] = f('zf')
            except:
              raise
        elif lev[k] == 'pf':
          level[k] = f(lev[k])
        elif lev[k] == 'ph':
          try:
            level[k] = f(lev[k])
          except:
            try:
              level[k] = f('pf')
            except:
              raise          
        if len(level[k].shape) == 2:
          level[k] = MV2.average(level[k](time=(tmin,tmax)),axis=0)
        kref = k
      except cdms2.error.CDMSError as e:
        data[k] = None
        level[k] = None   
        f.close()        
        if verbose:
          print 'Variable {2} probably unknown in dataset {0} (file={1})'.format(k,filein[k],varname[k])
          print 'Raised error: cdms2.error.CDMSError', e
        if error is not None:
          if isinstance(error,dict):
            if error.has_key(k):
              error[k].append(varname[k])
            else:
              error[k] = [varname[k],]
          else:
            print 'type of error unexpected:', type(error)
            print 'error should be a dictionnary'
            sys.exit()
      except AttributeError as e:
        data[k] = None
        level[k] = None
        f.close()          
        if verbose:
          print 'Probably no time axis for variable {2} in dataset {0} (file={1})'.format(k,filein[k],varname[k])
          print 'Raised error: AttributeError', e
        if error is not None:
          if isinstance(error,dict):
            if error.has_key(k):
              error[k].append(varname[k])
            else:
              error[k] = [varname[k],]
          else:
            print 'type of error unexpected:', type(error)
            print 'error should be a dictionnary'
            sys.exit()          
      except:
        raise

      f.close()

      if lperf:
        TT1 = TT.time()
        print 'file', filein[k],':', TT1-TT0, 's'
        TT0 = TT.time()

    if lperf:
        TT1 = TT.time()
        print 'Part 1:', TT1-TT0, 's'
        TT0 = TT.time()

    for i,k in enumerate(filein.keys()):

     if data[k] is None:
        del(data[k])
        del(level[k])
#        data[k] = data[kref]*0. + 1.e20
#        level[k] = level[kref]*0. + 1.e20
     else:
      if lev[k] in ['zf','zh']:
        if levunits == 'm':
          pass
        elif levunits == 'km':
          level[k] = level[k]/1000.
        else:
          print 'levunits={0} for lev={1} not coded yet'.format(levunits,lev[k])
          sys.exit()
      elif lev[k] in ['pf','ph']:
        if levunits == 'Pa':
          pass
        elif levunits == 'hPa':
          level[k] = level[k]/100.
        else:
          print 'levunits={} for lev=pf not coded yet'.format(levunits)
          sys.exit()
      else:
        print 'lev={} not coded yet'.format(lev[k])
        sys.exit()

    if lperf:
        TT1 = TT.time()
        print 'Part 2:', TT1-TT0, 's'
        TT0 = TT.time()

    if init:
      f = cdms2.open(filein[kref])
      data['init'] = f(varname[kref])[0,:]*coef[k]
      tmp = f(lev[kref])
      if len(tmp.shape) == 2:
        level['init'] = tmp[0,:]
      elif len(tmp.shape) == 1:
        level['init'] = tmp[:]
      else:
        print 'level shape unexpected:', tmp.shape
        sys.exit()
      f.close()

      if lev[kref] in ['zf','zh']:
        if levunits == 'm':
          pass
        elif levunits == 'km':
          level['init'] = level['init']/1000.
        else:
          print 'levunits={0} for lev={1} not coded yet'.format(levunits,lev[kref])
          sys.exit()
      elif lev[kref] in ['pf','ph']:
        if levunits == 'Pa':
          pass
        elif levunits == 'hPa':
          level['init'] = level['init']/100.
        else:
          print 'levunits={} for lev=pf not coded yet'.format(levunits)
          sys.exit()
      else:
        print 'lev={} not coded yet'.format(lev[kref])
        sys.exit()

      if lines is None:
        lines = {}
      lines['init'] = 'k--'

      if lperf:
        TT1 = TT.time()
        print 'Part init:', TT1-TT0, 's'
        TT0 = TT.time()


    for k in data.keys():
      level[k] = np.array(level[k](squeeze=1))
      data[k] = np.array(data[k](squeeze=1))
      #print k, data[k].shape, level[k].shape

    if lbias:
        if refdataset is None:
            print 'please provide reference dataset (keyword refdataset) to compute bias)'
            sys.exit()
        else: 
            for k in data.keys():
              if not(k == refdataset):
                data[k] = data[k]-data[refdataset]
            data[refdataset] = data[refdataset]*0.


    if lperf:
        TT1 = TT.time()
        print 'Part 3:', TT1-TT0, 's'
        TT0 = TT.time()

    plotutils.plot1D(data,level,lines=lines,**kwargs)

    if lperf:
        TT1 = TT.time()
        print 'Plotting:', TT1-TT0, 's'
        TT0 = TT.time()
    

def plot2D(filein,varname,coef=None,units='',lev=None,levunits=None,tmin=None,tmax=None,dtlabel='1h',namefig=None,lbias=False,refdataset=None,error=None,**kwargs):
    """
       Do a 2D plot of varname for several MUSC file
    """

    title0 = kwargs['title']

    data = {}
    levax = {}
    time = {}
    timeax = {}
    if coef is None:
      coef = {}
      for k in filein.keys():
        coef[k] = 1.
    elif isinstance(coef,int) or isinstance(coef,float):
      tmp = coef
      coef = {}
      for k in filein.keys():
        coef[k] = tmp
    
    if lev is None:
      lev = {}
      levunits = {}
      for k in filein.keys():
        lev[k] = 'zh'
        levunits[k] = 'km'
    elif isinstance(lev,str):
      tmp = lev
      lev = {}
      for k in filein.keys():
        lev[k] = tmp

    if levunits is None:
      levunits = {}
      for k in filein.keys():
        if lev[k] == 'zh':
          levunits[k] = 'km'
        elif lev[k] in['ph','pf']:
          levunits[k] = 'hPa'
        else:
          print 'lev={} not coded yet'.format(lev[k])
          sys.exit()
    elif isinstance(levunits,str):
      tmp = levunits
      levunits = {}
      for k in filein.keys():
        levunits[k] = tmp

    datasets = filein.keys()
    if lbias:
      if refdataset is None:
          print 'please provide reference dataset (keyword refdataset) to compute bias)'
          sys.exit()
      else: 
          f = cdms2.open(filein[refdataset])
          dataref = f(varname[refdataset],squeeze=1)*coef[refdataset]
          f.close()
          tmp = []
          for dd in datasets:
              if not(dd == refdataset):
                  tmp.append(dd)
          datasets = tmp


    for k in datasets: #filein.keys():
      #print k
      f = cdms2.open(filein[k])
      try:
        data[k] = f(varname[k],squeeze=1)*coef[k]
        if lbias:
            nt,nlev = data[k].shape
            tmp = data[k].getTime()
            data[k] = data[k]-dataref[0:nt,:]
            data[k].setAxis(0,tmp)
      except cdms2.error.CDMSError as e:
        data[k] = None
        f.close()          
        if verbose:
          print 'Variable {2} probably unknown in dataset {0} (file={1})'.format(k,filein[k],varname[k])
          print 'Raised error: cdms2.error.CDMSError', e
        if error is not None:
          if isinstance(error,dict):
            if error.has_key(k):
              error[k].append(varname[k])
            else:
              error[k] = [varname[k],]
          else:
            print 'type of error unexpected:', type(error)
            print 'error should be a dictionnary'
            sys.exit()            
      except:
        raise
      
      if data[k] is not None:
        try:
          levax[k] = f(lev[k],squeeze=1)
        except:
          if lev[k] == 'zh':
            levax[k] = f('zf')
          if lev[k] == 'ph':
            levax[k] = f('pf')            
        f.close()

        if lev[k] == 'zh':
          if levunits[k] == 'm':
            pass
          elif levunits[k] == 'km':
             levax[k] = levax[k]/1000.
          else:
            print 'levunits={} for lev=zh not coded yet'.format(levunits[k])
            sys.exit()
        elif lev[k] == 'ph':
          if levunits[k] == 'Pa':
            pass
          elif levunits[k] == 'hPa':
            levax[k] = levax[k]/100.
          else:
            print 'levunits={} for lev=ph not coded yet'.format(levunits[k])
            sys.exit()
        else:
          print 'lev={} not coded yet'.format(lev)
          sys.exit()
     
        time[k] = data[k].getTime()

      
        if len(levax[k].shape) == 2:
          nt,nlev = levax[k].shape
          timeax[k] = np.tile(time[k][:],(nlev,1))
          X = timeax[k]
          Y = np.transpose(levax[k])
        else:
          nlev, = levax[k].shape
          nt, = time[k].shape
          timeax[k] = np.tile(time[k][:],(nlev,1))
          levax[k] = np.tile(levax[k],(nt,1))
          X = np.array(timeax[k][:])
          Y = np.transpose(levax[k][:])

        if tmin is None:
          tmin = cdtime.reltime(time[k][0],time.units)
        if tmax is None:
          tmax = cdtime.reltime(time[k][-1],time.units)

        tt = []
        tlabels = []

        tminloc = tmin.tocomp()
        if dtlabel == '1h':
          tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

          t0 = tmin0.add(0,cdtime.Hour)
          while t0.cmp(tmax) <= 0:
            if t0.cmp(tmin) >= 0: 
              tt.append(t0.torel(time[k].units).value)
              tlabels.append('{0}'.format(t0.hour))
            t0 = t0.add(1,cdtime.Hour)
        elif dtlabel == '2h':
          tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

          t0 = tmin0.add(0,cdtime.Hour)
          while t0.cmp(tmax) <= 0:
            if t0.cmp(tmin) >= 0: 
              tt.append(t0.torel(time[k].units).value)
              tlabels.append('{0}'.format(t0.hour))
            t0 = t0.add(2,cdtime.Hour)
        elif dtlabel == '6h':
          tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

          t0 = tmin0.add(0,cdtime.Hour)
          while t0.cmp(tmax) <= 0:
            if t0.cmp(tmin) >= 0: 
              tt.append(t0.torel(time[k].units).value)
              tlabels.append('{0}'.format(t0.hour))
            t0 = t0.add(6,cdtime.Hour) 
        elif dtlabel == '10d':
          tmin0 = cdtime.comptime(tminloc.year,tminloc.month,tminloc.day,tminloc.hour)

          t0 = tmin0.add(0,cdtime.Hour)
          while t0.cmp(tmax) <= 0:
            if t0.cmp(tmin) >= 0: 
              tt.append(t0.torel(time[k].units).value)
              tlabels.append('{0}/{1}'.format(t0.month,t0.day))
            if t0.day == 1:
              t0 = cdtime.comptime(t0.year,t0.month,10,0)
            elif t0.day == 10:
              t0 = cdtime.comptime(t0.year,t0.month,20,0)
            elif t0.day == 20:
              if t0.month == 12:
                t0 = cdtime.comptime(t0.year+1,1,1,0)
              else:
                t0 = cdtime.comptime(t0.year,t0.month+1,1,0)
            else:
              print 't0 unexpected',t0
            
        else:
          print 'dtlabel={} not coded yet'.format(dtlabel)
          sys.exit()

        tlabels = tt,tlabels

        if isinstance(namefig,str):
            tmp = k + '_' + namefig
        elif isinstance(namefig,dict):
            tmp = namefig[k]
        elif namefig is None:
            tmp = None
        else:
            print 'namefig type unexpected:', namefig
            sys.exit()

        kwargs['title'] = '{0} - {1}'.format(title0,k)

        plotutils.plot2D(X,Y,ma.transpose(data[k]),\
            xmin = tmin.torel(time[k].units).value,\
            xmax = tmax.torel(time[k].units).value,\
            xlabels=tlabels,\
            namefig=tmp,\
            **kwargs)

def getvarnames(var,fin,varnames,coefs,coef=1):
  varloc = {}
  for sim in fin.keys():
    try:
      varloc[sim] = varnames[sim][var]
    except:
      varloc[sim] = var

  coefloc = {}
  for sim in fin.keys():
    try:
      coefloc[sim] = coefs[sim][var]
    except:
      coefloc[sim] = coef

  return varloc,coefloc

def getlines(fin,lines):
  ll = ['r','g','b']
  lineloc = {}
  i = 0
  for sim in fin.keys():
    try:
      lineloc[sim] = lines[sim]
    except:
      lineloc[sim] = ll[i]
      i = i + 1

  return lineloc
