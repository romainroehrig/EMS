import numpy as np
import numpy.ma as ma
#import matplotlib
#matplotlib.rcParams['text.usetex'] = True
#matplotlib.rcParams['text.latex.unicode'] = True
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from copy import copy

def plot1D(x,y,lines=None,title='',xname='',yname='',xlabels=None,ylabels=None,xmin=None,xmax=None,ymin=None,ymax=None,namefig=None,lgrid=False,figsize=None):
    """
       Make a simple 1D plot
    """

    if not(figsize is None):
      plt.figure(figsize=figsize)

    if lines is None:
      for k in x.keys():
        cs = plt.plot(x[k],y[k],label=k)
    else:
      for k in x.keys():
        if lines.has_key(k):
          cs = plt.plot(x[k],y[k],lines[k],label=k)
        else:
          cs = plt.plot(x[k],y[k],label=k)

    if xmin is None:
        xmin = 1.e20
        for k in x.keys():
          xmin = min(xmin,np.min(x[k]))
        #print 'xmin', xmin
    if xmax is None:
        xmax = -1.e20
        for k in x.keys():
          tmp = np.where(x[k] == 1.e20, -1.e20, x[k])
          xmax = max(xmax,np.max(tmp))
        #print 'xmax', xmax
    if ymin is None:
        ymin = 1.e20
        for k in y.keys():
          ymin = min(ymin,np.min(y[k]))
    if ymax is None:
        ymax = -1.e20
        for k in y.keys():
          ymax = max(ymax,np.max(y[k]))


    plt.xlim(xmin,xmax)
    plt.ylim(ymin,ymax)
    if xlabels is not None:
        plt.xticks(xlabels[0][:],xlabels[1][:])
    if ylabels is not None:
        plt.yticks(ylabels[0][:],ylabels[1][:])

    plt.legend(loc='best')
    plt.title(title)
    plt.xlabel(xname)
    plt.ylabel(yname)

    if lgrid:
      plt.grid(True,linestyle='--')

    if namefig is None:
        plt.show()
    else:
      plt.savefig(namefig)

    plt.close()

def plot2D(x,y,data,cmap=plt.cm.RdBu,levels=None,firstwhite=False,badcolor='darkgrey',extend='neither',title='',xname='',yname='',xlabels=None,ylabels=None,xmin=None,xmax=None,ymin=None,ymax=None,namefig=None,minmax=False,lgrid=False,figsize=None):
    """
       Make a simple 2D plot
    """

    if not(figsize is None):
      plt.figure(figsize=figsize)

    if levels is None:
      nn = 20
    else:
      nn = 255/len(levels)

    cmaploc = copy(cmap)
    cmaplist = [cmaploc(i) for i in range(cmaploc.N-1)]
    if firstwhite:
      cmaplist[nn] = (1,1,1,0)
    cmaploc = cmaploc.from_list('Custom cmap',cmaplist[nn:-nn], len(cmaplist[nn:-nn]))
    cmaploc.set_over(cmaplist[-1])
    cmaploc.set_under(cmaplist[0])
    cmaploc.set_bad(badcolor)


    if levels is None:
      cs = plt.pcolormesh(x,y,data, cmap=cmaploc)
      plt.colorbar(cs,extend=extend) 
    else:
      norm = BoundaryNorm(levels, ncolors=cmaploc.N, clip=False)
      cs = plt.pcolormesh(x,y,data, cmap=cmaploc, norm=norm)
      plt.colorbar(cs,ticks=levels,extend=extend)

    if xmin is None:
        xmin = ma.min(x)
    if xmax is None:
        xmax = ma.max(x)
    if ymin is None:
        ymin = ma.min(y)
    if ymax is None:
        ymax = ma.max(y)


    plt.xlim(xmin,xmax)
    plt.ylim(ymin,ymax)
    if xlabels is not None:
        plt.xticks(xlabels[0][:],xlabels[1][:])
    if ylabels is not None:
        plt.yticks(ylabels[0][:],ylabels[1][:])

    plt.title(title)
    plt.xlabel(xname)
    plt.ylabel(yname)

    if lgrid:
      plt.grid(True,linestyle='--')

    if minmax:
      if ymin < ymax:
        nx,ny = data.shape
        tmp = ma.where(x[:nx,:ny] >= xmin, data, 1.e20)
        tmp = ma.where(x[:nx,:ny] <= xmax, tmp, 1.e20)
        tmp = ma.where(y[:nx,:ny] >= ymin, tmp, 1.e20)
        tmp = ma.where(y[:nx,:ny] <= ymax, tmp, 1.e20)
        mini = ma.min(tmp)

        tmp = ma.where(x[:nx,:ny] >= xmin, data, -1.e20)
        tmp = ma.where(x[:nx,:ny] <= xmax, tmp, -1.e20)
        tmp = ma.where(y[:nx,:ny] >= ymin, tmp, -1.e20)
        tmp = ma.where(y[:nx,:ny] <= ymax, tmp, -1.e20)
        maxi = ma.max(tmp)
      else:
        nx,ny = data.shape
        tmp = ma.where(x[:nx,:ny] >= xmin, data, 1.e20)
        tmp = ma.where(x[:nx,:ny] <= xmax, tmp, 1.e20)
        tmp = ma.where(y[:nx,:ny] >= ymax, tmp, 1.e20)
        tmp = ma.where(y[:nx,:ny] <= ymin, tmp, 1.e20)
        mini = ma.min(tmp)

        tmp = ma.where(x[:nx,:ny] >= xmin, data, -1.e20)
        tmp = ma.where(x[:nx,:ny] <= xmax, tmp, -1.e20)
        tmp = ma.where(y[:nx,:ny] >= ymax, tmp, -1.e20)
        tmp = ma.where(y[:nx,:ny] <= ymin, tmp, -1.e20)
        maxi = ma.max(tmp)

      plt.text(xmax+(xmax-xmin)/20, ymin-(ymax-ymin)/20., "min = {:f}".format(mini), {'color': 'k', 'fontsize': 10},
         horizontalalignment='left',
         verticalalignment='center',
         clip_on=False)
      plt.text(xmax+(xmax-xmin)/20, ymin-(ymax-ymin)/10., "max = {:f}".format(maxi), {'color': 'k', 'fontsize': 10},
         horizontalalignment='left',
         verticalalignment='center',
         clip_on=False)

    if namefig is None:
        plt.show()
    else:
      plt.savefig(namefig)

    plt.close()
