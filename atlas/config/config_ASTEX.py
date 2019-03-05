# -*- coding:UTF-8 -*-

import cdtime
import matplotlib as plt # for colormaps

###################################
# Configuration file for SANDU atlas
###################################

var2compute = ['zcb','zct']

tmin = cdtime.comptime(1992,6,13,0)
tmax = cdtime.comptime(1992,6,14,16)


#################
# plot2D

plot2D = \
        {\
        'ymin'    :    0.                  ,\
        'ymax'    :    4.                  ,\
        'yname'   : 'altitude k(m)'        ,\
        'levunits': 'km'                   ,\
        'dtlabel' : '6h'                   ,\
        'xname'   : '13-14 June 1992 (UTC)',\
        }
plot2D['var2plot'] = \
        {\
        'u'       : {'levels': [i*0.5 for i in range(-10,1)]         , 'extend':'both'                                       },\
        'v'       : {'levels': [i*1. for i in range(-12,1)]          , 'extend':'both'                                       },\
        'theta'   : {'levels': range(285,306,1)                     , 'extend':'both'                                       },\
        'qv'      : {'levels': range(0,14,1)                        , 'extend':'max'                    , 'cmap':plt.cm.RdBu},\
        'ql'      : {'levels': range(0,421,30)                      , 'extend':'max', 'firstwhite':True , 'cmap':plt.cm.RdBu},\
        'qr'      : {'levels': range(0,16,1)                        , 'extend':'max', 'firstwhite':True , 'cmap':plt.cm.RdBu},\
        'rneb'    : {'levels': [0,1,5] + range(10,100,10) + [95,100], 'extend':'max', 'firstwhite':True , 'cmap':plt.cm.RdBu},\
        'w_up'    : {'levels': [i*0.1 for i in range(0,16,1)]       , 'extend':'max', 'firstwhite':True                     },\
        'alpha_up': {'levels': range(0,11,1)                        , 'extend':'max', 'firstwhite':True                     },\
        'Mf'      : {'levels': [i*0.005 for i in range(0,16,1)]     , 'extend':'max', 'firstwhite':True                     },\
        'dTv_up'  : {'levels': [i*0.1 for i in range(-7,8,1)]       , 'extend':'both'                                       },\
        'B_up'    : {'levels': [i*0.002 for i in range(-7,8,1)]     , 'extend':'both'                                       },\
        'eps_u'   : {'levels': [i*0.3 for i in range(0,15,1)]       , 'extend':'both'                                       },\
        'det_u'   : {'levels': [i*0.3 for i in range(0,15,1)]       , 'extend':'both'                                       },\
        }

#################
# timeseries

plotTS = \
        {\
        'dtlabel': '6h'                   ,\
        'xname'  : '13-14 June 1992 (UTC)',\
        }
plotTS['var2plot'] = \
        {\
        'shf'  : {'ymin':-10., 'ymax':   40.},\
        'lhf'  : {'ymin':  0., 'ymax':  200.},\
        'ustar': {'ymin':  0., 'ymax':    1.},\
        'tsurf': {'ymin':280., 'ymax':  320.},\
        'rain' : {'ymin':  0., 'ymax':    3.},\
        'cc'   : {'ymin':  0., 'ymax':  105.},\
        'zcb'  : {'ymin':  0., 'ymax': 1000.},\
        'zct'  : {'ymin':  0., 'ymax': 3000.},\
        'lwp'  : {'ymin':  0., 'ymax':  250.},\
        }

#################
# Averaged profiles over time slices

plotAvgP = {}
plotAvgP['hour23-24'] = \
        {\
        'tmin'    : tmin.add(23,cdtime.Hour),\
        'tmax'    : tmin.add(24,cdtime.Hour),\
        'ymin'    :    0.                   ,\
        'ymax'    :    4.                   ,\
        'yname'   : 'altitude (km)'         ,\
        'levunits': 'km'                    ,\
        'title'   : '23-24 hour'            ,\
        }
plotAvgP['hour23-24']['var2plot'] = \
        {\
        'u'       : {'xmin':   -5.  , 'xmax':   5. , 'init':True },\
        'v'       : {'xmin':  -12.  , 'xmax':   0. , 'init':True },\
        'theta'   : {'xmin':  285.  , 'xmax': 315. , 'init':True },\
        'qv'      : {'xmin':    0.  , 'xmax':  13. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax': 500.               },\
        'qr'      : {'xmin':    0.  , 'xmax':  20.               },\
        'rneb'    : {'xmin':    0.  , 'xmax': 105.               },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -1.  , 'xmax':   1.               },\
        'B_up'    : {'xmin':   -0.02, 'xmax':   0.02             },\
        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'det_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        }

plotAvgP['hour39-40'] = \
        {\
        'tmin'    : tmin.add(39,cdtime.Hour),\
        'tmax'    : tmin.add(40,cdtime.Hour),\
        'ymin'    :    0.                   ,\
        'ymax'    :    4.                   ,\
        'yname'   : 'altitude (km)'         ,\
        'levunits': 'km'                    ,\
        'title'   : '39-40 hour'            ,\
        }
plotAvgP['hour39-40']['var2plot'] = \
        {\
        'u'       : {'xmin':   -5.  , 'xmax':   5. , 'init':True },\
        'v'       : {'xmin':  -12.  , 'xmax':   0. , 'init':True },\
        'theta'   : {'xmin':  285.  , 'xmax': 315. , 'init':True },\
        'qv'      : {'xmin':    0.  , 'xmax':  13. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax': 500.               },\
        'qr'      : {'xmin':    0.  , 'xmax':  20.               },\
        'rneb'    : {'xmin':    0.  , 'xmax': 105.               },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -1.  , 'xmax':   1.               },\
        'B_up'    : {'xmin':   -0.02, 'xmax':   0.02             },\
        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'det_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        }


#################
# Checking initial profiles - global view

plotInitGV = \
        {\
        'ymin'    :  0.            ,\
        'ymax'    : 20.            ,\
        'yname'   : 'altitude (km)',\
        'levunits': 'km'           ,\
        }

plotInitGV['var2plot'] = \
        {\
        'u'    : {'xmin':  -5., 'xmax':    5.},\
        'v'    : {'xmin': -20., 'xmax':   20.},\
        'theta': {'xmin': 280., 'xmax':  400.},\
        'qv'   : {'xmin':   0., 'xmax':   15.},\
        'ql'   : {'xmin':  -1., 'xmax':  700.},\
        'qi'   : {'xmin':  -1., 'xmax':  200.},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'zh'},\
        }

#################
# Checking initial profiles - zoom in lowest levels

plotInitLL = \
        {\
        'ymin'    :    0.          ,\
        'ymax'    :    4.          ,\
        'yname'   : 'altitude (km)',\
        'levunits': 'km'           ,\
        }

plotInitLL['var2plot'] = \
        {\
        'u'    : {'xmin':  -5., 'xmax':    5.},\
        'v'    : {'xmin': -20., 'xmax':   20.},\
        'theta': {'xmin': 280., 'xmax':  325.},\
        'qv'   : {'xmin':  -1., 'xmax':   18.},\
        'ql'   : {'xmin':  -1., 'xmax':  700.},\
        'qi'   : {'xmin':  -1., 'xmax':  200.},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'zh'},\
        }

