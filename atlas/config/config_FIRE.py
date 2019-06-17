# -*- coding:UTF-8 -*-

import cdtime
import matplotlib as plt # for colormaps

###################################
# Configuration file for FIRE atlas
###################################

var2compute = ['zcb','zct']

tmin = cdtime.comptime(1987,7,14,8)
tmax = cdtime.comptime(1987,7,15,8)


#################
# plot2D

plot2D = \
        {\
        'ymin'    :    0.                  ,\
        'ymax'    : 1200.                  ,\
        'yname'   : 'altitude (m)'         ,\
        'levunits': 'm'                    ,\
        'dtlabel' : '2h'                   ,\
        'xname'   : '14-15 July 1987 (UTC)',\
        }
plot2D['var2plot'] = \
        {\
        'u'       : {'levels': [i*0.5 for i in range(0,11)]         , 'extend':'both'                                       },\
        'v'       : {'levels': [i*0.5 for i in range(-10,1)]        , 'extend':'both'                                       },\
        'theta'   : {'levels': range(287,301,1)                     , 'extend':'both'                                       },\
        'qv'      : {'levels': range(0,11,1)                        , 'extend':'max'                    , 'cmap':plt.cm.RdBu},\
        'ql'      : {'levels': range(0,701,50)                      , 'extend':'max', 'firstwhite':True , 'cmap':plt.cm.RdBu},\
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
        'dtlabel': '2h'                   ,\
        'xname'  : '14-15 July 1987 (UTC)',\
        }
plotTS['var2plot'] = \
        {\
        'shf'  : {'ymin': -5., 'ymax':   20.},\
        'lhf'  : {'ymin':  0., 'ymax':   50.},\
        'ustar': {'ymin':  0., 'ymax':    1.},\
        'tsurf': {'ymin':280., 'ymax':  320.},\
        'rain' : {'ymin':  0., 'ymax':    3.},\
        'cc'   : {'ymin':  0., 'ymax':  105.},\
        'zcb'  : {'ymin':  0., 'ymax': 1000.},\
        'zct'  : {'ymin':  0., 'ymax': 1000.},\
        'lwp'  : {'ymin':  0., 'ymax':  200.},\
        }

#################
# Averaged profiles over time slices

plotAvgP = {}
plotAvgP['hour4-8'] = \
        {\
        'tmin'    : tmin.add(4,cdtime.Hour),\
        'tmax'    : tmin.add(8,cdtime.Hour),\
        'ymin'    :    0.                  ,\
        'ymax'    : 1200.                  ,\
        'yname'   : 'altitude (m)'         ,\
        'levunits': 'm'                    ,\
        'title'   : '4-6 hour (night)'     ,\
        }
plotAvgP['hour4-8']['var2plot'] = \
        {\
        'u'       : {'xmin':    0.  , 'xmax':   5. , 'init':True },\
        'v'       : {'xmin':   -6.  , 'xmax':   0. , 'init':True },\
        'theta'   : {'xmin':  280.  , 'xmax': 325. , 'init':True },\
        'qv'      : {'xmin':    0.  , 'xmax':  15. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax': 700.               },\
        'qr'      : {'xmin':    0.  , 'xmax':  10.               },\
        'rneb'    : {'xmin':    0.  , 'xmax': 105.               },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -1.  , 'xmax':   1.               },\
        'B_up'    : {'xmin':   -0.02, 'xmax':   0.02             },\
        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'det_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        }

plotAvgP['hour12-16'] = \
        {\
        'tmin'    : tmin.add(12,cdtime.Hour),\
        'tmax'    : tmin.add(16,cdtime.Hour),\
        'ymin'    :    0.                   ,\
        'ymax'    : 1200.                   ,\
        'yname'   : 'altitude (m)'          ,\
        'levunits': 'm'                     ,\
        'title'   : '12-16 hour (day)'      ,\
        }
plotAvgP['hour12-16']['var2plot'] = \
        {\
        'u'       : {'xmin':    0.  , 'xmax':   5. , 'init':True },\
        'v'       : {'xmin':   -6.  , 'xmax':   0. , 'init':True },\
        'theta'   : {'xmin':  280.  , 'xmax': 325. , 'init':True },\
        'qv'      : {'xmin':    0.  , 'xmax':  15. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax': 700.               },\
        'qr'      : {'xmin':    0.  , 'xmax':  10.               },\
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
        'qv'   : {'xmin':  -1., 'xmax':   18.},\
        'ql'   : {'xmin':  -1., 'xmax':  700.},\
        'qi'   : {'xmin':  -1., 'xmax':  200.},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'zh'},\
        }

#################
# Checking initial profiles - zoom in lowest levels

plotInitLL = \
        {\
        'ymin'    :    0.         ,\
        'ymax'    : 1200.         ,\
        'yname'   : 'altitude (m)',\
        'levunits': 'm'           ,\
        }

plotInitLL['var2plot'] = \
        {\
        'u'    : {'xmin':  -5., 'xmax':    5.},\
        'v'    : {'xmin': -20., 'xmax':   20.},\
        'theta': {'xmin': 280., 'xmax':  325.},\
        'qv'   : {'xmin':   0., 'xmax':   15.},\
        'qv'   : {'xmin':  -1., 'xmax':   18.},\
        'ql'   : {'xmin':  -1., 'xmax':  700.},\
        'qi'   : {'xmin':  -1., 'xmax':  200.},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'zh'},\
        }

