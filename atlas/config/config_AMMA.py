# -*- coding:UTF-8 -*-

import cdtime
import matplotlib as plt # for colormaps

###################################
# Configuration file for FIRE atlas
###################################

var2compute = ['zcb','zct','theta_0_500','theta_2000_5000','qv_0_500','qv_2000_5000']

tmin = cdtime.comptime(2006,7,10,6)
tmax = cdtime.comptime(2006,7,10,18)


#################
# plot2D

plot2D = \
        {\
        'ymin'    :    0.               ,\
        'ymax'    :   16.               ,\
        'yname'   : 'altitude (km)'     ,\
        'levunits': 'km'                ,\
        'dtlabel' : '1h'                ,\
        'xname'   : '10 July 2006 (UTC)',\
        }
plot2D['var2plot'] = \
        {\
        'u'       : {'levels': [i*1. for i in range(-15,6,1)]  , 'extend':'both'                                       },\
        'v'       : {'levels': [i*1. for i in range(-7,8,1)]   , 'extend':'both'                                       },\
        'theta'   : {'levels': range(300,331,2)                , 'extend':'both'                                       },\
        'qv'      : {'levels': range(0,19,1)                   , 'extend':'max'                   , 'cmap':plt.cm.RdBu },\
        'ql'      : {'levels': range(0,41,4)                   , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'qi'      : {'levels': [i*0.3 for i in range(0,16,1)]  , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'qr'      : {'levels': [i*0.5 for i in range(0,21,1)]  , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'qsn'     : {'levels': [i*0.5 for i in range(0,21,1)]  , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'rneb'    : {'levels': [0,1] + range(4,21,2)           , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'w_up'    : {'levels': [i*0.2 for i in range(0,16,1)]  , 'extend':'max', 'firstwhite':True                     },\
        'alpha_up': {'levels': [i*1. for i in range(0,16,1)]   , 'extend':'max', 'firstwhite':True                     },\
        'Mf'      : {'levels': [i*0.005 for i in range(0,16,1)], 'extend':'max', 'firstwhite':True                     },\
        'dTv_up'  : {'levels': [i*0.1 for i in range(-7,8,1)]  , 'extend':'both'                                       },\
        'B_up'    : {'levels': [i*0.002 for i in range(-7,8,1)], 'extend':'both'                                       },\
        'eps_u'   : {'levels': [i*0.3 for i in range(0,15,1)]  , 'extend':'both'                                       },\
        'det_u'   : {'levels': [i*0.3 for i in range(0,15,1)]  , 'extend':'both'                                       },\
        'tnthl'   : {'levels': [i*0.5 for i in range(-7,8)]    , 'extend':'both'                                       },\
        'tnqt'    : {'levels': [i*0.5 for i in range(-7,8)]    , 'extend':'both'                  , 'cmap':plt.cm.RdBu },\
        }

#################
# timeseries

plotTS = \
        {\
        'dtlabel': '2h'                ,\
        'xname'  : '10 July 2006 (UTC)',\
        }
plotTS['var2plot'] = \
        {\
        'shf'            : {'ymin':-10., 'ymax':  400.},\
        'lhf'            : {'ymin':-10., 'ymax':  100.},\
        'ustar'          : {'ymin':  0., 'ymax':    1.},\
        'tsurf'          : {'ymin':280., 'ymax':  330.},\
        'rain'           : {'ymin':  0., 'ymax':    5.},\
        'cc'             : {'ymin':  0., 'ymax':  100.},\
        'zcb'            : {'ymin':  0., 'ymax': 5000.},\
        'zct'            : {'ymin':  0., 'ymax':18000.},\
        'lwp'            : {'ymin':  0., 'ymax':  100.},\
        'iwp'            : {'ymin':  0., 'ymax':   15.},\
        'theta_0_500'    : {'ymin':300., 'ymax':  312.},\
        'theta_2000_5000': {'ymin':310., 'ymax':  320.},\
        'qv_0_500'       : {'ymin': 10., 'ymax':   20.},\
        'qv_2000_5000'   : {'ymin':  2., 'ymax':    8.},\
        }

#################
# Averaged profiles over time slices

plotAvgP = {}
plotAvgP['hour11-12'] = \
        {\
        'tmin'    : cdtime.comptime(2006,7,10,11),\
        'tmax'    : cdtime.comptime(2006,7,10,12),\
        'ymin'    :    0.                        ,\
        'ymax'    :    4.                        ,\
        'yname'   : 'altitude (km)'              ,\
        'levunits': 'km'                         ,\
        'title'   : '11-12 hour'                 ,\
        }
plotAvgP['hour11-12']['var2plot'] = \
        {\
        'u'       : {'xmin':  -20.  , 'xmax':  10. , 'init':True },\
        'v'       : {'xmin':  -20.  , 'xmax':  10. , 'init':True },\
        'theta'   : {'xmin':  300.  , 'xmax': 320. , 'init':True },\
        'qv'      : {'xmin':    6.  , 'xmax':  18. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax': 100.               },\
        'qi'      : {'xmin':    0.  , 'xmax':  10.               },\
        'qr'      : {'xmin':    0.  , 'xmax':  50.               },\
        'qsn'     : {'xmin':    0.  , 'xmax':  50.               },\
        'rneb'    : {'xmin':    0.  , 'xmax':  60.               },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -2.  , 'xmax':   2.               },\
        'B_up'    : {'xmin':   -0.05, 'xmax':   0.05             },\
        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'det_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'tnthl'   : {'xmin':   -2.  , 'xmax':   2.               },\
        'tnqt'    : {'xmin':   -2.  , 'xmax':   2.               },\
        }

plotAvgP['hour15-16'] = \
        {\
        'tmin'    : cdtime.comptime(2006,7,10,15),\
        'tmax'    : cdtime.comptime(2006,7,10,16),\
        'ymin'    :    0.                        ,\
        'ymax'    :    8.                        ,\
        'yname'   : 'altitude (km)'              ,\
        'levunits': 'km'                         ,\
        'title'   : '15-16 hour'                 ,\
        }
plotAvgP['hour15-16']['var2plot'] = \
        {\
        'u'       : {'xmin':  -20.  , 'xmax':  10. , 'init':True },\
        'v'       : {'xmin':  -20.  , 'xmax':  10. , 'init':True },\
        'theta'   : {'xmin':  300.  , 'xmax': 335. , 'init':True },\
        'qv'      : {'xmin':    6.  , 'xmax':  18. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax': 100.               },\
        'qi'      : {'xmin':    0.  , 'xmax':  10.               },\
        'qr'      : {'xmin':    0.  , 'xmax':  50.               },\
        'qsn'     : {'xmin':    0.  , 'xmax':  50.               },\
        'rneb'    : {'xmin':    0.  , 'xmax':  60.               },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -2.  , 'xmax':   2.               },\
        'B_up'    : {'xmin':   -0.05, 'xmax':   0.05             },\
        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'det_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'tnthl'   : {'xmin':   -2.  , 'xmax':   2.               },\
        'tnqt'    : {'xmin':   -2.  , 'xmax':   2.               },\
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
        'u'    : {'xmin': -20., 'xmax':   10.},\
        'v'    : {'xmin': -20., 'xmax':   20.},\
        'theta': {'xmin': 295., 'xmax':  400.},\
        'qv'   : {'xmin':   0., 'xmax':   18.},\
        'qv'   : {'xmin':  -1., 'xmax':   18.},\
        'ql'   : {'xmin':  -1., 'xmax':  200.},\
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
        'u'    : {'xmin': -20., 'xmax':   10.},\
        'v'    : {'xmin': -20., 'xmax':   10.},\
        'theta': {'xmin': 295., 'xmax':  325.},\
        'qv'   : {'xmin':   0., 'xmax':   18.},\
        'qv'   : {'xmin':  -1., 'xmax':   18.},\
        'ql'   : {'xmin':  -1., 'xmax':  200.},\
        'qi'   : {'xmin':  -1., 'xmax':  200.},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'zh'},\
        }

