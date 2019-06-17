# -*- coding:UTF-8 -*-

import cdtime
import matplotlib as plt # for colormaps

###################################
# Configuration file for RICO atlas
###################################

var2compute = ['zcb','zct']#,'theta_0_500','theta_2000_5000','qv_0_500','qv_2000_5000']

tmin = cdtime.comptime(2004,12,16,0)
tmax = cdtime.comptime(2004,12,17,0)


#################
# plot2D

plot2D = \
        {\
        'ymin'    :    0.                   ,\
        'ymax'    :    4.                   ,\
        'yname'   : 'altitude (km)'         ,\
        'levunits': 'km'                    ,\
        'dtlabel' : '2h'                    ,\
        'xname'   : '16 December 2004 (UTC)',\
        }
plot2D['var2plot'] = \
        {\
        'u'       : {'levels': range(-12,1,1)                  , 'extend':'both'                                       },\
        'v'       : {'levels': range(-8,9,1)                   , 'extend':'both'                                       },\
        'theta'   : {'levels': range(295,311,1)                , 'extend':'both'                                       },\
        'qv'      : {'levels': [0,] + range(3,18,1)            , 'extend':'max'                   , 'cmap':plt.cm.RdBu },\
        'ql'      : {'levels': range(0,21,1)                   , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'qr'      : {'levels': [i*0.2 for i in range(0,21,1)]  , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'rneb'    : {'levels': range(0,16,1)                   , 'extend':'max', 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'w_up'    : {'levels': [i*0.2 for i in range(0,16,1)]  , 'extend':'max', 'firstwhite':True                     },\
        'alpha_up': {'levels': [i*1. for i in range(0,16,1)]   , 'extend':'max', 'firstwhite':True                     },\
        'Mf'      : {'levels': [i*0.002 for i in range(0,16,1)], 'extend':'max', 'firstwhite':True                     },\
        'dTv_up'  : {'levels': [i*0.1 for i in range(-7,8,1)]  , 'extend':'both'                                       },\
        'B_up'    : {'levels': [i*0.002 for i in range(-7,8,1)], 'extend':'both'                                       },\
        'eps_u'   : {'levels': [i*0.3 for i in range(0,15,1)]  , 'extend':'both'                                       },\
        'det_u'   : {'levels': [i*0.3 for i in range(0,15,1)]  , 'extend':'both'                                       },\
#        'tnthl'   : {'levels': [i*0.5 for i in range(-7,8)]    , 'extend':'both'                                       },\
#        'tnqt'    : {'levels': [i*0.5 for i in range(-7,8)]    , 'extend':'both'                  , 'cmap':plt.cm.RdBu },\
        }

#################
# timeseries

plotTS = \
        {\
        'dtlabel': '2h'                    ,\
        'xname'  : '16 December 2004 (UTC)',\
        }
plotTS['var2plot'] = \
        {\
        'shf'            : {'ymin':-10., 'ymax':   50. },\
        'lhf'            : {'ymin':  0., 'ymax':  250. },\
        'ustar'          : {'ymin':  0., 'ymax':    0.6},\
        'tsurf'          : {'ymin':280., 'ymax':  320. },\
        'rain'           : {'ymin':  0., 'ymax':    2. },\
        'cc'             : {'ymin':  0., 'ymax':  105. },\
        'zcb'            : {'ymin':  0., 'ymax': 1000. },\
        'zct'            : {'ymin':  0., 'ymax': 4000. },\
        'lwp'            : {'ymin':  0., 'ymax':   80. },\
#        'theta_0_500'    : {'ymin':300., 'ymax':  312.},\
#        'theta_2000_5000': {'ymin':310., 'ymax':  320.},\
#        'qv_0_500'       : {'ymin': 10., 'ymax':   20.},\
#        'qv_2000_5000'   : {'ymin':  2., 'ymax':    8.},\
        }

#################
# Averaged profiles over time slices

plotAvgP = {}
plotAvgP['hour8-12'] = \
        {\
        'tmin'    : tmin.add(8,cdtime.Hour)      ,\
        'tmax'    : tmin.add(12,cdtime.Hour)     ,\
        'ymin'    :    0.                        ,\
        'ymax'    :    4.                        ,\
        'yname'   : 'altitude (km)'              ,\
        'levunits': 'km'                         ,\
        'title'   : '8-12 hour'                  ,\
        }
plotAvgP['hour8-12']['var2plot'] = \
        {\
        'u'       : {'xmin':  -12.  , 'xmax':  12. , 'init':True },\
        'v'       : {'xmin':  -10.  , 'xmax':  10. , 'init':True },\
        'theta'   : {'xmin':  295.  , 'xmax': 325. , 'init':True },\
        'qv'      : {'xmin':    0.  , 'xmax':  20. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax':  40.               },\
        'qr'      : {'xmin':    0.  , 'xmax':   5.               },\
        'rneb'    : {'xmin':    0.  , 'xmax':  20.               },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -1.  , 'xmax':   1.               },\
        'B_up'    : {'xmin':   -0.02, 'xmax':   0.02             },\
        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'det_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
#        'tnthl'   : {'xmin':   -2.  , 'xmax':   2.               },\
#        'tnqt'    : {'xmin':   -2.  , 'xmax':   2.               },\
        }

plotAvgP['hour20-24'] = \
        {\
        'tmin'    : tmin.add(20,cdtime.Hour)     ,\
        'tmax'    : tmin.add(24,cdtime.Hour)     ,\
        'ymin'    :    0.                        ,\
        'ymax'    :    4.                        ,\
        'yname'   : 'altitude (km)'              ,\
        'levunits': 'km'                         ,\
        'title'   : '20-24 hour'                 ,\
        }
plotAvgP['hour20-24']['var2plot'] = \
        {\
        'u'       : {'xmin':  -12.  , 'xmax':  12. , 'init':True },\
        'v'       : {'xmin':  -12.  , 'xmax':  12. , 'init':True },\
        'theta'   : {'xmin':  295.  , 'xmax': 325. , 'init':True },\
        'qv'      : {'xmin':    0.  , 'xmax':  20. , 'init':True },\
        'ql'      : {'xmin':    0.  , 'xmax':  40.               },\
        'qr'      : {'xmin':    0.  , 'xmax':   5.               },\
        'rneb'    : {'xmin':    0.  , 'xmax':  20.               },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -1.  , 'xmax':   1.               },\
        'B_up'    : {'xmin':   -0.02, 'xmax':   0.02             },\
        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
        'det_u'   : {'xmin':   -0.5 , 'xmax':   5.               },\
#        'tnthl'   : {'xmin':   -2.  , 'xmax':   2.               },\
#        'tnqt'    : {'xmin':   -2.  , 'xmax':   2.               },\
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
        'u'    : {'xmin': -12., 'xmax':   35.},\
        'v'    : {'xmin': -10., 'xmax': 10.},\
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
        'u'    : {'xmin': -12., 'xmax':   12.},\
        'v'    : {'xmin': -10., 'xmax':   10.},\
        'theta': {'xmin': 295., 'xmax':  325.},\
        'qv'   : {'xmin':   0., 'xmax':   18.},\
        'qv'   : {'xmin':  -1., 'xmax':   18.},\
        'ql'   : {'xmin':  -1., 'xmax':  200.},\
        'qi'   : {'xmin':  -1., 'xmax':  200.},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'zh'},\
        }

