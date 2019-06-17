# -*- coding:UTF-8 -*-

import cdtime
import matplotlib as plt # for colormaps

###################################
# Configuration file for FIRE atlas
###################################

var2compute = []#['zcb','zct','theta_0_500','theta_2000_5000','qv_0_500','qv_2000_5000']

tmin = cdtime.comptime(2009,12,11,0)
tmax = cdtime.comptime(2009,12,11,6)


#################
# plot2D

plot2D = \
        {\
        'ymin'    :    0.                  ,\
        'ymax'    :    4.                  ,\
        'yname'   : 'altitude (km)'        ,\
        'levunits': 'km'                   ,\
        'dtlabel' : '1h'                   ,\
        'xname'   : 'Hours since beginning',\
        }
plot2D['var2plot'] = \
        {\
        'u'       : {'levels': range(-1,16,1)                  , 'extend':'both'                                       },\
        'v'       : {'levels': range(-6,7,1)                   , 'extend':'both'                                       },\
        'theta'   : {'levels': range(300,316,1)                , 'extend':'both'                                       },\
        'w_up'    : {'levels': [i*0.2 for i in range(0,16,1)]  , 'extend':'max', 'firstwhite':True                     },\
        'alpha_up': {'levels': [i*1. for i in range(0,16,1)]   , 'extend':'max', 'firstwhite':True                     },\
        'Mf'      : {'levels': [i*0.005 for i in range(0,16,1)], 'extend':'max', 'firstwhite':True                     },\
        'dTv_up'  : {'levels': [i*0.1 for i in range(-7,8,1)]  , 'extend':'both'                                       },\
        'B_up'    : {'levels': [i*0.002 for i in range(-7,8,1)], 'extend':'both'                                       },\
        'eps_u'   : {'levels': [i*0.3 for i in range(0,15,1)]  , 'extend':'both'                                       },\
        'det_u'   : {'levels': [i*0.3 for i in range(0,15,1)]  , 'extend':'both'                                       },\
        }

#################
# timeseries

plotTS = \
        {\
        'dtlabel': '1h'                ,\
        'xname'  : '21 June 1997 (UTC)',\
        }
plotTS['var2plot'] = \
        {\
        'shf'            : {'ymin':-40., 'ymax':  300.},\
        'lhf'            : {'ymin':-40., 'ymax':  300.},\
        'ustar'          : {'ymin':  0., 'ymax':    1.},\
        'tsurf'          : {'ymin':280., 'ymax':  320.},\
#        'theta_0_500'    : {'ymin':300., 'ymax':  312.},\
#        'theta_2000_5000': {'ymin':310., 'ymax':  320.},\
#        'qv_0_500'       : {'ymin': 10., 'ymax':   20.},\
#        'qv_2000_5000'   : {'ymin':  2., 'ymax':    8.},\
        }

#################
# Averaged profiles over time slices

plotAvgP = {}
plotAvgP['hour4-5'] = \
        {\
        'tmin'    : tmin.add(4,cdtime.Hour)      ,\
        'tmax'    : tmin.add(5,cdtime.Hour)      ,\
        'ymin'    :    0.                        ,\
        'ymax'    :    4.                        ,\
        'yname'   : 'altitude (km)'              ,\
        'levunits': 'km'                         ,\
        'title'   : '4-5 hour'                   ,\
        }
plotAvgP['hour4-5']['var2plot'] = \
        {\
        'u'       : {'xmin':   -1.  , 'xmax':  17. , 'init':True },\
        'v'       : {'xmin':   -3.  , 'xmax':  10. , 'init':True },\
        'theta'   : {'xmin':  298.  , 'xmax': 325. , 'init':True },\
        'w_up'    : {'xmin':    0.  , 'xmax':   4.               },\
        'alpha_up': {'xmin':    0.  , 'xmax':  25.               },\
        'Mf'      : {'xmin':    0.  , 'xmax':   0.3, 'lev':'zh'  },\
        'dTv_up'  : {'xmin':   -2.  , 'xmax':   1.               },\
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
        'u'    : {'xmin':  -1., 'xmax':   17.},\
        'v'    : {'xmin':  -3., 'xmax':   10.},\
        'theta': {'xmin': 295., 'xmax':  400.},\
        'qv'   : {'xmin':  -1., 'xmax':    5.},\
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
        'u'    : {'xmin':  -1., 'xmax':   17.},\
        'v'    : {'xmin':  -3., 'xmax':   10.},\
        'theta': {'xmin': 295., 'xmax':  325.},\
        'qv'   : {'xmin':  -1., 'xmax':    5.},\
        'ql'   : {'xmin':  -1., 'xmax':  200.},\
        'qi'   : {'xmin':  -1., 'xmax':  200.},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'zh'},\
        }

