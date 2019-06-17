# -*- coding:UTF-8 -*-

import cdtime
import matplotlib as plt # for colormaps

###################################
# Configuration file for FIRE atlas
###################################

var2compute = ['Qr_int','TOA_cre_sw','TOA_cre_lw','Qr_int_cre']

tmin = cdtime.comptime(2011,10,1)
tmax = cdtime.comptime(2011,12,31)


#################
# plot2D

plot2D = \
        {\
        'ymin'    : 1000.                   ,\
        'ymax'    :   50.                   ,\
        'yname'   : 'Pressure (hPa)'        ,\
        'levunits': 'hPa'                   ,\
        'dtlabel' : '10d'                   ,\
        'xname'   : 'October-December 2011' ,\
        'lgrid'   : True                    ,\
        'figsize' : (20,5) ,\
        }
plot2D['var2plot'] = \
        {\
        'u'       : {'levels': [i*1. for i in range(-15,6,1)]   , 'lev':'ph', 'extend':'both'                                        },\
        'v'       : {'levels': [i*1. for i in range(-7,8,1)]    , 'lev':'ph', 'extend':'both'                                        },\
#        'theta'   : {'levels': range(300,331,2)                 , 'lev':'ph', 'extend':'both'                                        },\
#        'qv'      : {'levels': range(0,19,1)                    , 'lev':'ph', 'extend':'max'                    , 'cmap':plt.cm.RdBu },\
#        'ql'      : {'levels': range(0,41,4)                    , 'lev':'ph', 'extend':'max' , 'firstwhite':True, 'cmap':plt.cm.RdBu },\
#        'qi'      : {'levels': [i*0.3 for i in range(0,16,1)]   , 'lev':'ph', 'extend':'max' , 'firstwhite':True, 'cmap':plt.cm.RdBu },\
#        'qr'      : {'levels': [i*0.5 for i in range(0,21,1)]   , 'lev':'ph', 'extend':'max' , 'firstwhite':True, 'cmap':plt.cm.RdBu },\
#        'qsn'     : {'levels': [i*0.5 for i in range(0,21,1)]   , 'lev':'ph', 'extend':'max' , 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'hur'     : {'levels': [0,5]+range(10,91,10)+[95,100]   , 'lev':'ph', 'extend':'max' , 'firstwhite':True, 'cmap':plt.cm.RdBu },\
        'rneb'    : {'levels': [0,1,5]+range(10,91,10)+[95,100] , 'lev':'ph', 'extend':'max' , 'firstwhite':True, 'cmap':plt.cm.RdBu },\
#        'w_up'    : {'levels': [i*0.2 for i in range(0,16,1)]   , 'lev':'ph', 'extend':'max' , 'firstwhite':True                     },\
#        'alpha_up': {'levels': [i*1. for i in range(0,16,1)]    , 'lev':'ph', 'extend':'max' , 'firstwhite':True                     },\
#        'Mf'      : {'levels': [i*0.005 for i in range(0,16,1)] , 'lev':'ph', 'extend':'max' , 'firstwhite':True                     },\
#        'dTv_up'  : {'levels': [i*0.1 for i in range(-7,8,1)]   , 'lev':'ph', 'extend':'both'                                        },\
#        'B_up'    : {'levels': [i*0.002 for i in range(-7,8,1)] , 'lev':'ph', 'extend':'both'                                        },\
#        'eps_u'   : {'levels': [i*0.3 for i in range(0,15,1)]   , 'lev':'ph', 'extend':'both'                                        },\
#        'det_u'   : {'levels': [i*0.3 for i in range(0,15,1)]   , 'lev':'ph', 'extend':'both'                                        },\
        'Q1'      : {'levels': [i*2. for i in range(-7,8)]      , 'lev':'ph' , 'extend':'both'                                       },\
        'Q2'      : {'levels': [i*2. for i in range(-7,8)]      , 'lev':'ph' , 'extend':'both'                  , 'cmap':plt.cm.RdBu },\
        }

#################
# plot2D

plot2Dbias = \
        {\
        'ymin'    : 1000.                   ,\
        'ymax'    :   50.                   ,\
        'yname'   : 'Pressure (hPa)'        ,\
        'levunits': 'hPa'                   ,\
        'dtlabel' : '10d'                   ,\
        'xname'   : 'October-December 2011' ,\
        'lgrid'   : True                    ,\
        'figsize' : (20,5) ,\
        }
plot2Dbias['var2plot'] = \
        {\
        'u'       : {'levels': [i*0.5 for i in range(-7,8,1)] , 'lev':'ph', 'refdataset':'CSU', 'extend':'both'                     },\
        'v'       : {'levels': [i*0.5 for i in range(-7,8,1)] , 'lev':'ph', 'refdataset':'CSU', 'extend':'both'                     },\
        'theta'   : {'levels': range(-10,11,1)                , 'lev':'ph', 'refdataset':'CSU', 'extend':'both'                     },\
        'qv'      : {'levels': [i*0.5 for i in range(-7,8,1)] , 'lev':'ph', 'refdataset':'CSU', 'extend':'both', 'cmap':plt.cm.RdBu },\
        'hur'     : {'levels': [i*5. for i in range(-7,8,1)]  , 'lev':'ph', 'refdataset':'CSU', 'extend':'both', 'cmap':plt.cm.RdBu },\
#        'Q1'      : {'levels': [i*2. for i in range(-7,8)]  , 'lev':'ph',                       'extend':'both'                     },\
#        'Q2'      : {'levels': [i*2. for i in range(-7,8)]  , 'lev':'ph',                       'extend':'both', 'cmap':plt.cm.RdBu },\
        }
        
#################
# timeseries

plotTS = \
        {\
        'dtlabel': '10d'                   ,\
        'xname'  : 'October-December 2011' ,\
        'lgrid'   : True                   ,\
        'figsize' : (20,5) ,\
        }
plotTS['var2plot'] = \
        {\
        'shf'            : {'ymin':  -5., 'ymax':  30.},\
        'lhf'            : {'ymin': -10., 'ymax': 200.},\
#        'ustar'          : {'ymin':   0., 'ymax':    1.},\
        'tsurf'          : {'ymin': 299., 'ymax': 304.},\
        'rain'           : {'ymin':   0., 'ymax':  80.},\
        'cc'             : {'ymin':   0., 'ymax': 105.},\
        'cltl'           : {'ymin':   0., 'ymax': 105.},\
        'clth'           : {'ymin':   0., 'ymax': 105.},\
        'Qr_int'         : {'ymin':-200., 'ymax':  50.},\
        'Qr_int_cre'     : {'ymin': -50., 'ymax': 200.},\
        'rsut'           : {'ymin':   0., 'ymax': 300.},\
        'rsdt'           : {'ymin': 350., 'ymax': 450.},\
        'rlut'           : {'ymin': 100., 'ymax': 350.},\
        'rsus'           : {'ymin':   0., 'ymax':  30.},\
        'rsds'           : {'ymin':   0., 'ymax': 400.},\
        'rlus'           : {'ymin': 400., 'ymax': 500.},\
        'rlds'           : {'ymin': 300., 'ymax': 500.},\
        'TOA_cre_sw'     : {'ymin':-250., 'ymax':  50.},\
        'TOA_cre_lw'     : {'ymin':   0., 'ymax': 200.},\
#        'zcb'            : {'ymin':   0., 'ymax': 5000.},\
#        'zct'            : {'ymin':   0., 'ymax':18000.},\
        'lwp'            : {'ymin':   0., 'ymax': 300.},\
        'iwp'            : {'ymin':   0., 'ymax': 300.},\
#        'theta_0_500'    : {'ymin': 300., 'ymax':  312.},\
#        'theta_2000_5000': {'ymin': 310., 'ymax':  320.},\
#        'qv_0_500'       : {'ymin':  10., 'ymax':   20.},\
#        'qv_2000_5000'   : {'ymin':   2., 'ymax':    8.},\
        }

#################
# Averaged profiles over time slices

plotAvgP = {}
plotAvgP['avg'] = \
        {\
        'tmin'    : tmin                      ,\
        'tmax'    : tmax                      ,\
        'ymin'    : 1000.                     ,\
        'ymax'    :   50.                     ,\
        'yname'   : 'Pressure (hPa)'          ,\
        'levunits': 'hPa'                     ,\
        'title'   : 'Average'                 ,\
        'lgrid'   : True                      ,\
        }
plotAvgP['avg']['var2plot'] = \
        {\
        'u'       : {'xmin':  -20.  , 'xmax':  10.  , 'lev':'pf'},\
        'v'       : {'xmin':   -2.  , 'xmax':   2.  , 'lev':'pf'},\
        'theta'   : {'xmin':  290.  , 'xmax': 340.  , 'lev':'pf'},\
        'qv'      : {'xmin':    0.  , 'xmax':  22.  , 'lev':'pf'},\
#        'ql'      : {'xmin':    0.  , 'xmax': 100.  , 'lev':'pf'},\
#        'qi'      : {'xmin':    0.  , 'xmax':  10.  , 'lev':'pf'},\
#        'qr'      : {'xmin':    0.  , 'xmax':  50.  , 'lev':'pf'},\
#        'qsn'     : {'xmin':    0.  , 'xmax':  50.  , 'lev':'pf'},\
        'hur'     : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'rneb'    : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
#        'w_up'    : {'xmin':    0.  , 'xmax':   4.  , 'lev':'pf'},\
#        'alpha_up': {'xmin':    0.  , 'xmax':  25.  , 'lev':'pf'},\
#        'Mf'      : {'xmin':    0.  , 'xmax':   0.3 , 'lev':'pf'},\
#        'dTv_up'  : {'xmin':   -2.  , 'xmax':   2.  , 'lev':'pf'},\
#        'B_up'    : {'xmin':   -0.05, 'xmax':   0.05, 'lev':'pf'},\
#        'eps_u'   : {'xmin':   -0.5 , 'xmax':   5. , 'lev':'pf'},\
#        'det_u'   : {'xmin':   -0.5 , 'xmax':   5. , 'lev':'pf'},\
        'Q1'      : {'xmin':   -2.  , 'xmax':   5.  , 'lev':'pf'},\
        'Q2'      : {'xmin':   -2.  , 'xmax':   5.  ,'lev':'pf'},\
        }

plotAvgP['avg_bias'] = \
        {\
        'tmin'    : tmin                      ,\
        'tmax'    : tmax                      ,\
        'ymin'    : 1000.                     ,\
        'ymax'    :   50.                     ,\
        'yname'   : 'Pressure (hPa)'          ,\
        'levunits': 'hPa'                     ,\
        'title'   : 'Bias average'            ,\
        'lgrid'   : True                      ,\
        }
plotAvgP['avg_bias']['var2plot'] = \
        {\
        'u'       : {'xmin':   -2.  , 'xmax':   2. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'v'       : {'xmin':   -2.  , 'xmax':   2. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'theta'   : {'xmin':   -4.  , 'xmax':   4. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'qv'      : {'xmin':   -3.  , 'xmax':   3. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'hur'     : {'xmin':  -40.  , 'xmax':  40. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
#        'Q1'      : {'xmin':   -2.  , 'xmax':   5. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
#        'Q2'      : {'xmin':   -2.  , 'xmax':   5. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        }

# 1st MJO event

plotAvgP['MJO1'] = \
        {\
        'tmin'    : cdtime.comptime(2011,10,20,0),\
        'tmax'    : cdtime.comptime(2011,11,1,0) ,\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'MJO1 (10/20-11/01)'         ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['MJO1']['var2plot'] = \
        {\
        'hur'     : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'rneb'    : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'Q1'      : {'xmin':   -2.  , 'xmax':   12.  , 'lev':'pf'},\
        'Q2'      : {'xmin':   -2.  , 'xmax':   12.  ,'lev':'pf'},\
        }

plotAvgP['MJO1_bias'] = \
        {\
        'tmin'    : cdtime.comptime(2011,10,20,0),\
        'tmax'    : cdtime.comptime(2011,11,1,0) ,\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'MJO1 (10/20-11/01) - Bias'  ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['MJO1_bias']['var2plot'] = \
        {\
        'theta'   : {'xmin':   -5.  , 'xmax':   5. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'qv'      : {'xmin':   -3.  , 'xmax':   3. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        }

# 2nd suppressed phase

plotAvgP['SUPP2'] = \
        {\
        'tmin'    : cdtime.comptime(2011,11,5,0) ,\
        'tmax'    : cdtime.comptime(2011,11,15,0),\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'SUPP2 (11/05-11/15)'        ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['SUPP2']['var2plot'] = \
        {\
        'hur'     : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'rneb'    : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'Q1'      : {'xmin':   -2.  , 'xmax':    5. , 'lev':'pf'},\
        'Q2'      : {'xmin':   -2.  , 'xmax':    5. ,'lev':'pf'},\
        }

plotAvgP['SUPP2_bias'] = \
        {\
        'tmin'    : cdtime.comptime(2011,11,5,0) ,\
        'tmax'    : cdtime.comptime(2011,11,15,0),\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'SUPP2 (11/05-11/15) - Bias' ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['SUPP2_bias']['var2plot'] = \
        {\
        'theta'   : {'xmin':   -5.  , 'xmax':   5. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'qv'      : {'xmin':   -3.  , 'xmax':   3. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        }

# 2nd MJO event

plotAvgP['MJO2'] = \
        {\
        'tmin'    : cdtime.comptime(2011,11,20,0),\
        'tmax'    : cdtime.comptime(2011,12,1,0) ,\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'MJO2 (11/20-12/01)'         ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['MJO2']['var2plot'] = \
        {\
        'hur'     : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'rneb'    : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'Q1'      : {'xmin':   -2.  , 'xmax':   12.  , 'lev':'pf'},\
        'Q2'      : {'xmin':   -2.  , 'xmax':   12.  ,'lev':'pf'},\
        }

plotAvgP['MJO2_bias'] = \
        {\
        'tmin'    : cdtime.comptime(2011,11,20,0),\
        'tmax'    : cdtime.comptime(2011,12,1,0) ,\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'MJO2 (11/20-12/01) - Bias'  ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['MJO2_bias']['var2plot'] = \
        {\
        'theta'   : {'xmin':   -5.  , 'xmax':   5. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'qv'      : {'xmin':   -3.  , 'xmax':   3. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        }        

# 3rd suppressed phase

plotAvgP['SUPP3'] = \
        {\
        'tmin'    : cdtime.comptime(2011,12,1,0) ,\
        'tmax'    : cdtime.comptime(2011,12,10,0),\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'SUPP3 (12/01-12/10)'        ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['SUPP3']['var2plot'] = \
        {\
        'hur'     : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'rneb'    : {'xmin':    0.  , 'xmax':  100. , 'lev':'pf'},\
        'Q1'      : {'xmin':   -2.  , 'xmax':    5. , 'lev':'pf'},\
        'Q2'      : {'xmin':   -2.  , 'xmax':    5. ,'lev':'pf'},\
        }

plotAvgP['SUPP3_bias'] = \
        {\
        'tmin'    : cdtime.comptime(2011,12,1,0) ,\
        'tmax'    : cdtime.comptime(2011,12,10,0),\
        'ymin'    : 1000.                        ,\
        'ymax'    :   50.                        ,\
        'yname'   : 'Pressure (hPa)'             ,\
        'levunits': 'hPa'                        ,\
        'title'   : 'SUPP3 (12/01-12/10) - Bias' ,\
        'lgrid'   : True                         ,\
        }
plotAvgP['SUPP3_bias']['var2plot'] = \
        {\
        'theta'   : {'xmin':   -5.  , 'xmax':   5. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        'qv'      : {'xmin':   -3.  , 'xmax':   3. , 'lev':'pf', 'lbias':True, 'refdataset':'CSU'},\
        }        
        
#################
# Checking initial profiles - global view

plotInitGV = \
        {\
        'ymin'    : 1000.           ,\
        'ymax'    :   50.           ,\
        'yname'   : 'Pressure (hPa)',\
        'levunits': 'hPa'           ,\
        }

plotInitGV['var2plot'] = \
        {\
        'u'    : {'xmin': -20., 'xmax':   10., 'lev':'pf'},\
        'v'    : {'xmin': -20., 'xmax':   20., 'lev':'pf'},\
        'theta': {'xmin': 295., 'xmax':  400., 'lev':'pf'},\
        'qv'   : {'xmin':   0., 'xmax':   18., 'lev':'pf'},\
        'qv'   : {'xmin':  -1., 'xmax':   18., 'lev':'pf'},\
        'ql'   : {'xmin':  -1., 'xmax':  200., 'lev':'pf'},\
        'qi'   : {'xmin':  -1., 'xmax':  200., 'lev':'pf'},\
        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'pf'},\
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
#        'u'    : {'xmin': -20., 'xmax':   10., 'lev':'pf'},\
#        'v'    : {'xmin': -20., 'xmax':   10., 'lev':'pf'},\
#        'theta': {'xmin': 295., 'xmax':  325., 'lev':'pf'},\
#        'qv'   : {'xmin':   0., 'xmax':   18., 'lev':'pf'},\
#        'qv'   : {'xmin':  -1., 'xmax':   18., 'lev':'pf'},\
#        'ql'   : {'xmin':  -1., 'xmax':  200., 'lev':'pf'},\
#        'qi'   : {'xmin':  -1., 'xmax':  200., 'lev':'pf'},\
#        'tke'  : {'xmin':  -1., 'xmax':    2., 'lev':'pf'},\
        }

