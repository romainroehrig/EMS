#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
Describe where to find the case driver files
All cases have at least one subcase, which is REF if not really relevant
"""

import os
import logging
logger = logging.getLogger(__name__)

####################################
#### Some initialization
# List of cases
cases = []
# Dictionnary (case, list of subcases)
subcases = {}
# 2-level dictionnary (case, list of (subcase, driver file))
data_input = {}

# Get the CASES directory
import ems
rep0 = os.path.join(ems._dirEMS,'../data/CASES') 

# Some debug information
logger.debug('-'*60)
logger.debug('-'*10 + ' Some debug information from EMS_cases.py')
logger.debug("CASES directory: " + rep0)


####################################
#### Definition of cases
####################################

####################################
#### Stable boundary-layer cases

# GABLS1 Case
case = 'GABLS1'
cases.append(case)
subcases[case] = ['REF', 'MESONH']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# GABLS4 Case
case = 'GABLS4'
cases.append(case)
subcases[case] = ['STAGE3', 'STAGE3-SHORT']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

####################################
#### Dry convection cases

# AYOTTE Cases
case = 'AYOTTE'
cases.append(case)
#subcases[case] = ['00SC', '00WC', '03SC', '05SC', '05WC', '24F', '24SC']
subcases[case] = ['00SC', '00WC', '03SC', '05SC', '05WC', '24SC']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# IHOP Cases
case = 'IHOP'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# AMMAsec Cases
#case = 'AMMAsec'
#cases.append(case)
#subcases[case] = ['REF']
#data_input[case] = {}
#data_input[case]['REF'] = rep0 + '/AMMAsec/AMMAsec_driver_FC_RR.nc'

# WANGARA Cases
#case = 'WANGARA'
#cases.append(case)
#subcases[case] = ['REF']
#data_input[case] = {}
#data_input[case]['REF'] = rep0 + '/WANGARA/WANGARA_driver_FC_RR.nc'

####################################
#### Shallow convection cases

# SCMS Cases
case = 'SCMS'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# RICO Case
case = 'RICO'
cases.append(case)
subcases[case] = ['SHORT', 'MESONH']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# ARMCU Case
case = 'ARMCU'
cases.append(case)
subcases[case] = ['REF', 'MESONH', 'E3SM']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# BOMEX Case
case = 'BOMEX'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# MPACE Case
case = 'MPACE'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}


####################################
#### Stratocumulus cases

# FIRE Case
case = 'FIRE'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# DYCOMS Case
#case = 'DYCOMS'
#cases.append(case)
#subcases[case] = ['REF']
#data_input[case] = {}
#data_input[case]['REF'] = rep0 + '/DYCOMS/DYCOMS_driver_FC_RR.nc'

# ASTEX cases
#case = 'ASTEX'
#cases.append(case)
#subcases[case] = ['EUCLIPSE']
#data_input[case] = {}
#data_input[case]['EUCLIPSE'] = rep0 + '/ASTEX/ASTEX_GASS-EUCLIPSE_driver_RR.nc'

# SANDU composite cases
case = 'SANDU'
cases.append(case)
subcases[case] = ['REF', 'FAST', 'SLOW']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

####################################
#### Deep convection cases

# LBA case
#case = 'LBA'
#cases.append(case)
#subcases[case] = ['REF','MesoNH']
#data_input[case] = {}
#data_input[case]['REF'] = rep0 + '/LBA/LBA_driver_FC_RR.nc'
#data_input[case]['MesoNH'] = rep0 + '/LBA/LBA_driver_MesoNH_RR.nc'

# ARMCVP case
#case = 'ARMCVP'
#cases.append(case)
#subcases[case] = ['REF','NoRad','omg']
#data_input[case] = {}
#data_input[case]['REF'] = rep0 + '/ARMCVP/ARMCVP_FG.nc'
#data_input[case]['NoRad'] = rep0 + '/ARMCVP/ARMCVP_FG_norad.nc'
#data_input[case]['omg'] = rep0 + '/ARMCVP/ARMCVP_FG_omg.nc'

# AMMA case
case = 'AMMA'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {subcase: rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)
                             for subcase in subcases[case]}

# CINDY-DYNAMO cases
#case = 'CINDY-DYNAMO'
#cases.append(case)
#subcases[case] = ['NSA3a','NSA3aflux','SSA3a']
#subcases[case] = ['NSA3a']
#subcases[case] = ['NSA3aflux']
#subcases[case] = ['Revelle-ARM-CSU-13Nov']
#subcases[case] = ['Revelle-PE-13Nov']
#subcases[case] = ['COCOA-13Nov']
#subcases[case] = ['COCOA-13-22Nov']
#subcases[case] = ['COCOA']
#subcases[case] = ['COCOA-MJO1']
#data_input[case] = {}
#data_input[case]['NSA3a'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3a_driver_RR.nc'
#data_input[case]['NSA3aflux'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3aflux_driver_RR.nc'
#data_input[case]['NSA3aflux-MJO1'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3aflux_driver_RR_MJO1.nc'
#data_input[case]['SSA3a'] = rep0 + '/CINDY-DYNAMO/SSA3a/cindy-dynamo-SSA3a_driver_RR.nc'
#data_input[case]['Revelle-ARM-CSU-leg3'] = rep0 + '/CINDY-DYNAMO/RevelleARM1/CINDY-DYNAMO_Revelle-ARM-CSU-leg3_driver_RR_extended.nc'
#data_input[case]['Revelle-ARM-CSU-13Nov'] = rep0 + '/CINDY-DYNAMO/RevelleARM1/CINDY-DYNAMO_Revelle-ARM-CSU-13Nov_driver_RR_extended.nc'
#data_input[case]['Revelle-PE-13Nov'] = rep0 + '/CINDY-DYNAMO/RevellePE/CINDY-DYNAMO_Revelle-PE-13Nov_driver_RR.nc'
#data_input[case]['COCOA'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU_50km_driver_RR_extended.nc'
#data_input[case]['COCOA-13Nov'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU-13Nov_50km_driver_RR_extended.nc'
#data_input[case]['COCOA-13-22Nov'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU-13-22Nov_50km_driver_RR_extended.nc'
#data_input[case]['COCOA-MJO1'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU-MJO1_50km_driver_RR_extended.nc'

case = 'DYNAMO'
cases.append(case)
subcases[case] = ['NSA3A', 'NSA3A_D1', 'NSA3A_D30', 'NSA3A_MJO1']
data_input[case] = {}
for subcase in subcases[case]:
    data_input[case][subcase] = rep0 + '/{0}_{1}_SCM_driver.nc'.format(case,subcase)

# Derbyshire cases
#case = 'Derbyshire'
#cases.append(case)
#subcases[case] = ['RH25','RH50','RH70','RH90']
#data_input[case] = {}
#data_input[case]['RH25'] = rep0 + '/Derbyshire/Derbyshire_RH25_driver_RR.nc'
#data_input[case]['RH50'] = rep0 + '/Derbyshire/Derbyshire_RH50_driver_RR.nc'
#data_input[case]['RH70'] = rep0 + '/Derbyshire/Derbyshire_RH70_driver_RR.nc'
#data_input[case]['RH90'] = rep0 + '/Derbyshire/Derbyshire_RH90_driver_RR.nc'

# RCE-MIP
#case = 'RCEMIP'
#cases.append(case)
#subcases[case] = ['SST295','SST300','SST305','SST301.15','SST295_DEPHY','SST300_DEPHY','SST305_DEPHY','SST301.15_DEPHY']
#data_input[case] = {}
#for SST in [295,300,305,301.15]:
#  data_input[case]['SST{0}'.format(SST)] = rep0 + '/RCEMIP/RCEMIP_SST{0}.nc'.format(SST)
#  data_input[case]['SST{0}_DEPHY'.format(SST)] = rep0 + '/RCEMIP/RCEMIP_SST{0}_DEPHY.nc'.format(SST)

# RCE
#case = "RCE"
#cases.append(case)
#subcases[case] = []
#data_input[case] = {}
#for SST in [28,]:
#  subcases[case] = subcases[case] + ['KUANG_SST{0}_DEPHY'.format(SST),]
#  data_input[case]['KUANG_SST{0}_DEPHY'.format(SST)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_DEPHY.nc'.format(SST)

#  subcases[case] = subcases[case] + ['KUANG_SST{0}_TP05_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(90,39,-1)]
#  subcases[case] = subcases[case] + ['KUANG_SST{0}_TM05_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(90,39,-1)]
#  subcases[case] = subcases[case] + ['KUANG_SST{0}_QP02_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(90,39,-1)]
#  subcases[case] = subcases[case] + ['KUANG_SST{0}_QM02_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(90,39,-1)]  
#  for lev in range(90,39,-1):
#      data_input[case]['KUANG_SST{0}_TP05_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TP05_l{1:0>2}_DEPHY.nc'.format(SST,lev)
#      data_input[case]['KUANG_SST{0}_TM05_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TM05_l{1:0>2}_DEPHY.nc'.format(SST,lev)
#      data_input[case]['KUANG_SST{0}_QP02_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QP02_l{1:0>2}_DEPHY.nc'.format(SST,lev)
#      data_input[case]['KUANG_SST{0}_QM02_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QM02_l{1:0>2}_DEPHY.nc'.format(SST,lev)

####################################


def available(case=None):
    """
    List available cases/subcases.
    """

    if case is None:
        logger.info('-'*30 + ' Available cases')
        for cc in cases:
            for ss in subcases[cc]:
                logger.info('{0} {1} {2}'.format(cc, ss, data_input[cc][ss]))
        logger.info('-'*60)
    else:
        logger.info('-'*30 + ' Available subcase for case = ' + case)
        for ss in subcases[case]:
            logger.info('{0} {1} {2}'.format(case, ss, data_input[case][ss]))
        logger.info('-'*60)



def check(case,subcase):
    if not(case in cases):
        logger.error('case {0} is not known'.format(case))
        logger.error('known cases: ' + cases)
        available()
        raise ValueError

    if not(subcase in subcases[case]):
        logging.error('subcase {0} is not known for case {1}'.format(subcase,case))
        logging.error('known subcases for case {0}: {1}'.format(case, ' '.join(subcases[case])))
        available(case)
        raise ValueError


for cc in cases:
    for ss in subcases[cc]:
        logger.debug('{0} {1} {2}'.format(cc, ss, data_input[cc][ss]))
logger.debug('-'*60)

