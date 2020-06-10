# Describe where to find the case driver files
# All cases have at least one subcase, which is REF if not really relevant

import os

# If ldebug=True, a few information about the cases are printed
ldebug = False

####################################
#### Some initialization
# List of cases
cases = []
# Dictionnary (case, list of subcases)
subcases = {}
# 2-level dictionnary (case, list of (subcase, driver file))
data_input = {}

# Get the CASES directory
rep0 = os.getenv('REP_EMS') + '/CASES/'
if ldebug:
    print '-'*60
    print '-'*10, 'Some debug information from EMS_cases.py'
    print "CASES directory:", rep0


####################################
#### Definition of cases
####################################

####################################
#### Stable boundary-layer cases

# GABLS4 Case
case = 'GABLS4'
cases.append(case)
subcases[case] = ['OA_FLUX_USTAR','OA_10Z0']
data_input[case] = {}
data_input[case]['Stage3'] = rep0 + '/GABLS4/GABLS4_24h_driver_FC_RR_flux_z03.nc'
data_input[case]['Stage2'] = rep0 + '/GABLS4/stage2/GABLS4_SCM_LES_STAGE2_RR.nc'
data_input[case]['OA_FLUX_USTAR'] = rep0 + '/GABLS4/Olivier/GABLS4_24h_driver_FC_RR_flux_ustar.nc'
data_input[case]['OA_10Z0'] = rep0 + '/GABLS4/Olivier/driver_10h_flux_z0_MEAN.nc'

####################################
#### Dry convection cases

# AYOTTE Cases
case = 'AYOTTE'
cases.append(case)
subcases[case] = ['00SC','00WC','03SC','05SC','05WC','24F','24SC','24SC_us','24SC_modif','24SC_modif_us']
data_input[case] = {}
for cc in subcases[case]:
  data_input[case][cc] = rep0 + '/AYOTTE/AYOTTE_A{0}_driver_FC_RR.nc'.format(cc)

data_input[case]['24SC_us'] = rep0 + '/AYOTTE/AYOTTE_A24SC_driver_FC_RR_us.nc'.format(cc)
data_input[case]['24SC_modif'] = rep0 + '/AYOTTE/AYOTTE_A24SC_modif_driver_FC_RR.nc'.format(cc)
data_input[case]['24SC_modif_us'] = rep0 + '/AYOTTE/AYOTTE_A24SC_modif_driver_FC_RR_us.nc'.format(cc)

# IHOP Cases
case = 'IHOP'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/IHOP/IHOP_driver_FC_RR.nc'

# AMMAsec Cases
case = 'AMMAsec'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/AMMAsec/AMMAsec_driver_FC_RR.nc'

# WANGARA Cases
case = 'WANGARA'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/WANGARA/WANGARA_driver_FC_RR.nc'

####################################
#### Shallow convection cases

# SCMS Cases
case = 'SCMS'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/SCMS/SCMS_driver_FC_RR.nc'

# RICO Case
case = 'RICO'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/RICO/rico_driver_RR.nc'
data_input[case]['DEPHY'] = rep0 + '/RICO/rico_driver_RR_new3.nc'
data_input[case]['DEPHY2'] = rep0 + '/RICO/RICO_SHORT_SCM_driver.nc'

# ARMCU Case
case = 'ARMCU'
cases.append(case)
subcases[case] = ['REF']
#subcases[case] = ['REF','DEPHY','DEPHY2']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/ARMCU/ARMCu_driver_RR.nc'
data_input[case]['DEPHY'] = rep0 + '/ARMCU/ARMCu_driver_RR_new.nc'
data_input[case]['DEPHY2'] = rep0 + '/ARMCU/ARMCU_REF_SCM_driver.nc'

# BOMEX Case
case = 'BOMEX'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/BOMEX/BOMEX_driver_MPL_RR.nc'

####################################
#### Stratocumulus cases

# FIRE Case
case = 'FIRE'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
#data_input[case]['REF'] = rep0 + '/FIRE/Fire-I_driver_RR_v3.nc'
data_input[case]['REF'] = rep0 + '/FIRE/Fire-I_driver_RR.nc'

# DYCOMS Case
case = 'DYCOMS'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/DYCOMS/DYCOMS_driver_FC_RR.nc'

# ASTEX cases
case = 'ASTEX'
cases.append(case)
subcases[case] = ['EUCLIPSE']
data_input[case] = {}
data_input[case]['EUCLIPSE'] = rep0 + '/ASTEX/ASTEX_GASS-EUCLIPSE_driver_RR.nc'

# SANDU composite cases
case = 'SANDU'
cases.append(case)
subcases[case] = ['REF','FAST','SLOW']
#subcases[case] = ['REF','FAST','SLOW','REF_DEPHY']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/SANDU/Composite_REF_driver_RR.nc'
data_input[case]['REF_DEPHY'] = rep0 + '/SANDU/Composite_REF_driver_RR_new.nc'
data_input[case]['FAST'] = rep0 + '/SANDU/Composite_FAST_driver_RR.nc'
data_input[case]['SLOW'] = rep0 + '/SANDU/Composite_SLOW_driver_RR.nc'

####################################
#### Deep convection cases

# LBA case
case = 'LBA'
cases.append(case)
subcases[case] = ['REF','MesoNH']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/LBA/LBA_driver_FC_RR.nc'
data_input[case]['MesoNH'] = rep0 + '/LBA/LBA_driver_MesoNH_RR.nc'

# ARMCVP case
case = 'ARMCVP'
cases.append(case)
subcases[case] = ['REF','NoRad','omg']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/ARMCVP/ARMCVP_FG.nc'
data_input[case]['NoRad'] = rep0 + '/ARMCVP/ARMCVP_FG_norad.nc'
data_input[case]['omg'] = rep0 + '/ARMCVP/ARMCVP_FG_omg.nc'

# AMMA Cases
case = 'AMMA'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/AMMA/AMMA_20060710_setupI_driver_RR.nc'

# CINDY-DYNAMO cases
case = 'CINDY-DYNAMO'
cases.append(case)
#subcases[case] = ['NSA3a','NSA3aflux','SSA3a']
#subcases[case] = ['NSA3a']
#subcases[case] = ['NSA3aflux']
#subcases[case] = ['Revelle-ARM-CSU-13Nov']
#subcases[case] = ['Revelle-PE-13Nov']
#subcases[case] = ['COCOA-13Nov']
#subcases[case] = ['COCOA-13-22Nov']
#subcases[case] = ['COCOA']
subcases[case] = ['COCOA-MJO1']
data_input[case] = {}
data_input[case]['NSA3a'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3a_driver_RR.nc'
data_input[case]['NSA3aflux'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3aflux_driver_RR.nc'
data_input[case]['NSA3aflux-MJO1'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3aflux_driver_RR_MJO1.nc'
data_input[case]['SSA3a'] = rep0 + '/CINDY-DYNAMO/SSA3a/cindy-dynamo-SSA3a_driver_RR.nc'
data_input[case]['Revelle-ARM-CSU-leg3'] = rep0 + '/CINDY-DYNAMO/RevelleARM1/CINDY-DYNAMO_Revelle-ARM-CSU-leg3_driver_RR_extended.nc'
data_input[case]['Revelle-ARM-CSU-13Nov'] = rep0 + '/CINDY-DYNAMO/RevelleARM1/CINDY-DYNAMO_Revelle-ARM-CSU-13Nov_driver_RR_extended.nc'
data_input[case]['Revelle-PE-13Nov'] = rep0 + '/CINDY-DYNAMO/RevellePE/CINDY-DYNAMO_Revelle-PE-13Nov_driver_RR.nc'
data_input[case]['COCOA'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU_50km_driver_RR_extended.nc'
data_input[case]['COCOA-13Nov'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU-13Nov_50km_driver_RR_extended.nc'
data_input[case]['COCOA-13-22Nov'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU-13-22Nov_50km_driver_RR_extended.nc'
data_input[case]['COCOA-MJO1'] = rep0 + '/CINDY-DYNAMO/COCOA/CINDY-DYNAMO_Revelle-ARM-CSU-MJO1_50km_driver_RR_extended.nc'

# Derbyshire cases
case = 'Derbyshire'
cases.append(case)
subcases[case] = ['RH25','RH50','RH70','RH90']
data_input[case] = {}
data_input[case]['RH25'] = rep0 + '/Derbyshire/Derbyshire_RH25_driver_RR.nc'
data_input[case]['RH50'] = rep0 + '/Derbyshire/Derbyshire_RH50_driver_RR.nc'
data_input[case]['RH70'] = rep0 + '/Derbyshire/Derbyshire_RH70_driver_RR.nc'
data_input[case]['RH90'] = rep0 + '/Derbyshire/Derbyshire_RH90_driver_RR.nc'

# RCE-MIP
case = 'RCEMIP'
cases.append(case)
subcases[case] = ['SST295','SST300','SST305','SST301.15']
data_input[case] = {}
for SST in [295,300,305,301.15]:
  data_input[case]['SST{0}'.format(SST)] = rep0 + '/RCEMIP/RCEMIP_SST{0}.nc'.format(SST)

# RCE
case = "RCE"
cases.append(case)
subcases[case] = []
data_input[case] = {}
for SST in [28,]:
  subcases[case] = subcases[case] + ['KUANG_SST{0}_DEPHY'.format(SST),]
  data_input[case]['KUANG_SST{0}_DEPHY'.format(SST)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_DEPHY.nc'.format(SST)

  subcases[case] = subcases[case] + ['KUANG_SST{0}_TP05_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_TP02_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_TM05_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_TM02_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QP02_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QP01_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QM02_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)]  
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QM01_l{1:0>2}_DEPHY'.format(SST,lev) for lev in range(91,39,-1)] 
  subcases[case] = subcases[case] + ['KUANG_SST{0}_TP05_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_TP02_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_TM05_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_TM02_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QP02_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QP01_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QM02_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]  
  subcases[case] = subcases[case] + ['KUANG_SST{0}_QM01_l{1:0>2}_DEPHY_1d'.format(SST,lev) for lev in range(91,39,-1)]   
  for lev in range(91,39,-1):
      data_input[case]['KUANG_SST{0}_TP05_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TP05_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_TP02_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TP02_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_TM05_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TM05_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_TM02_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TM02_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QP02_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QP02_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QP01_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QP01_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QM02_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QM02_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QM01_l{1:0>2}_DEPHY'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QM01_l{1:0>2}_DEPHY.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_TP05_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TP05_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_TP02_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TP02_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_TM05_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TM05_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_TM02_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_TM02_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QP02_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QP02_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QP01_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QP01_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QM02_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QM02_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)
      data_input[case]['KUANG_SST{0}_QM01_l{1:0>2}_DEPHY_1d'.format(SST,lev)] = rep0 + '/RCE/KUANG/KUANG_SST{0}_QM01_l{1:0>2}_DEPHY_1d.nc'.format(SST,lev)

####################################

if ldebug:
  for cc in cases:
      for ss in subcases[cc]:
          print cc, ss, data_input[cc][ss]
  print '-'*60
