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

# CINDY-DYNAMO cases
case = 'CINDY-DYNAMO'
cases.append(case)
subcases[case] = ['NSA3a','NSA3aflux','SSA3a']
#subcases[case] = ['NSA3aflux','SSA3a']
subcases[case] = ['NSA3a']
data_input[case] = {}
data_input[case]['NSA3a'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3a_driver_RR.nc'
data_input[case]['NSA3aflux'] = rep0 + '/CINDY-DYNAMO/NSA3a/cindy-dynamo-NSA3aflux_driver_RR.nc'
data_input[case]['SSA3a'] = rep0 + '/CINDY-DYNAMO/SSA3a/cindy-dynamo-SSA3a_driver_RR.nc'

# AMMA Cases
case = 'AMMA'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/AMMA/AMMA_20060710_setupI_driver_RR.nc'

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

# SCMS Cases
case = 'SCMS'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/SCMS/SCMS_driver_FC_RR.nc'

# FIRE Case
case = 'FIRE'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/FIRE/Fire-I_driver_RR.nc'

# DYCOMS Case
case = 'DYCOMS'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/DYCOMS/DYCOMS_driver_FC_RR.nc'

# RICO Case
case = 'RICO'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/RICO/rico_driver_RR.nc'

# ARMCU Case
case = 'ARMCU'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/ARMCu/ARMCu_driver_RR.nc'

# BOMEX Case
case = 'BOMEX'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/BOMEX/BOMEX_driver_MPL_RR.nc'

# AYOTTE Cases
case = 'AYOTTE'
cases.append(case)
subcases[case] = ['00SC','00WC','03SC','05SC','05WC','24F','24SC']
data_input[case] = {}
for cc in subcases[case]:
  data_input[case][cc] = rep0 + '/AYOTTE/AYOTTE_{0}_driver_FC_RR.nc'.format(cc)

# Derbyshire cases
case = 'Derbyshire'
cases.append(case)
subcases[case] = ['RH25','RH50','RH70','RH90']
data_input[case] = {}
data_input[case]['RH25'] = rep0 + '/Derbyshire/Derbyshire_RH25_driver_RR.nc'
data_input[case]['RH50'] = rep0 + '/Derbyshire/Derbyshire_RH50_driver_RR.nc'
data_input[case]['RH70'] = rep0 + '/Derbyshire/Derbyshire_RH70_driver_RR.nc'
data_input[case]['RH90'] = rep0 + '/Derbyshire/Derbyshire_RH90_driver_RR.nc'


if ldebug:
  for cc in cases:
      for ss in subcases[cc]:
          print cc, ss, data_input[cc][ss]
  print '-'*60
