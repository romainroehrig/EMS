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
# AMMA Cases
case = 'AMMA'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/AMMA/AMMA_20060710_setupI_driver_RR.nc'

# FIRE Case
case = 'FIRE'
cases.append(case)
subcases[case] = ['REF']
data_input[case] = {}
data_input[case]['REF'] = rep0 + '/FIRE/Fire-I_driver_RR.nc'

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

# AYOTTE Cases
case = 'AYOTTE'
cases.append(case)
subcases[case] = ['A00SC','A00WC','A03SC','A05SC','A05WC','A24F','A24SC']
subcases[case] = ['A24SC']
data_input[case] = {}
for cc in subcases[case]:
  data_input[case][cc] = rep0 + '/AYOTTE/AYOTTE_{0}_driver_FC_RR.nc'.format(cc)


if ldebug:
  for cc in cases:
      for ss in subcases[cc]:
          print cc, ss, data_input[cc][ss]
  print '-'*60
