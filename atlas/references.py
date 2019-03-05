from cases import * # cases and subcases

files = {}
varnames = {}
coefs = {}
lines = {}

for case in  cases:
  files[case] = {}
  varnames[case] = {}
  coefs[case] = {}
  lines[case] = {}

  for subcase in subcases[case]:
    files[case][subcase] = {}
    varnames[case][subcase] = {}
    coefs[case][subcase] = {}
    lines[case][subcase] = {}

#################
# AYOTTE
case = 'AYOTTE'

for subcase in ['00SC','00WC','03SC','05SC','05WC','24F','24SC']:
  files[case][subcase] = {}
  varnames[case][subcase] = {}
  coefs[case][subcase] = {}
  lines[case][subcase] = {}

  sim = 'LES'
  files[case][subcase][sim] = '/Users/romainroehrig/data/LES/AYOTTE/AYOTTE{0}_LES_MESONH_RR.nc'.format(subcase)
  varnames[case][subcase][sim] = {}
  coefs[case][subcase][sim] = {}
  lines[case][subcase][sim] = 'k'

  sim = 'LES_csam'
  files[case][subcase][sim] = '/Users/romainroehrig/data/LES/AYOTTE/AYOTTE{0}_LES_MESONH_RR_csam.nc'.format(subcase)
  varnames[case][subcase][sim] = {}
  coefs[case][subcase][sim] = {}
  lines[case][subcase][sim] = 'k.'

#################
# ARMCU
case = 'ARMCU'
subcase = 'REF'

#sim = 'LES_1h'
#files[case][subcase][sim] = '/Users/romainroehrig/data/HighTune/LES//HTUNE_CERK4.1.ARMCU.000KCL_4D_new.nc'
#varnames[case][subcase][sim] = {}
#coefs[case][subcase][sim] = {}
#lines[case][subcase][sim] = 'k-.'

sim = 'LES_5min'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/ARMCU/ARMCU_LES_MESONH_RR.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k'

sim = 'BLM_csam'
files[case][subcase][sim] = '/Users/romainroehrig/data/HighTune/LES/ARMCU_BOMEX_BLM/ARMCU_csam.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k.'

sim = 'BLM_cld'
files[case][subcase][sim] = '/Users/romainroehrig/data/HighTune/LES/ARMCU_BOMEX_BLM/ARMCU_cld.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'b.'

sim = 'BLM_cor'
files[case][subcase][sim] = '/Users/romainroehrig/data/HighTune/LES/ARMCU_BOMEX_BLM/ARMCU_cor.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'g.'

#################
# FIRE
case = 'FIRE'
subcase = 'REF'

sim = 'LES'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/FIRE/FIRE_LES_MESONH_RR.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k'

#################
# SANDU
case = 'SANDU'

for subcase in ['REF','SLOW','FAST']:
  files[case][subcase] = {}
  varnames[case][subcase] = {}
  coefs[case][subcase] = {}
  lines[case][subcase] = {}

  sim = 'LES'
  files[case][subcase][sim] = '/Users/romainroehrig/scripts/SCM/Sandu/LES/SANDU_{0}_LES_SAM_RR.nc'.format(subcase)
  varnames[case][subcase][sim] = {}
  coefs[case][subcase][sim] = {}
  lines[case][subcase][sim] = 'k'

#################
# BOMEX
case = 'BOMEX'
subcase = 'REF'

sim = 'LES'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/BOMEX/BOMEX_LES_MESONH_RR.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k'

sim = 'LES_core'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/BOMEX/BOMEX_LES_MESONH_RR_core.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'g.'

sim = 'LES_csam'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/BOMEX/BOMEX_LES_MESONH_RR_csam.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k.'

#################
# RICO
case = 'RICO'
subcase = 'REF'

sim = 'LES'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/RICO/RICO_LES_MESONH_RR.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k'

sim = 'LES_core'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/RICO/RICO_LES_MESONH_RR_core.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'g.'

sim = 'LES_csam'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/RICO/RICO_LES_MESONH_RR_csam.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k.'

#################
# AMMA
case = 'AMMA'
subcase = 'REF'

sim = 'LES'
files[case][subcase][sim] = '/Users/romainroehrig/data/LES/AMMA/AMMA_LES_MESONH_RR.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k'


#################
# AMMA
case = 'CINDY-DYNAMO'
subcase = 'NSA3aflux'

sim = 'CSU'
files[case][subcase][sim] = '/Users/romainroehrig/data/CindyDynamo/NSA3a/CINDY-DYNAMO_NSA3a_RR_daily.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k'

sim = 'TRMM'
files[case][subcase][sim] = '/Users/romainroehrig/data/CindyDynamo/NSA3a/CINDY-DYNAMO_NSA3a_TRMM_RR_daily.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k'

sim = 'Q1-derived'
files[case][subcase][sim] = '/Users/romainroehrig/data/CindyDynamo/NSA3a/CINDY-DYNAMO_NSA3a_Q1-derived_RR_daily.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'r--'

sim = 'Q2-derived'
files[case][subcase][sim] = '/Users/romainroehrig/data/CindyDynamo/NSA3a/CINDY-DYNAMO_NSA3a_Q2-derived_RR_daily.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'b--'

sim = 'CERES'
files[case][subcase][sim] = '/Users/romainroehrig/data/CindyDynamo/NSA3a/CINDY-DYNAMO_NSA3a_CERES_RR_daily.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'k--'

sim = 'CERESv2'
files[case][subcase][sim] = '/Users/romainroehrig/data/CindyDynamo/NSA3a/CINDY-DYNAMO_NSA3a_CERES2_RR.nc'
varnames[case][subcase][sim] = {}
coefs[case][subcase][sim] = {}
lines[case][subcase][sim] = 'b--'
