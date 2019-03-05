from cases import * # cases and subcases

files = {}
varnames = {}
coefs = {}
lines = {}

#cases = ['AYOTTE','ARMCU','FIRE','BOMEX','RICO','AMMA','SANDU']
#subcases = {}
#for case in cases:
#    subcases[case] = ['REF',] 

#subcases['AYOTTE'] = ['00SC','00WC','03SC','05SC','05WC','24F','24SC']
#subcases['SANDU'] =  ['REF','FAST','SLOW']

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

sim2plot = ['CMIP6',]
name_atlas = 'CMIP6'

#sim2plot = ['CMIP6_pcmt_num','CMIP6',]
#name_atlas = 'CMIP6_pcmt_num'

#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_ECMNP04','CMIP6_pcmt_num_ECMNP16','CMIP6_pcmt_num_GCVRE05','CMIP6_pcmt_num_RQLCR4',]
#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_GCVRE05','CMIP6_pcmt_num_RQLCR4','CMIP6_pcmt_num_GCVTEXC200','CMIP6_pcmt_num_GCVTEXC200_RQLCR4_GCVRE05']
#name_atlas = 'CMIP6_pcmt_num_all'

#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_tmp',]
#name_atlas = 'CMIP6_pcmt_num_tmp'

#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_tmp2',]
#name_atlas = 'CMIP6_pcmt_num_tmp2'

#################
# files and some possible "corrections" of variables

for sim in sim2plot:
  varnames[sim] = {}
  coefs[sim] = {}
  coefs[sim]['shf'] = -1.
  coefs[sim]['lhf'] = -1.

for sim in ['CMIP6',]:
  for case in cases:
    for subcase in subcases[case]:
      files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)
      if case == 'CINDY-DYNAMO':
        files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_900s/{1}/{2}/Output/netcdf/Out_daily_plevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_ECMNP04' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.ECMNP04/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_ECMNP16' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.ECMNP16/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_GCVRE05' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.GCVRE05/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_RQLCR4' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.RQLCR4/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_GCVTEXC200' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.GCVTEXC200/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_GCVTEXC500' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.GCVTEXC500/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_GCVTEXC200_RQLCR4_GCVRE05' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.GCVTEXC200.RQLCR4.GCVRE05/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_tmp' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.tmp/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_tmp2' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6.tmp2/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

#################
# Lines

lines['CMIP6'] = 'r-'
lines['CMIP6_pcmt_num'] = 'b-'
lines['CMIP6_pcmt_num_tmp'] = 'r-'
lines['CMIP6_pcmt_num_tmp2'] = 'r-'
lines['CMIP6_pcmt_num_ECMNP04'] = 'green'
lines['CMIP6_pcmt_num_ECMNP16'] = 'orange'
lines['CMIP6_pcmt_num_GCVRE05'] = 'magenta'
lines['CMIP6_pcmt_num_RQLCR4'] = 'cyan'
lines['CMIP6_pcmt_num_GCVTEXC200'] = 'green'
lines['CMIP6_pcmt_num_GCVTEXC500'] = 'green'
lines['CMIP6_pcmt_num_GCVTEXC200_RQLCR4_GCVRE05'] = 'orange'
