from cases import * # cases and subcases

files = {}
varnames = {}
coefs = {}
lines = {}

for case in  cases:
  files[case] = {}
  for subcase in subcases[case]:
    files[case][subcase] = {}

#################

sim2plot = ['CMIP6',]
name_atlas = 'CMIP6'

#sim2plot = ['CONV1_TEST-010','CMIP6',]
#name_atlas = 'CONV1_TEST-010'

#sim2plot = ['CMIP6','CONV2_TEST-005','CONV2_TEST-010']
#name_atlas = 'CONV2_TEST'


#sim2plot = ['CMIP6_bestcf','CMIP6']
#name_atlas = 'CMIP6_bestcf'

#sim2plot = ['CMIP6_pcmt_num','CMIP6',]
#name_atlas = 'CMIP6_pcmt_num'

#sim2plot = ['CMIP6_pcmt_num_jfg','CMIP6',]
#name_atlas = 'CMIP6_pcmt_num_jfg'

#sim2plot = ['CMIP6_N0r','CMIP6',]
#name_atlas = 'CMIP6_N0r'


#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_ECMNP04','CMIP6_pcmt_num_ECMNP16','CMIP6_pcmt_num_GCVRE05','CMIP6_pcmt_num_RQLCR4',]
#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_GCVRE05','CMIP6_pcmt_num_RQLCR4','CMIP6_pcmt_num_GCVTEXC200','CMIP6_pcmt_num_GCVTEXC200_RQLCR4_GCVRE05']
#name_atlas = 'CMIP6_pcmt_num_all'

#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_tmp',]
#name_atlas = 'CMIP6_pcmt_num_tmp'

#sim2plot = ['CMIP6_pcmt_num','CMIP6_pcmt_num_tmp2',]
#name_atlas = 'CMIP6_pcmt_num_tmp2'

#sim2plot = ['CMIP6','CMIP6_pcmt','CMIP6_pcmt0']
#name_atlas = 'CMIP6_pcmt'


#################
# files and some possible "corrections" of variables

for sim in sim2plot:
  varnames[sim] = {}
  coefs[sim] = {}
  coefs[sim]['shf'] = -1.
  coefs[sim]['lhf'] = -1.

for sim in ['CMIP6','CMIP6_bestcf',]:
  for case in cases:
    for subcase in subcases[case]:
      files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)
      if case == 'CINDY-DYNAMO':
        files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_900s/{1}/{2}/Output/netcdf/Out_daily_plevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_N0r' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/CMIP6.N0r/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num/CMIP6/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt_num_jfg' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag.pcmt.num.jfg/CMIP6/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

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

sim = 'CONV1_TEST-010' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)

sim = 'CONV2_TEST-005' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_900s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)
    if case == 'CINDY-DYNAMO':
        files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_900s/{1}/{2}/Output/netcdf/Out_daily_plevel.nc'.format(sim,case,subcase)


sim = 'CONV2_TEST-010' # Avoid dots in the simulation name
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_900s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)
    if case == 'CINDY-DYNAMO':
        files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.diag/{0}/L91_900s/{1}/{2}/Output/netcdf/Out_daily_plevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt'
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.pcmt/CMIP6/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)
    if case == 'CINDY-DYNAMO':
        files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.pcmt/CMIP6/L91_900s/{1}/{2}/Output/netcdf/Out_daily_plevel.nc'.format(sim,case,subcase)

sim = 'CMIP6_pcmt0'
for case in cases:
  for subcase in subcases[case]:
    files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.pcmt0/CMIP6/L91_300s/{1}/{2}/Output/netcdf/Out_klevel.nc'.format(sim,case,subcase)
    if case == 'CINDY-DYNAMO':
        files[case][subcase][sim] =  '/Users/romainroehrig/MUSC/simulations//arp631.pcmt0/CMIP6/L91_900s/{1}/{2}/Output/netcdf/Out_daily_plevel.nc'.format(sim,case,subcase)


#################
# Lines

lines['CMIP6'] = 'r-'
lines['CMIP6_bestcf'] = 'b-'
lines['CMIP6_N0r'] = 'b-'
lines['CMIP6_pcmt_num'] = 'b-'
lines['CMIP6_pcmt_num_jfg'] = 'b-'
lines['CMIP6_pcmt_num_tmp'] = 'r-'
lines['CMIP6_pcmt_num_tmp2'] = 'r-'
lines['CMIP6_pcmt_num_ECMNP04'] = 'green'
lines['CMIP6_pcmt_num_ECMNP16'] = 'orange'
lines['CMIP6_pcmt_num_GCVRE05'] = 'magenta'
lines['CMIP6_pcmt_num_RQLCR4'] = 'cyan'
lines['CMIP6_pcmt_num_GCVTEXC200'] = 'green'
lines['CMIP6_pcmt_num_GCVTEXC500'] = 'green'
lines['CMIP6_pcmt_num_GCVTEXC200_RQLCR4_GCVRE05'] = 'orange'
lines['CONV1_TEST-010'] = 'r-'
lines['CONV2_TEST-005'] = 'b-'
lines['CONV2_TEST-010'] = 'g-'
lines['CMIP6_pcmt'] = 'b-'
lines['CMIP6_pcmt0'] = 'g-'
