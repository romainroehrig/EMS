import sys
import EMS_cases as CC

# model : ARPCLIMAT, AROME, AROME46t1, ARPPNT
#model = 'AROME'
#model = 'AROME46t1'
#model = 'ARPPNT'
model = 'ARPCLIMAT'

# If True, all available cases (defined in cases.py) are run
# If False, only the list specified here (in 'cases' list) are run
#allcases=True
allcases=False

#cases = ['AMMA','RICO','FIRE','ARMCU','AYOTTE']
cases = ['AYOTTE']

if model in ['ARPCLIMAT',]:
  # Number of vertical level
  nlev = 91
  # Model time step in seconds
  timestep = 300
elif model in ['AROME','AROME46t1',]:
  # Number of vertical level
  nlev= 90
  # Model time step in seconds
  timestep = 50
elif model in ['ARPPNT',]:
  # Number of vertical level
  nlev= 90
#  nlev= 105
  # Model time step in seconds
  timestep = 360
#  # Number of vertical level
else:
  print 'unknown model:', model
  sys.exit()

# Test that asked cases are available
for cc in cases:
  if not(cc in CC.cases):
    print 'case', cc, 'not available'
    print 'available cases:', CC.cases
    sys.exit()

lsurfex = True
if model in ['AROME','AROME46t1','ARPPNT']:
    lsurfex = False
