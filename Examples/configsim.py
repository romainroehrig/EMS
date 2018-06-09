import sys
import EMS_cases as CC

# model : ARPCLIMAT, AROME (ARPPNT a prevoir si besoin)
#model = 'AROME'
#model = 'ARPCLIMAT'
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
elif model in ['AROME',]:
  # Number of vertical level
  nlev= 90
  # Model time step in seconds
  timestep = 50
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
if model in ['AROME','ARPPNT']:
    lsurfex = False
