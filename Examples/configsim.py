import sys

# model : ARPCLIMAT, AROME, AROME46t1, ARPPNT
#model = 'AROME'
#model = 'AROME46t1'
#model = 'ARPPNT'
model = 'ARPCLIMAT'


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
    #nlev= 90
    nlev= 105
    # Model time step in seconds
    timestep = 360
else:
    print 'Model unknown:', model
    sys.exit()

lsurfex = True
if model in ['AROME','AROME46t1','ARPPNT']:
    lsurfex = False
