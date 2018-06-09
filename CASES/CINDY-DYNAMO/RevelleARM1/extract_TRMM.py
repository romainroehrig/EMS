import cdms2
import cdtime

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

tmin = cdtime.comptime(2011,11,11,0,0,0)
tmax = cdtime.comptime(2011,12,2,0,0,0)

f = cdms2.open('CINDY-DYNAMO_Revelle-ARM-TRMM_driver_RR_extended.nc')
variables = f.listvariables()

g = cdms2.open('CINDY-DYNAMO_Revelle-ARM-TRMM-leg3_driver_RR_extended.nc','w')

for var in variables:
  print var
  data = f(var,time = (tmin,tmax))

  g.write(data)

g.comment = 'Forcing and initial conditions for CINDY-DYNAMO Revelle ARM-TRMM-leg3 case'
g.reference = 'TBD'
g.author = 'R. Roehrig'


g.case = 'CINDY-DYNAMO - Revelle ARM-TRMM-leg3'
g.startDate = '20111111000000'
g.endDate = '20111202000000'

g.qadvh = 1
g.tadvh = 1
g.tadvv = 0
g.qadvv = 0
g.trad = 0

g.forc_omega = 1
g.forc_w = 0

g.forc_geo = 0

g.nudging_u = 10800
g.nudging_v = 10800
g.nudging_t = 10800
g.nudging_q = 10800

g.p_nudging_u = 110000
g.p_nudging_v = 110000
g.p_nudging_t = 10000
g.p_nudging_q = 10000


g.zorog = 0.
g.z0 = 0.1
g.surfaceType = 'ocean'
g.surfaceForcing = 'ts'


g.close()
f.close()


