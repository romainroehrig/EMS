import cdtime
import cdms2

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

tmin = cdtime.comptime(2011,10,1,0,0,0)
tmax = cdtime.comptime(2011,10,2,0,0,0)

data = {}
f = cdms2.open('cindy-dynamo-NSA3a_driver_RR.nc')
for var in f.listvariables():
  data[var] = f(var, time = (tmin,tmax))
f.close()


g = cdms2.open('cindy-dynamo-NSA3a_driver_RR_D1.nc','w')
for var in data.keys():
    g.write(data[var])

g.comment = "Forcing and initial conditions for CINDY-DYNAMO NSA3a case - Day 1" ;
g.reference = "http://johnson.atmos.colostate.edu/dynamo/products/array_averages/index.html" ;
g.author = "R. Roehrig" ;
g.case = "CINDY-DYNAMO - NSA3a - Day 1" ;
g.startDate = "20111001000000" ;
g.endDate =   "20111002000000" ;
g.qadvh = 1 ;
g.tadvh = 1 ;
g.tadvv = 0 ;
g.qadvv = 0 ;
g.trad = 0 ;
g.forc_omega = 1 ;
g.forc_w = 0 ;
g.forc_geo = 0 ;
g.nudging_u = 10800 ;
g.nudging_v = 10800 ;
g.nudging_t = 10800 ;
g.nudging_q = 10800 ;
g.p_nudging_u = 110000 ;
g.p_nudging_v = 110000 ;
g.p_nudging_t = 10000 ;
g.p_nudging_q = 10000 ;
g.zorog = 0. ;
#g.z0 = 0.01 ;
#g.ustar = 0. ;
g.surfaceType = "ocean" ;
g.surfaceForcing = "ts" ;
#g.surfaceForcingWind = "z0" ;

g.close()
