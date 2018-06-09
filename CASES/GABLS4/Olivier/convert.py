import cdms2, MV2
import numpy

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value)
cdms2.setNetcdfDeflateLevelFlag(value)

data = {}
f = cdms2.open('GABLS4_24h_driver_FC_RR_flux_zorog.nc')
for var in f.listvariables():
    print var
    data[var] = f(var)

attributes = {}
for att in f.listglobal():
    attributes[att] = f.getglobal(att)
    if type(attributes[att]) == numpy.ndarray:
        attributes[att] = float(attributes[att])

attributes['surfaceForcingWind'] = "z0"
del(attributes['ustar'])


g = cdms2.open('GABLS4_24h_driver_FC_RR_flux_zorog_RR.nc','w')
for var in f.listvariables():
  g.write(data[var])
for att in sorted(attributes.keys()):
    g.__setattr__(att,attributes[att])

g.close()
f.close()


