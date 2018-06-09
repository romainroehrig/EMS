import cdms2, MV2
import numpy

value = 0
cdms2.setNetcdfShuffleFlag(value)
cdms2.setNetcdfDeflateFlag(value)
cdms2.setNetcdfDeflateLevelFlag(value)

data = {}
f = cdms2.open('GABLS4_24h_driver_FC_RR_flux.nc')
for var in f.listvariables():
    print var
    data[var] = f(var)

attributes = {}
for att in f.listglobal():
    attributes[att] = f.getglobal(att)
    if type(attributes[att]) == numpy.ndarray:
        attributes[att] = float(attributes[att])

variables = ['PFCS','PFCLN','ZUSTAR']
for var in variables:
  print var
  g = open('{0}_ARPEGE_stage3_300s.evol'.format(var))
  lines = g.readlines()
  g.close()
  tmp = []
  for line in lines:
      tmp.append(float(line.split()[1]))

  data[var] = MV2.array(tmp,typecode=MV2.float)

nt = data['sfc_sens_flx'].shape[0]

data['sfc_sens_flx'][:,0,0] = data['PFCS'][0:nt]*-1
data['ustar'][:,0,0] = data['ZUSTAR'][0:nt]

g = cdms2.open('GABLS4_24h_driver_FC_RR_flux_EB.nc','w')
for var in f.listvariables():
  g.write(data[var])
for att in sorted(attributes.keys()):
    g.__setattr__(att,attributes[att])

g.close()
f.close()


