#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
import sys

#import time as TT

sys.path = ['./'] + sys.path

import string
import numpy
import netCDF4

import argparse

# Definition of arguments
parser = argparse.ArgumentParser()
parser.add_argument("-format", help="format of LFA files", type=int, required=True)

# Getting arguments
args = parser.parse_args()
lfaformat = args.format

if lfaformat == 8:
    import lfa8lib as lfa
elif lfaformat == 12:
    import lfa12lib as lfa
else:
    print 'ERROR : lfaformat unexepcted:', lfaformat
    raise ValueError

import variables as VV
import config

#TT0 = TT.time()

# Note: COSP output not fully validated...

REP_EMS = os.getenv('REP_EMS')

saveall = config.saveall

# If not present, add some important variables to read

if not(saveall):
    var2save0 = config.var2save
    var2save = []
    for var in var2save0:
        if var in VV.varnames.keys():
            var2save.append(VV.varnames[var])	 
        else:
            print var, 'not in variables.varnames.keys()'
            var2save.append(var)

    #var2save = var2save + ['PAPRS','PAPRSF','PAPHI','PAPHIF']
    var2save = var2save + ['PAPRS','PAPRSF']

    var2save = set(var2save)


inv_varnames = {v: k for k, v in VV.varnames.iteritems()}

# list of files is retrieved

os.system("find LFA -name '*.lfa' > var.tmp")

f = open('var.tmp')
files = f.readlines()
f.close()
os.system('rm var.tmp')

nfiles = len(files)
nstep = nfiles

# List of variables and their size and type is retrieved

lfalaf= REP_EMS + '/UTIL/Tools/LFA/bin/lfalaf'

os.system(lfalaf + ' ' + files[0].strip() + ' > var.tmp')

f = open('var.tmp')
line0 = f.readline()
lines=f.readlines()
f.close()
os.system('rm var.tmp')


variables = []
sizes = {}
types = {}
for line in lines:
    sp = string.split(line,'|')
    var = sp[3][1:-1]
    typ = sp[1]
    tmp = string.split(sp[2])
    size = int(tmp[1])
    variables.append(var)
    sizes[var] = size
    types[var] = typ

# special case of 4D variables which are written in lfa files over several 3D variables
#lvar4D = {}
for vv in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']:
#    lvar4D[vv] = False	
    if vv + '_001' in variables:
      variables.append(vv)	  
#      lvar4D[vv] = True
      sizes[vv] = sizes[vv + '_001']
      types[vv] = types[vv + '_001']


# Final set of variable to read
if saveall: 
    var2save = variables
    var2save = set(var2save)
else:
    variables = set(variables)
    var2save = variables.intersection(set(var2save))

# Do we create unsual axes ?

llev2 = False
for var in []: # To complete
    if var in variables: llev2 = True

lalt40 = False
for var in ['clcalipso','clcalipso2','clcalipsoice','clcalipsoliq','clcalipsoun']:
    if var in variables: lalt40 = True

ltemp = False
for var in ['clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun']:
    if var in variables: ltemp = True

lsza5 = False
for var in ['parasolRefl']:
    if var in variables: lsza5 = True

lcolumn = False
for var in ['boxtauisccp','boxptopisccp','dbze94','atb532','fracout']:
    if var in variables: 
        lcolumn = True

if lcolumn:
    ncol = int(lfa.readi(files[0].strip(),'NCOLUMNS',1))

ldbze = False
for var in ['cfadDbze94']:
    if var in variables:
        ldbze=True
        lalt40 = True

lsratio = False
for var in ['cfadLidarsr532']:
    if var in variables:
        lsratio=True
        lalt40 = True

ltau = False
lplev7 = False
for var in ['clisccp','clmodis']:
    if var in variables:
        ltau=True
        lplev7=True

lmisr = False
for var in ['clMISR']:
    if var in variables: lmisr = True


# Getting number of levels
klev = int(lfa.readi(files[0].strip(),'KLEV',1))

# Getting time step
tstep = float(lfa.readr(files[0].strip(),'TSPHY',1))

# Init nectdf file
f = netCDF4.Dataset('Out_klevel.nc','w',format='NETCDF3_CLASSIC')

level = f.createDimension('levf',klev)
levels = f.createVariable('levf','i4',('levf',))
levels.long_name = 'Full-level Number'
levels.units = '-'
levels.axis = 'Z'
levels[:] = range(0,klev)

Hlevel = f.createDimension('levh',klev+1)
Hlevels = f.createVariable('levh','i4',('levh',))
Hlevels.long_name = 'Half-level Number'
Hlevels.units = '-'
Hlevels.axis = 'Z'
Hlevels[:] = range(0,klev+1)

if llev2:
    level2 = f.createDimension('lev2',klev+2)
    level2s = f.createVariable('lev2','i4',('lev2',))
    level2s.long_name = 'Level number'
    level2s.units = '-'
    level2s[:] = range(0,klev+2)

if lalt40:
    alt40 = f.createDimension('alt40',40)
    alt40s = f.createVariable('alt40','f8',('alt40',))
    alt40s.long_name = 'altitude'
    alt40s.units = 'm'
    alt40s.axis = 'Z'
    alt40s[:] = [240., 720., 1200., 1680., 2160., 2640., 3120., 3600., 4080., 4560., 5040., 5520., 6000., 6480., 6960., 7440., 7920., 8400., 8880., 9360., 9840., 10320., 10800., 11280., 11760., 12240., 12720., 13200., 13680., 14160., 14640., 15120., 15600., 16080., 16560., 17040., 17520., 18000., 18480., 18960.]

if ltemp:
    temp = f.createDimension('tempAxis',40)
    temps = f.createVariable('tempAxis','f8',('tempAxis',))
    temps.long_name = 'Temperature'
    temps.units = 'C'
    temps[:] = [-91.5,-88.5,-85.5,-82.5,-79.5,-76.5,-73.5,-70.5,-67.5,-64.5,-61.5,-58.5,-55.5,-52.5,-49.5,-46.5,-43.5,-40.5,-37.5,-34.5,-31.5,-28.5,-25.5,-22.5,-19.5,-16.5,-13.5,-10.5, -7.5, -4.5,-1.5,  1.5,  4.5,  7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5]

if lsza5:
    sza5 = f.createDimension('sza5',5)
    sza5s = f.createVariable('sza5','f8',('sza5',))
    sza5s.long_name = 'Solar Zenith Angle'
    sza5s.units = 'degree'
    sza5s[:] = [0., 20., 40., 60., 80.]

if lcolumn: # or lvar4D['dbze94'] or lvar4D['atb532'] or lvar4D['fracout']:
    column = f.createDimension('column',ncol)
    columns = f.createVariable('column','i4',('column'))
    columns.long_name = 'column'
    columns.units = '-'
    columns[:] = [float(i) for i in range(0,ncol)]

if ldbze: #lvar4D['cfadDbze94']:
    dbze = f.createDimension('dbze',15)
    dbzes = f.createVariable('dbze','f8',('dbze',))
    dbzes.long_name = 'CloudSat simulator equivalent radar reflectivity factor'
    dbzes.units = 'dBZ'  
    dbzes[:] = [-47.5, -42.5, -37.5, -32.5, -27.5, -22.5, -17.5, -12.5, -7.5, -2.5, 2.5, 7.5, 12.5, 17.5, 22.5]

if lsratio: #lvar4D['cfadLidarsr532']:
    sratio = f.createDimension('scatratio',15)
    sratios = f.createVariable('scatratio','f8',('scatratio',))
    sratios.long_name = 'lidar backscattering ratio'
    sratios.units = '1'
    sratios[:] = [0.005, 0.605, 2.1, 4., 6., 8.5, 12.5, 17.5, 22.5, 27.5, 35., 45., 55., 70., 50040.]

if ltau: #lvar4D['clisccp'] or lvar4D['clmodis'] or lvar4D['clMISR']:  
    tau = f.createDimension('tau',7)
    taus = f.createVariable('tau','f8',('tau',))
    taus.long_name = 'cloud optical depth'
    taus.units = '1'  
    taus[:] = [0.15, 0.8, 2.45, 6.5, 16.2, 41.5, 100.]

if lplev7: #lvar4D['clisccp'] or lvar4D['clmodis']:  
    plev7 = f.createDimension('plev7',7)  
    plev7s = f.createVariable('plev7','f8',('plev7',))
    plev7s.long_name = 'pressure'
    plev7s.units = 'Pa'
    plev7s.axis = 'Z'
    plev7s[:] = [90000., 74000., 62000., 50000., 37500., 24500., 9000.]

if lmisr: #lvar4D['clMISR']:  
    cth16 = f.createDimension('cth16',16)
    cth16s = f.createVariable('cth16','f8',('cth16',))
    cth16s.long_name = 'altitude'
    cth16s.units = 'm'  
    cth16s.axis = 'Z'
    cth16s[:] = [1000.*x for x in [0., 0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.5, 4.5, 6., 8., 10., 12., 14.5, 16., 18.]]

time = f.createDimension('time',None)
times = f.createVariable('time','f8',('time',))
times.calendar = 'gregorian'
times.axis = 'T'


# Create variables in netcdf file

data = {}
shape = {}
ii=0
for var in sorted(var2save):
    if sizes[var] == 1:
      axis = ('time')
    if sizes[var] == 2:
      axis = ('time','TOASurf')
      if ii == 0:
        topdown = f.createDimension('TOASurf',2)
        ii = 1
    if sizes[var] == klev and not(var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']):
        axis = ('time','levf')
        shape[var] = (nstep,klev,)
    if sizes[var] == klev+1  and not(var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']):
        axis = ('time','levh')
        shape[var] = (nstep,klev+1,)
    if sizes[var] == klev+2:
        axis = ('time','lev2')
        shape[var] = (nstep,klev+2,)
    if var in ['clcalipso','clcalipso2','clcalipsoice','clcalipsoliq','clcalipsoun'] and sizes[var] == 40:
        axis = ('time','alt40')
        shape[var] = (nstep,40,)
    if var in ['clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun'] and sizes[var] == 40:
        axis = ('time','temp')
        shape[var] = (nstep,40,)
    if var in ['parasolRefl'] and sizes[var] == 5:
        axis = ('time','sza5')
        shape[var] = (nstep,5,)
    if lcolumn and var in ['boxtauisccp','boxptopisccp'] and sizes[var] == ncol:
        axis = ('time','column')	
        shape[var] = (nstep,ncol,)
    if var in ['fracout', 'atb532', 'dbze94']:
        axis = ('time','column','levf')
        shape[var] = (nstep,ncol,klev)
    if var in ['cfadDbze94']:
        axis = ('time','dbze','alt40')
        shape[var] = (nstep,15,40)
    if var in ['cfadLidarsr532']:
        axis = ('time','sratio','alt40')
        shape[var] = (nstep,15,40)
    if var in ['clisccp','clmodis']:
        axis = ('time','tau','plev7')
        shape[var] = (nstep,7,7)
    if var in ['clMISR']:
        axis = ('time','tau','cth16')
        shape[var] = (nstep,7,16)



    if types[var] == 'I4':  	
        typ = 'i4'
    if types[var] == 'R4':
        typ = 'f4'
    if types[var] == 'R8':
        typ = 'f8'    
    if types[var] == 'C ':
        typ = 'c'	  
  
    if typ <> 'c':
        if var in inv_varnames.keys():
            data[var] = f.createVariable(inv_varnames[var],typ,axis,fill_value=1.e20)
            data[var].long_name = VV.names[inv_varnames[var]]
            data[var].units = VV.units[inv_varnames[var]]
            data[var].original_name = var
        else:
            data[var] = f.createVariable(var,typ,axis,fill_value=1.e20)
            data[var].long_name = var
            data[var].units = '-'
            data[var].original_name = var

        data[var].missing_value = numpy.float32(1.e20)

  

# Read lfa files and write data in netcdf file

nindat = lfa.readi(files[0].strip(),'NINDAT',1)

year = int(str(nindat[0])[0:4])
month = int(str(nindat[0])[4:6])
day = int(str(nindat[0])[6:8])
units = 'seconds since %(year)4.4i-%(month)2.2i-%(day)2.2i 0:0:0.0'%{"year": year, "month": month, "day": day}
times.units = units

nsssss = lfa.iterate_readi('NSSSSS',1,tstep,nstep)
rstati = lfa.iterate_readr('RSTATI',1,tstep,nstep)

times[:] = nsssss + rstati

for var in var2save:
    print var
    if types[var] <> 'C':
        if var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']:

            datatmp = numpy.zeros(shape[var],dtype=numpy.float)             
            for i in range(0,shape[var][0]):
                vv = var + '_%(i)3.3i'%{"i": i+1}
                datatmp[:,i,:] = lfa.iterate_readr(vv,sizes[var],tstep,nstep)

        else:
        
            if types[var] == 'I4':
                datatmp = lfa.iterate_readi(var,sizes[var],tstep,nstep)
                if sizes[var] == 1:
                    datatmp = numpy.squeeze(datatmp)
            if types[var] == 'R4':
                datatmp = lfa.iterate_readr(var,sizes[var],tstep,nstep)
                if sizes[var] == 1:
                    datatmp = numpy.squeeze(datatmp)

            if var in inv_varnames.keys():
                data[var][:] = datatmp*VV.coefs[inv_varnames[var]]
            else:
                data[var][:] = datatmp

f.close()

#print 'Elapsed:', TT.time() - TT0
