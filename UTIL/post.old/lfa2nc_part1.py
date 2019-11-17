import sys
#print sys.path
sys.path = ['./'] + sys.path

import lfa
import netCDF4
import os
import string
import numpy

import variables
import config


saveall = config.saveall

if not(saveall):
  var2save0 = config.var2save
  var2save = []
  for var in var2save0:
    var2save.append(variables.varnames[var])	 

  var2add = ['NINDAT',\
             'NSSSSS',\
             'RSTATI',\
	     'TSPHY',\
	     'PAPRS',\
	     'PAPRSF',\
	     'PAPHI',\
	     'PAPHIF']

  for var in var2add:
    var2save.append(var)

  var2save = set(var2save)

# list of files

os.system('ls LFA/*.lfa > var.tmp')

f = open('var.tmp')
files = f.readlines()
f.close()
os.system('rm var.tmp')


nfiles = len(files)

# List of variables and their size and type is retrieved

lfalaf= os.get('REP_EMS') + '/UTIL/Tools/LFA/bin/lfalaf'

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
#  print sp
  var = sp[3][1:-1]
  type = sp[1]
  tmp = string.split(sp[2])
  size = int(tmp[1])
  variables.append(var)
  sizes[var] = size
  types[var] = type

lvar4D = {}
for vv in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']:
  lvar4D[vv] = False	
  if vv + '_001' in variables:
    variables.append(vv)	  
    lvar4D[vv] = True
    sizes[vv] = sizes[vv + '_001']
    types[vv] = types[vv + '_001']

if saveall: 
  var2save = variables
  var2save = set(var2save)
else:
  variables = set(variables)
  var2save = variables.intersection(set(var2save))


# Getting levels number

klev = int(lfa.readi(files[0].strip(),'KLEV',1))
lcolumn = False
try:
  ncol = int(lfa.readi(files[0].strip(),'NCOLUMNS',1))
  if not(ncol == 0): lcolumn = True
except:
  pass

# Init nectdf file

f = netCDF4.Dataset('global.nc','w',format='NETCDF3_CLASSIC')
level = f.createDimension('level',klev)
Hlevel = f.createDimension('Hlevel',klev+1)
level2 = f.createDimension('level2',klev+2)
alt40 = f.createDimension('alt40',40)
temp = f.createDimension('temp',40)
sza5 = f.createDimension('sza5',5)
if lcolumn or lvar4D['dbze94'] or lvar4D['atb532'] or lvar4D['fracout']:
  column = f.createDimension('column',ncol)
if lvar4D['cfadDbze94']:
  dbze = f.createDimension('dbze',15)
if lvar4D['cfadLidarsr532']:
  sratio = f.createDimension('sratio',15)
if lvar4D['clisccp'] or lvar4D['clmodis'] or lvar4D['clMISR']:  
  tau = f.createDimension('tau',7)
if lvar4D['clisccp'] or lvar4D['clmodis']:  
  plev7 = f.createDimension('plev7',7)  
if lvar4D['clMISR']:  
  cth16 = f.createDimension('cth16',16)

time = f.createDimension('time',None)

data = {}
ii=0
for var in var2save:
  if sizes[var] == 1:
    axis = ('time')
  if sizes[var] == 2:
    axis = ('time','TOASurf')
    if ii == 0:
      topdown = f.createDimension('TOASurf',2)
      ii = 1
  if sizes[var] == klev and not(var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']):
    axis = ('time','level')
  if sizes[var] == klev+1  and not(var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']):
    axis = ('time','Hlevel')
  if sizes[var] == klev+2:
    axis = ('time','level2')
  if var in ['clcalipso','clcalipso2','clcalipsoice','clcalipsoliq','clcalipsoun'] and sizes[var] == 40:
    axis = ('time','alt40')
  if var in ['clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun'] and sizes[var] == 40:
    axis = ('time','temp')
  if var in ['parasolRefl'] and sizes[var] == 5:
    axis = ('time','sza5')
  if lcolumn and var in ['boxtauisccp','boxptopisccp'] and sizes[var] == ncol:
    axis = ('time','column')	
  if var in ['fracout', 'atb532', 'dbze94']:
    axis = ('time','column','level')
  if var in ['cfadDbze94']:
    axis = ('time','dbze','alt40')
  if var in ['cfadLidarsr532']:
    axis = ('time','sratio','alt40')
  if var in ['clisccp','clmodis']:
    axis = ('time','tau','plev7')
  if var in ['clMISR']:
    axis = ('time','tau','cth16')



  if types[var] == 'I4':  	
    typ = 'i4'
  if types[var] == 'R4':
    typ = 'f4'
  if types[var] == 'C ':
    typ = 'c'	  
  
  if typ <> 'c':
    data[var] = f.createVariable(var,typ,axis)
  

it = -1
for file in files:
  it = it + 1
  if config.verbose >= 2:
    print file.strip()
  for var in var2save:
    if var in ['fracout', 'atb532', 'dbze94']:
      datatmp = numpy.zeros((ncol,klev),dtype=numpy.float)
      for i in range(0,ncol):
        vv = var + '_%(i)3.3i'%{"i": i+1}
	datatmp[i,:] = lfa.readr(file.strip(),vv,sizes[var])

      data[var][it] = datatmp
    elif var in ['cfadDbze94']:
      datatmp = numpy.zeros((15,40),dtype=numpy.float)
      for i in range(0,15):
        vv = var + '_%(i)3.3i'%{"i": i+1}
	datatmp[i,:] = lfa.readr(file.strip(),vv,sizes[var])

      data[var][it] = datatmp

    elif var in ['cfadLidarsr532']:
      datatmp = numpy.zeros((15,40),dtype=numpy.float)
      for i in range(0,15):
        vv = var + '_%(i)3.3i'%{"i": i+1}
	datatmp[i,:] = lfa.readr(file.strip(),vv,sizes[var])

      data[var][it] = datatmp

    elif var in ['clisccp', 'clmodis']:
      datatmp = numpy.zeros((7,7),dtype=numpy.float)
      for i in range(0,7):
        vv = var + '_%(i)3.3i'%{"i": i+1}
	datatmp[i,:] = lfa.readr(file.strip(),vv,sizes[var])

      data[var][it] = datatmp

    elif var in ['clMISR']:
      datatmp = numpy.zeros((7,16),dtype=numpy.float)
      for i in range(0,7):
        vv = var + '_%(i)3.3i'%{"i": i+1}
	datatmp[i,:] = lfa.readr(file.strip(),vv,sizes[var])

      data[var][it] = datatmp

    else:
      
#    print var
      if types[var] <> 'C ':
        if types[var] == 'I4':
          tmp = lfa.readi(file.strip(),var,sizes[var])
          if sizes[var] == 1:
            tmp = int(tmp)
        if types[var] == 'R4':
          tmp = lfa.readr(file.strip(),var,sizes[var])
          if sizes[var] == 1:
            tmp = float(tmp)

        data[var][it] = tmp


f.close()

