#!/usr/bin/env python
# -*- coding:UTF-8 -*-

lperf = True #False
if lperf:
  import time
  t00 = time.time()

import os, sys

mainrep = os.getenv('REP_EMS')
repinit = os.getcwd()

repARPC_UTIL = mainrep + '/UTIL/Init_Forc/ARPCLIMAT/'
repARO_UTIL = mainrep + '/UTIL/Init_Forc/AROME/'
repARO46t1_UTIL = mainrep + '/UTIL/Init_Forc/AROME46t1/'
repARP_UTIL = mainrep + '/UTIL/Init_Forc/ARPPNT/'
repSFX_UTIL = mainrep + '/UTIL/Init_Forc/SURFEX/'
repRun_UTIL = mainrep + '/UTIL/Runs/'
repAtlas_UTIL = mainrep + '/UTIL/atlas/'

def install_ATM(model,case,filecase,repout,nlev,timestep=None,subcase=None,loverwrite=False,lupdate=False):

    """ Prepare files of atmospheric initial conditions and forcing needed to run MUSC """

    rep = repout + '/' + case + '/'
    if subcase is not None:
      rep = rep + '/' + subcase + '/' 

    print '-'*40
    print 'Prepare Atmospheric files'
    print 'Case:', case, 'subcase:',subcase
    print 'nlev:', nlev
    print 'timestep:', timestep
    print 'Installation in', rep

    if model == 'ARPCLIMAT':
      repUTIL = repARPC_UTIL
    elif model == 'AROME':
      repUTIL = repARO_UTIL
    elif model == 'AROME46t1':
      repUTIL = repARO46t1_UTIL      
    elif model == 'ARPPNT':
      repUTIL = repARP_UTIL
    else:
      print 'Model unexpected:', model
      sys.exit()

    if (loverwrite):
      os.system('rm -rf ' + rep)
    flagExist = True
    if not(os.path.exists(rep)):
      os.makedirs(rep)
      flagExist = False

    if not(flagExist):
      os.chdir(rep)
      os.system('ln -s ' + filecase + ' data_input.nc')
      os.system('ln -s ' + repARPC_UTIL + 'interpvertp.so .')
      if model == 'ARPCLIMAT':
        os.system('ln -s ' + repUTIL + 'prepare_forcing.py .')
      os.system('ln -s ' + repUTIL + 'prepare_nam1D.py .')
      os.system('ln -s ' + repUTIL + 'prepare_profile.sh .')
      os.system('ln -s ' + repUTIL + 'L' + str(nlev) + '.dta .')
      if timestep is None:
        os.system('./prepare_profile.sh ' + str(nlev))
      else:
        os.system('./prepare_profile.sh ' + str(nlev) + ' ' + str(int(timestep)))
    else: 
      print 'Directory already exists:', rep
      if lupdate:
        os.chdir(rep)
        os.system('ln -s ' + repUTIL + 'L' + str(nlev) + '.dta .')
        if timestep is None:
          os.system('./prepare_profile.sh ' + str(nlev))
        else:
          os.system('./prepare_profile.sh ' + str(nlev) + ' ' + str(int(timestep)))
      else:
        print 'Nothing is done'

    os.chdir(repinit)

    print '-'*40
      

def install_SFX(model,case,filecase,repout,cycle,PGD,PREP,config,namref,subcase=None,loverwrite=False,lupdate=False,ecoclimap=mainrep + '/UTIL/ecoclimap_cnrm_cm6.02'):

    """ Prepare files of atmospheric initial conditions and forcing needed to run MUSC """

    if model not in ['ARPCLIMAT']:
      'SURFEX preparation is not coded for model=', model
      sys.exit()

    if lperf:
      t0 = time.time()
      t00 = time.time()

    rep = repout + '/' + cycle + '/' + config + '/' + case + '/'
    if subcase is not None:
      rep = rep + '/' + subcase + '/'

    print '-'*40
    print 'Prepare SFX files'
    print 'Case:', case, 'subcase:',subcase
    print 'Cycle:', cycle
    print 'PGD:', PGD
    print 'PREP:', PREP
    print 'config:', config
    print 'namref:', namref
    print 'Installation in', rep


    if (loverwrite):
      os.system('rm -rf ' + rep)
    flagExist = True
    if not(os.path.exists(rep)):
      os.makedirs(rep)
      flagExist = False

    if not(flagExist):
      os.chdir(rep)
      os.system('ln -s ' + filecase + ' data_input.nc')
#      os.system('ln -s ' + repSFX_UTIL + 'prep_nam.py .')
      os.system('ln -s ' + repSFX_UTIL + 'prepare_SURFEX.sh .')
      os.system('ln -s ' + ecoclimap + ' .')
      os.system('ln -s ' + PGD + ' .')
      os.system('ln -s ' + PREP + ' .')

      if lperf:
        t1 = time.time()
        print 'First things:', t1-t0
        t0 = time.time()

      # Preparation namelist SURFEX
      g = open('tmp.py','w')

      print >> g, 'import prepare_namSFX'
      print >> g, 'case = "' + case + '"'
      if not(subcase is None):
        print >> g, 'subcase = "' + subcase + '"'
      else:
        print >> g, 'subcase = None'
      print >> g, 'filecase = "data_input.nc"'
      print >> g, 'namref = "' + namref + '"'
      print >> g, 'namout = "namsurf"'
      print >> g, 'prepare_namSFX.prep_nam_SFX(case,filecase,namref,namout=namout,subcase=subcase)'

      g.close()

      os.system('python tmp.py')
      os.system('rm -f tmp.py')
    
      if lperf:
        t1 = time.time()
        print 'Prepare SURFEX namelist:', t1-t0
        t0 = time.time()

      # PGD and PREP
      os.system('./prepare_SURFEX.sh')

      if lperf:
        t1 = time.time()
        print 'Prepare PGD/PREP:', t1-t0
        t0 = time.time()

    else: 
      print 'Directory already exists:', rep
      if lupdate:
        os.chdir(rep)

        g = open('tmp.py','w')

        print >> g, 'import prepare_namSFX'
        print >> g, 'case = "' + case + '"'
        if not(subcase is None):
          print >> g, 'subcase = "' + subcase + '"'
        else:
          print >> g, 'subcase = None'
        print >> g, 'filecase = "data_input.nc"'
        print >> g, 'namref = "' + namref + '"'
        print >> g, 'namout = "namsurf"'
        print >> g, 'prepare_namSFX.prep_nam_SFX(case,filecase,namref,namout=namout,subcase=subcase)'

        g.close()

        os.system('python tmp.py')
        os.system('rm -f tmp.py')

        os.system('./prepare_SURFEX.sh')
      else:
        print 'Nothing is done'

    os.chdir(repinit)
    print '-'*40

    if lperf:
      t1 = time.time()
      print 'Total:', t1-t00


def install_Run(model,case,filecase,repout,config,configOut,subcase=None,loverwrite=False,lupdate=False):

    """ Install a MUSC simulation """
    
    if lperf:
      t0 = time.time()
      t00 = time.time()

    import cdms2
    import cdtime

    rep = repout + '/' + config['cycle'] + '/' + config['name'] + '/L' + str(config['levels']) + '_' + str(config['TSTEP']) + 's/'  + case + '/'
    if subcase is not None:
      rep = rep + '/' + subcase + '/'

    print '-'*40
    print 'Prepare MUSC simulation'
    print 'Case:', case, 'subcase:',subcase
    print 'Cycle:', config['cycle']
    print 'MASTER:', config['MASTER']
    print 'Configuration name:', config['name']
    print '{0} reference namelist:'.format(model), config['namATMref']
    if config['lsurfex']:
      print 'SURFEX reference namelist:', config['namSFXref']
      print 'Ecoclimap directory:', config['ecoclimap']
    print 'Initial Conditions file:', config['initfile']
    if model == 'ARPCLIMAT':
      print 'Atmospheric forcing files:', config['forcingfiles']
    if config['lsurfex']:
      print 'PGD file:', config['PGDfile']
      print 'PREP file:', config['PREPfile']
    print 'Timestep:', config['TSTEP']
    print 'Levels:', config['levels']

    print 'Postprocessing:'
    print 'dirpost:', configOut['dirpost']
    print 'configpost:', configOut['configpost']
    print 'variablesDict:', configOut['variablesDict']

    print 'Installation in', rep


    if (loverwrite):
      os.system('rm -rf ' + rep)
    flagExist = True
    if not(os.path.exists(rep)):
      os.makedirs(rep)
      flagExist = False

    if not(flagExist):
      os.chdir(rep)
      os.makedirs('./logs/')
      os.system('ln -s ' + filecase + ' data_input.nc')
      os.system('cp ' + config['namATMref'] + ' namATMref')
      if config['lsurfex']:
        os.system('cp ' + config['namSFXref'] + ' namSFXref')

      if lperf:
        t1 = time.time()
        print 'First things:', t1-t0
        t0 = time.time()

      # Determination NSTOP
      f = cdms2.open(filecase)
      startDate = str(f.startDate)
      endDate = str(f.endDate)
      f.close()

      year = int(startDate[0:4])
      month = int(startDate[4:6])
      day = int(startDate[6:8])
      hour = int(startDate[8:10])
      minute = int(startDate[10:12])
#      second = int(startDate[12:14])
      second = 0

      units = 'hours since %(year)4.4i-%(month)2.2i-%(day)2.2i %(hour)2.2i:%(minute)2.2i:%(second)2.2i'%{"year":year, "month": month, "day": day, "hour": hour, "minute": minute, "second": second}

      tstart = cdtime.comptime(year,month,day,hour,minute,second)

      year = int(endDate[0:4])
      month = int(endDate[4:6])
      day = int(endDate[6:8])
      hour = int(endDate[8:10])
      minute = int(endDate[10:12])
#      second = int(endDate[12:14])
      second = 0

      tend = cdtime.comptime(year,month,day,hour,minute,second)

      tmp = tend.torel(units).value - tstart.torel(units).value
      NSTOP = 'h' + str(int(tmp))
      print 'NSTOP:', NSTOP

      if lperf:
        t1 = time.time()
        print 'Compute NSTOP:', t1-t0
        t0 = time.time()

      # Preparation namelist
      g = open('tmp.py','w')

      print >> g, 'import prepare_namATM_{0} as prepare_namATM'.format(model)
      if subcase is None:
        print >> g, 'prepare_namATM.prep_nam_ATM("' + case + '","' + filecase + '","' + config['namATMref'] + '",' + str(config['TSTEP']) + ',"' + NSTOP + '",namout="namarp_' + config['name'] + '")'          
      else:
        print >> g, 'prepare_namATM.prep_nam_ATM("' + case + '","' + filecase + '","' + config['namATMref'] + '",' + str(config['TSTEP']) + ',"' + NSTOP + '",namout="namarp_' + config['name'] + '",subcase="' + subcase + '")'

      g.close()

      os.system('python tmp.py')
      os.system('rm -f tmp.py')

      if lperf:
        t1 = time.time()
        print 'Prepare {0} namelist:'.format(model), t1-t0
        t0 = time.time()

      # Preparation namelist SURFEX
      if config['lsurfex']:
        g = open('tmp.py','w')

        print >> g, 'import prepare_namSFX'
        print >> g, 'case = "' + case + '"'
        print >> g, 'filecase = "' + filecase + '"'
        print >> g, 'namref = "' + config['namSFXref'] + '"'
        print >> g, 'namout = "namsfx_' + config['name'] + '"'
        print >> g, 'prepare_namSFX.prep_nam_SFX(case,filecase,namref,namout=namout,subcase="'+subcase+'")'

        g.close()

        os.system('python tmp.py')
        os.system('rm -f tmp.py')

        if lperf:
          t1 = time.time()
          print 'Prepare SURFEX namelist:', t1-t0
          t0 = time.time()

      # Determination NSTOP
      f = cdms2.open(filecase)
      startDate = str(f.startDate)
      endDate = str(f.endDate)
      f.close()

      year = int(startDate[0:4])
      month = int(startDate[4:6])
      day = int(startDate[6:8])
      hour = int(startDate[8:10])
      minute = int(startDate[10:12])
#      second = int(startDate[12:14])
      second = 0

      units = 'hours since %(year)4.4i-%(month)2.2i-%(day)2.2i %(hour)2.2i:%(minute)2.2i:%(second)2.2i'%{"year":year, "month": month, "day": day, "hour": hour, "minute": minute, "second": second}

      tstart = cdtime.comptime(year,month,day,hour,minute,second)

      year = int(endDate[0:4])
      month = int(endDate[4:6])
      day = int(endDate[6:8])
      hour = int(endDate[8:10])
      minute = int(endDate[10:12])
#      second = int(endDate[12:14])
      second = 0

      tend = cdtime.comptime(year,month,day,hour,minute,second)

      tmp = tend.torel(units).value - tstart.torel(units).value
      NSTOP = 'h' + str(int(tmp))
      print 'NSTOP:', NSTOP

      if lperf:
        t1 = time.time()
        print 'Compute NSTOP:', t1-t0
        t0 = time.time()



      os.system('ln -s ' + repRun_UTIL + 'run_{0}.sh run.sh'.format(model))

      # Preparation fichier config de la simulation
      g = open('param_' + config['name'],'w')

      print >> g, '#!/bin/sh'
      print >> g, 'set -x'
      print >> g, '#'
      print >> g, 'cycle=' + config['cycle']
      print >> g, 'MASTER=' + config['MASTER']
      print >> g, '#'
      print >> g, 'levels=' + str(config['levels'])
      print >> g, 'TSTEP=' + str(config['TSTEP'])
      print >> g, 'NSTOP=' + NSTOP
      print >> g, '#'
      print >> g, 'CONFIG=' + config['name']
      print >> g, 'NAMARP=namarp_' + config['name']
      if config['lsurfex']:
        print >> g, 'NAMSFX=namsfx_' + config['name']
      print >> g, '#'
      print >> g, 'INITFILE=' + config['initfile']
      if model == 'ARPCLIMAT':
        print >> g, 'FORCING_FILES=' + config['forcingfiles'] + ''
      if config['lsurfex']:
        print >> g, 'PGD=' + config['PGDfile']
        print >> g, 'PREP=' + config['PREPfile']
      print >> g, 'ecoclimap=' + config['ecoclimap']
      print >> g, '#'
      print >> g, 'dirpost=' + configOut['dirpost'] # '/home/roehrig/MUSC/UTIL/post'
      print >> g, 'configpost=' + configOut['configpost']
      print >> g, 'variablesDict=' + configOut['variablesDict']
      print >> g, 'installpost=True'
      print >> g, 'runpost=True'

      g.close()
      
      os.system('chmod u+x param_' + config['name'])

      # Preparation fichier d'execution de la simulation
      g = open('exec.sh','w')

      print >> g, '#!/bin/sh'
      print >> g, 'set -x'
      print >> g, 'date'
      print >> g, 'rm -f param'
      print >> g, 'ln -s param_' + config['name'] + ' param'
      print >> g, '. ./param'
      print >> g, '. ./run.sh > run_${cycle}_${CONFIG}_L${levels}_${TSTEP}s.log 2>&1'
      print >> g, 'mv run_${cycle}_${CONFIG}_L${levels}_${TSTEP}s.log logs/'
      print >> g, 'echo log file: logs/run_${cycle}_${CONFIG}_L${levels}_${TSTEP}s.log'
      print >> g, 'date'

      g.close()

      os.system('chmod u+x exec.sh')

      if lperf:
        t1 = time.time()
        print 'Prepare Run:', t1-t0
        t0 = time.time()

      # Execution de la simulation
      os.system('./exec.sh > exec.log 2>&1')

      if lperf:
        t1 = time.time()
        print 'Execution:', t1-t0
        t0 = time.time()

    elif lupdate: 
      print 'Directory already exists:', rep
      print 'We only re-run the simulation'
      os.chdir(rep)

      # Execution de la simulation
      os.system('./exec.sh > exec.log 2>&1')

      if lperf:
        t1 = time.time()
        print 'Execution:', t1-t0
        t0 = time.time()

    else: 
      print 'Directory already exists:', rep
      print 'Nothing is done'

    os.chdir(repinit)

    print '-'*40

    if lperf:
      t1 = time.time()
      print 'Total:', t1-t00


def install_Atlas(case,repout,config,configAtlas,subcase=None,loverwrite=False,lupdate=False):

    """ Install Atlas for MUSC simulation """

    rep = repout + '/' + config['cycle'] + '/' + config['name'] + '/' + case + '/'
    if subcase is not None:
      rep = rep + '/' + subcase + '/'
    rep = rep + '/L' + str(config['levels']) + '_' + str(config['TSTEP']) + 's/'

    print '-'*40
    print 'Install Atlas for MUSC simulation'
    print 'Case:', case, 'subcase:',subcase
    print 'Cycle:', config['cycle']
    print 'Configuration name:', config['name']
    print 'Timestep:', config['TSTEP']
    print 'Levels:', config['levels']
    print 'Main directory:', config['repout']

    print 'Atlas config:'
    print 'config:', configAtlas['config']
    print 'varplot:', configAtlas['varplot']

    print 'Installation in', rep


    if (loverwrite):
      os.system('rm -rf ' + rep)
    flagExist = True
    if not(os.path.exists(rep)):
      os.makedirs(rep)
      flagExist = False

    if not(flagExist):
      os.chdir(rep)
      os.system('ln -s ' + repAtlas_UTIL + '/atlas.py .')
      os.system('ln -s ' + repAtlas_UTIL + '/plot_util.py .')
      os.system('ln -s ' + repAtlas_UTIL + '/' + configAtlas['config'] + ' config.py')
      os.system('ln -s ' + repAtlas_UTIL + '/' + configAtlas['varplot'] + ' varplot.py')

      g = open('simu.py','w')

      if subcase is None:
        print >> g, "case = '" + case + "'"
      else:
        print >> g, "case = '" + case + "/" + subcase + "'"
      print >> g, "cycle = '" + config['cycle'] + "'"
      print >> g, "name = '" + config['name'] + "'"
      print >> g, "simu = '" + config['name'] + "/L" + str(config['levels']) + "_" + str(config['TSTEP']) + "s'"
      print >> g, "repsimu = '" + config['repout'] + "' + '/simulations/' + cycle + '/' + name + '/' + case + '/Output/netcdf/'"

      g.close()

      os.system('python atlas.py')

    else: 
      print 'Directory already exists:', rep
      if lupdate:
        os.chdir(rep)
        os.system('python atlas.py')
      else:
        print 'Nothing is done'

    print '-'*40
