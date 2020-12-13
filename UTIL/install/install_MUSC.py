#!/usr/bin/env python
# -*- coding:UTF-8 -*-

lperf = True #False
if lperf:
  import time
  t00 = time.time()

import os

repEMS = os.getenv('REP_EMS')
repinit = os.getcwd()

import sys

from datetime import datetime

repARPC_UTIL = repEMS + '/UTIL/Init_Forc/ARPCLIMAT/'
repARO_UTIL = repEMS + '/UTIL/Init_Forc/AROME/'
repARO46t1_UTIL = repEMS + '/UTIL/Init_Forc/AROME46t1/'
repARP_UTIL = repEMS + '/UTIL/Init_Forc/ARPPNT/'
repSFX_UTIL = repEMS + '/UTIL/Init_Forc/SURFEX/'
repRun_UTIL = repEMS + '/UTIL/Runs/'
repAtlas_UTIL = repEMS + '/UTIL/atlas/'

def install_ATM(model,case,subcase,filecase,repout,vert_grid,timestep=None,loverwrite=False,lupdate=False):

    """ Prepare files of atmospheric initial conditions and forcing needed to run MUSC """

    rep = os.path.join(repout,case,subcase)

    print '#'*40
    print 'Prepare Atmospheric files'
    print 'Case:', case, 'subcase:',subcase
    print 'Vertical grid:', vert_grid
    print 'timestep:', timestep
    print 'Installation in', rep

    vert_grid_file = os.path.basename(vert_grid)
    vert_grid_name = vert_grid_file.split('.')[0]

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
        os.symlink(filecase,'data_input.nc')
        os.symlink(repUTIL + 'prepare_init_forc.py','prepare_init_forc.py')
        os.symlink(repUTIL + 'prepare_init_forc.sh','prepare_init_forc.sh')
        os.symlink(vert_grid,vert_grid_file)
        if timestep is None:
            os.system('./prepare_init_forc.sh {0} {1} {2}'.format(case,subcase,vert_grid_name))
        else:
            os.system('./prepare_init_forc.sh {0} {1} {2} {3}'.format(case,subcase,vert_grid_name,int(timestep)))
    else: 
        print 'Directory already exists:', rep
        if lupdate:
            os.chdir(rep)
            try:
                os.symlink(vert_grid,vert_grid_file)
            except OSError:
                pass
            except:
                raise
            if timestep is None:
                os.system('./prepare_init_forc.sh {0} {1} {2}'.format(case,subcase,vert_grid_name))
            else:
                os.system('./prepare_init_forc.sh {0} {1} {2} {3}'.format(case,subcase,vert_grid_name,int(timestep)))
        else:
            print 'Nothing is done'

    os.chdir(repinit)

    print '-'*40
      

def install_SFX(model,case,subcase,filecase,repout,PGD,PREP,namref,loverwrite=False,lupdate=False,ecoclimap=repEMS + '/UTIL/ecoclimap_cnrm_cm6.02'):

    """ Prepare files of atmospheric initial conditions and forcing needed to run MUSC """

    if model not in ['ARPCLIMAT']:
        'SURFEX preparation is not coded for model=', model
        sys.exit()

    if lperf:
        t0 = time.time()
        t00 = time.time()

    rep = os.path.join(repout,case,subcase)

    print '#'*40
    print 'Prepare SFX files'
    print 'Case:', case, 'subcase:',subcase
    print 'PGD:', PGD
    print 'PREP:', PREP
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
        os.symlink(filecase,'data_input.nc')
        os.symlink(repSFX_UTIL + 'prepare_SURFEX.sh','prepare_SURFEX.sh')
        os.symlink(ecoclimap,'ecoclimap')
        os.symlink(PGD,'PGD')
        os.symlink(PREP,'PREP')

        if lperf:
            t1 = time.time()
            print 'First things:', t1-t0
            t0 = time.time()

        # Preparation namelist SURFEX
        g = open('tmp.py','w')

        print >> g, 'import prepare_namSFX'
        print >> g, 'case = "' + case + '"'
        print >> g, 'subcase = "' + subcase + '"'
        print >> g, 'filecase = "data_input.nc"'
        print >> g, 'namref = "' + namref + '"'
        print >> g, 'namout = "namsurf"'
        print >> g, 'prepare_namSFX.prep_nam_SFX(case,subcase,filecase,namref,namout=namout)'

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
            print >> g, 'subcase = "' + subcase + '"'
            print >> g, 'filecase = "data_input.nc"'
            print >> g, 'namref = "' + namref + '"'
            print >> g, 'namout = "namsurf"'
            print >> g, 'prepare_namSFX.prep_nam_SFX(case,subcase,filecase,namref,namout=namout)'

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


def install_Run(model,case,subcase,filecase,repout,config,configOut,loverwrite=False,lupdate=False,lrerun=False):

    """ Install a MUSC simulation """
    
    if lperf:
        t0 = time.time()
        t00 = time.time()

    sys.path.append('{0}/DEPHY-SCM/utils/'.format(repEMS))
    from Case import Case

    rep = os.path.join(repout,case,subcase)

    print '#'*40
    print 'Prepare MUSC simulation'
    print 'Case:', case, 'subcase:',subcase
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
    print 'Vertical grid:', config['vert_grid']

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
        os.symlink(filecase,'data_input.nc')
        os.system('cp ' + config['namATMref'] + ' namATMref')
        if config['lsurfex']:
            os.system('cp ' + config['namSFXref'] + ' namSFXref')

        if lperf:
            t1 = time.time()
            print 'First things:', t1-t0
            t0 = time.time()

    if not(flagExist) or lupdate:
        os.chdir(rep)

        # Determination NSTOP
        CASE = Case('{0}/{1}'.format(case,subcase))
        CASE.read('data_input.nc')

        startDate = CASE.startDate
        year = int(startDate[0:4])
        month = int(startDate[4:6])
        day = int(startDate[6:8])
        hour = int(startDate[8:10])
        minute = int(startDate[10:12])
        second = int(startDate[12:14])

        tstart = datetime(year,month,day,hour,minute,second)

        endDate = CASE.endDate
        year = int(endDate[0:4])
        month = int(endDate[4:6])
        day = int(endDate[6:8])
        hour = int(endDate[8:10])
        minute = int(endDate[10:12])
        second = int(endDate[12:14])

        tend = datetime(year,month,day,hour,minute,second)

        tmp = tend-tstart
        tmp = tmp.total_seconds()/3600
        NSTOP = 'h' + str(int(tmp))
        print 'NSTOP:', NSTOP

        if lperf:
            t1 = time.time()
            print 'Compute NSTOP:', t1-t0
            t0 = time.time()
            

        # Preparation namelist
        g = open('tmp.py','w')

        print >> g, 'import prepare_namATM_{0} as prepare_namATM'.format(model)
        print >> g, 'prepare_namATM.prep_nam_ATM("{0}","{1}","{2}","{3}",{4},"{5}",namout="namarp_{6}")'.format(case,subcase,filecase,config['namATMref'],config['TSTEP'],NSTOP,config['name'])

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
            print >> g, 'subcase = "' + subcase + '"'
            print >> g, 'filecase = "' + filecase + '"'
            print >> g, 'namref = "' + config['namSFXref'] + '"'
            print >> g, 'namout = "namsfx_' + config['name'] + '"'
            print >> g, 'prepare_namSFX.prep_nam_SFX(case,subcase,filecase,namref,namout=namout)'

            g.close()

            os.system('python tmp.py')
            os.system('rm -f tmp.py')

            if lperf:
                t1 = time.time()
                print 'Prepare SURFEX namelist:', t1-t0
                t0 = time.time()

        try:
            os.symlink(repRun_UTIL + 'run_{0}.sh'.format(model),'run.sh')
        except OSError:
            pass
        except:
            raise

        # Preparation fichier config de la simulation
        g = open('param_' + config['name'],'w')

        print >> g, '#!/bin/sh'
        print >> g, 'set -x'
        print >> g, '#'
        print >> g, 'MASTER=' + config['MASTER']
        print >> g, '#'
        print >> g, 'vert_grid=' + os.path.basename(config['vert_grid'])
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
        print >> g, 'dirpost=' + configOut['dirpost'] 
        print >> g, 'configpost=' + configOut['configpost']
        print >> g, 'variablesDict=' + configOut['variablesDict']
        print >> g, 'lfaformat=' + str(configOut['lfaformat'])
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
        print >> g, '. ./run.sh > run_${CONFIG}.log 2>&1'
        print >> g, 'mv run_${CONFIG}.log logs/'
        print >> g, 'echo log file: logs/run_${CONFIG}.log'
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

    elif lrerun: 
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

    print '#'*40

    if lperf:
        t1 = time.time()
        print 'Total:', t1-t00
