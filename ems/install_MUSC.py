#!/usr/bin/env python
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import os
import shutil
import logging
logger = logging.getLogger(__name__)

import ems

# Global variables
repinit = os.getcwd()

# For performance monitoring
import time
lperf = False
def perf_init():
    return time.time()

def perf(tin, s):
    if lperf:
        t = time.time()
        logger.info('PERF - {0}: {1} '.format(s,t-tin))
    return time.time()

# Main functions
def install_atm(model, case, subcase, filecase,
                repout, vert_grid, timestep=None, 
                ASCII2FA=os.path.join(ems._dirEMS, '../aux/ASCII2FA/bin/ascii2fa'),
                lforc_ascii=True, lsurfex=True, loverwrite=False, lupdate=False):

    """ Prepare files of atmospheric initial conditions and forcing needed to run MUSC """

    rep = os.path.join(repout,case,subcase)

    logger.info('#'*40)
    logger.info('Prepare Atmospheric files')
    logger.info('Case: ' + case + ' subcase: ' + subcase)
    logger.info('Vertical grid: ' + vert_grid)
    logger.info('timestep: ' + str(timestep))
    logger.info('ASCII2FA: ' + ASCII2FA)
    logger.info('Installation in ' + rep)

    vert_grid_file = os.path.basename(vert_grid)
    vert_grid_name = vert_grid_file.split('.')[0]

    if (loverwrite):
        os.system('rm -rf ' + rep)

    flagExist = os.path.exists(rep)

    if not(flagExist):
        os.makedirs(rep)
        os.chdir(rep)
        os.symlink(filecase, 'data_input.nc')
        os.symlink(vert_grid,vert_grid_file)
    else:
        logger.info('Directory already exists: ' + rep)
    
    if not(flagExist) or lupdate:
        os.chdir(rep)
        if lupdate:
            try:
                os.symlink(vert_grid,vert_grid_file)
            except OSError:
                pass
            except:
                raise

        # Directory for forcings
        if lforc_ascii:
            dirforc = 'files_{0}_{1}s/'.format(vert_grid_name, int(timestep)) if timestep is not None else None
            dirforc = os.path.join(rep,dirforc)
            if os.path.exists(dirforc):
                shutil.rmtree(dirforc)
            os.mkdir(dirforc)
        else:
            dirforc = None

        # Directory for diagnostics
        dirdiags = 'images/'
        dirdiags = os.path.join(rep,dirdiags)
        if os.path.exists(dirdiags):
            shutil.rmtree(dirdiags)
        os.mkdir(dirdiags)

        # Prepare restart and forcing
        ems.prep_init_forc_atm(model,
                timestep, vert_grid_file, #timestep and vertical grid description file
                nam1d='nam1D_{0}'.format(vert_grid_name), #output namelist
                ncfile='data_input.nc', #case description
                logps=(model == 'AROME46t1'),
                lforc_ascii=lforc_ascii, lsurfex=lsurfex,
                dirforc=dirforc, dirdiags=dirdiags,
                save_init=True, file_init='init_{0}.nc'.format(vert_grid_name),
                save_forc=True, file_forc='forc_{0}.nc'.format(vert_grid_name))
        os.symlink('nam1D_{0}'.format(vert_grid_name), 'nam1D')
        os.environ['OMP_NUM_THREADS'] = '1'
        os.symlink(ASCII2FA, 'ascii2fa')
        result = os.system('./ascii2fa > ascii2fa_{0}.log 2>&1'.format(vert_grid_name))
        if os.WEXITSTATUS(result) != 0:
            raise RuntimeError("Error during ascii2fa execution")
        os.rename('1D.file', 'initfile_{0}'.format(vert_grid_name))
        os.remove('nam1D')
    else:
        logger.info('Nothing is done')

    os.chdir(repinit)

    logger.info('#'*40)
      

def install_sfx(model, case, subcase, filecase, repout,
                PGD, PREP, namref, 
                loverwrite=False, lupdate=False,
                ecoclimap=os.path.join(ems._dirEMS, '../data/ecoclimap_cnrm_cm6.02'),
                sfxfmt='LFI'):

    """ Prepare files of atmospheric initial conditions and forcing needed to run MUSC """

    if model not in ['ARPCLIMAT','AROME46t1']:
        raise NotImplementedError('SURFEX preparation is not coded for model=', model)

    t0 = perf_init()
    tinit = t0

    rep = os.path.join(repout, case, subcase)

    logger.info('#'*40)
    logger.info('Prepare SFX files')
    logger.info('Case: ' + case + 'subcase: ' + subcase)
    logger.info('PGD: ' + PGD)
    logger.info('PREP: ' + PREP)
    logger.info('namref: ' + namref)
    logger.info('Installation in ' + rep)

    assert os.path.exists(PGD) and os.path.exists(PREP), "PGD and PREP executables must exist"

    if (loverwrite):
        os.system('rm -rf ' + rep)

    flagExist = os.path.exists(rep)

    if not(flagExist):
        os.makedirs(rep)
        os.chdir(rep)
        os.symlink(filecase, 'data_input.nc')
        os.symlink(ecoclimap, 'ecoclimap')
        os.symlink(PGD, 'PGD')
        os.symlink(PREP, 'PREP')

        t0 = perf(t0, 'First things')
    else:
        logger.info('Directory already exists: ' + rep)

    if not(flagExist) or lupdate:
        os.chdir(rep)

        # Preparation namelist SURFEX
        ems.prep_nam_sfx("data_input.nc", namref, namout="namsurf", sfxfmt=sfxfmt)
    
        t0 = perf(t0, 'Prepare SURFEX namelist')

        # PGD and PREP
        os.symlink('namsurf', 'OPTIONS.nam')
        for f in ['ecoclimapII_eu_covers_param.bin', 'ecoclimapI_covers_param.bin']:
            os.symlink(os.path.join('ecoclimap', f), f)
        result = os.system('./PGD > PGD.log 2>&1')
        if os.WEXITSTATUS(result) != 0:
            raise RuntimeError("Error during PGD execution")
        result = os.system('./PREP > PREP.log 2>&1')
        if os.WEXITSTATUS(result) != 0:
            raise RuntimeError("Error during PREP execution")
        for f in ['PGD.des', 'class_cover_data.tex', 'PREP.des']:
            try:
                os.remove(f)
            except OSError:
                pass
            except:
                raise


        t0 = perf(t0, 'Prepare PGD/PREP')
    else: 
        logger.info('Nothing is done')

    os.chdir(repinit)

    t0= perf(tinit, 'Total')

    logger.info('#'*40)

def install_run(model,case,subcase,filecase,repout,config,configOut,loverwrite=False,lupdate=False,lrerun=False):

    """ Install a MUSC simulation """
    
    t0 = perf_init()
    tinit = t0

    #from ems.dephycf.Case import Case

    rep = os.path.join(repout,case,subcase)

    logger.info('#'*40)
    logger.info('Prepare MUSC simulation')
    logger.info('Case: ' + case + ' subcase: ' + subcase)
    logger.info('MASTER: ' + config['MASTER'])
    logger.info('Configuration name: ' + config['name'])
    logger.info('{0} reference namelist: {1}'.format(model, config['namATMref']))
    if config['lsurfex']:
        logger.info('SURFEX reference namelist: ' + config['namSFXref'])
        logger.info('Ecoclimap directory: ' + config['ecoclimap'])
        logger.info('SURFEX PGD/PREP format: ' + config['sfxfmt'])
    logger.info('Initial Conditions file: ' + config['initfile'])
    logger.info('rrtm files: {0}'.format(config['rrtm']))
    if model == 'ARPCLIMAT':
        logger.info('Atmospheric forcing files: ' + config['forcingfiles'])
    if config['lsurfex']:
        logger.info('PGD file: ' + config['PGDfile'])
        logger.info('PREP file: ' + config['PREPfile'])
    logger.info('Timestep: ' + str(config['TSTEP']))
    logger.info('Vertical grid: ' + config['vert_grid'])

    logger.info('Postprocessing:')
    logger.info('dirpost: ' + configOut['dirpost'])
    logger.info('configpost: ' + configOut['configpost'])
    logger.info('variablesDict: ' + configOut['variablesDict'])

    logger.info('Installation in ' + rep)


    if (loverwrite):
        os.system('rm -rf ' + rep)

    flagExist = os.path.exists(rep)

    if not(flagExist):
        os.makedirs(rep)
        os.chdir(rep)
        os.makedirs('./listings/')
        os.symlink(filecase,'data_input.nc')
        os.system('cp ' + config['namATMref'] + ' namATMref')
        if config['lsurfex']:
            os.system('cp ' + config['namSFXref'] + ' namSFXref')

        t0 = perf(t0, 'First things')

    if not(flagExist) or lupdate:
        os.chdir(rep)

        # Preparation namelist
        NSTOP = ems.prep_nam_atm(model, 'data_input.nc',
                         config['namATMref'], config['TSTEP'],
                         namout="namarp_{0}".format(config['name']),
                         lsurfex=config['lsurfex'])

        t0 = perf(t0, 'Prepare {0} namelist'.format(model))

        # Preparation namelist SURFEX
        if config['lsurfex']:
            ems.prep_nam_sfx(filecase, config['namSFXref'], namout="namsfx_" + config['name'], sfxfmt=config['sfxfmt'])

            t0 = perf(t0, 'Prepare SURFEX namelist')

        try:
            os.symlink(os.path.join(ems._dirEMS, 'scripts/run_{0}.sh'.format(model)), 'run.sh')
        except OSError:
            pass
        except:
            raise

        # Preparation fichier config de la simulation
        with open('param_' + config['name'], 'w') as g:
            g.write('#!/bin/sh\n')
            g.write('set -x\n')
            g.write('#\n')
            g.write('model=' + model + '\n')
            g.write('#\n')
            g.write('MASTER=' + config['MASTER'] + '\n')
            g.write('#\n')
            g.write('vert_grid=' + os.path.basename(config['vert_grid']) + '\n')
            g.write('TSTEP=' + str(config['TSTEP']) + '\n')
            g.write('NSTOP=' + NSTOP + '\n')
            g.write('#\n')
            g.write('CONFIG=' + config['name'] + '\n')
            g.write('NAMARP=namarp_' + config['name'] + '\n')
            if config['lsurfex']:
                g.write('NAMSFX=namsfx_' + config['name'] + '\n')
            g.write('#\n')
            g.write('RRTM=' + config['rrtm'] + '\n')
            g.write('#\n')
            g.write('INITFILE=' + config['initfile'] + '\n')
            if model == 'ARPCLIMAT':
                g.write('FORCING_FILES=' + config['forcingfiles'] + '\n')
            if config['lsurfex']:
                g.write('PGD=' + config['PGDfile'] + '\n')
                g.write('PREP=' + config['PREPfile'] + '\n')
            g.write('ecoclimap=' + config['ecoclimap'] + '\n')
            g.write('#\n')
            g.write('dirpost=' + configOut['dirpost'] + '\n')
            g.write('configpost=' + configOut['configpost'] + '\n')
            g.write('variablesDict=' + configOut['variablesDict'] + '\n')
            g.write('installpost=True\n')
            g.write('runpost=True\n')

        os.system('chmod u+x param_' + config['name'])

        # Preparation fichier d'execution de la simulation
        with open('exec.sh', 'w') as g:
            g.write('#!/bin/sh\n')
            g.write('set -x\n')
            g.write('date\n')
            g.write('rm -f param\n')
            g.write('ln -s param_' + config['name'] + ' param\n')
            g.write('. ./param\n')
            g.write('. ./run.sh > run_${CONFIG}.log 2>&1\n')
            g.write('mv run_${CONFIG}.log listings/\n')
            g.write('echo log file: logs/run_${CONFIG}.log\n')
            g.write('date')

        os.system('chmod u+x exec.sh')

        t0 = perf(t0, 'Prepare Run')

        # Execution de la simulation
        result = os.system('./exec.sh > exec.log 2>&1')
        if os.WEXITSTATUS(result) != 0:
            raise RuntimeError("Error during MUSC execution")

        t0 = perf(t0, 'Execution')

    elif lrerun: 
        logger.info('Directory already exists: ' + rep)
        logger.info('We only re-run the simulation')
        os.chdir(rep)

        # Execution de la simulation
        result = os.system('./exec.sh > exec.log 2>&1')
        if os.WEXITSTATUS(result) != 0:
            raise RuntimeError("Error during MUSC execution")

        t0 = perf(t0, 'Execution')

    else: 
        logger.info('Directory already exists: ' + rep)
        logger.info('Nothing is done')

    os.chdir(repinit)

    logger.info('#'*40)

    t0 = perf(tinit, 'Total')
