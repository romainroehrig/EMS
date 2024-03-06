#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import os
import stat
import subprocess
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
        if os.path.exists(rep):
            shutil.rmtree(rep)

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
                logps=(model == 'AROME' or model == 'ARPPNT'),
                lforc_ascii=lforc_ascii, lsurfex=lsurfex,
                dirforc=dirforc, dirdiags=dirdiags,
                save_init=True, file_init='init_{0}.nc'.format(vert_grid_name),
                save_forc=True, file_forc='forc_{0}.nc'.format(vert_grid_name))
        os.symlink('nam1D_{0}'.format(vert_grid_name), 'nam1D')
        os.symlink(ASCII2FA, 'ascii2fa')
        with open('ascii2fa_{0}.log'.format(vert_grid_name), 'w') as log:
            p = subprocess.run('./ascii2fa', cwd=rep, stderr=subprocess.STDOUT, stdout=log,
                               env=dict(os.environ, OMP_NUM_THREADS='1'))
            if p.returncode != 0:
                raise RuntimeError("Error during ascii2fa execution (log: {})".format(os.path.abspath(log.name)))

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

    if model not in ['ARPCLIMAT', 'AROME', 'ARPPNT']:
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
        if os.path.exists(rep):
            shutil.rmtree(rep)

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
        env_loc = os.environ.copy()
        env_loc['DR_HOOK_NOT_MPI'] = 'true'
        with open('PGD.log', 'w') as log:
            p = subprocess.run('./PGD', cwd=rep, stderr=subprocess.STDOUT, stdout=log, env=env_loc)
            if p.returncode != 0:
                raise RuntimeError("Error during PGD execution (log: {})".format(os.path.abspath(log.name)))
        with open('PREP.log', 'w') as log:
            p = subprocess.run('./PREP', cwd=rep, stderr=subprocess.STDOUT, stdout=log, env=env_loc)
            if p.returncode != 0:
                raise RuntimeError("Error during PREP execution (log: {})".format(os.path.abspath(log.name)))
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
        if os.path.exists(rep):
            shutil.rmtree(rep)

    flagExist = os.path.exists(rep)

    if not(flagExist):
        os.makedirs(rep)
        os.chdir(rep)
        os.makedirs('./listings/')
        os.symlink(filecase,'data_input.nc')
        shutil.copy(config['namATMref'], 'namATMref')
        if config['lsurfex']:
            shutil.copy(config['namSFXref'], 'namSFXref')

        t0 = perf(t0, 'First things')

    if not(flagExist) or lupdate:
        os.chdir(rep)

        # Removing link to previous exec_dir in case it exists
        link_exec = 'exec_dir'
        logger.debug(f'Remove {link_exec} if it exists (execution directory from a previous run attempt')
        shutil.rmtree(link_exec, ignore_errors=True)

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
            os.symlink(os.path.join(ems._dirEMS, 'scripts/run.sh'), 'run.sh')
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
            g.write('echo log file: listings/run_${CONFIG}.log\n')
            g.write('date')

        os.chmod('exec.sh', os.stat('exec.sh').st_mode | stat.S_IEXEC)

        t0 = perf(t0, 'Prepare Run')

        # Execution de la simulation
        with open('exec.log', 'w') as log:
            p = subprocess.run('exec.sh', cwd=rep, stderr=subprocess.STDOUT, stdout=log)
            if p.returncode != 0:
                exec_error(rep, config)
                raise RuntimeError("Error during MUSC execution (log: {})".format(os.path.abspath(log.name)))

        t0 = perf(t0, 'Execution')

    elif lrerun: 
        logger.info('Directory already exists: ' + rep)
        logger.info('We only re-run the simulation')
        os.chdir(rep)

        # Execution de la simulation
        with open('exec.log', 'w') as log:
            p = subprocess.run('exec.sh', cwd=rep, stderr=subprocess.STDOUT, stdout=log)
            if p.returncode != 0:
                exec_error(rep, config)
                raise RuntimeError("Error during MUSC execution (log: {})".format(os.path.abspath(log.name)))

        t0 = perf(t0, 'Execution')

    else: 
        logger.info('Directory already exists: ' + rep)
        logger.info('Nothing is done')

    os.chdir(repinit)

    logger.info('#'*40)

    t0 = perf(tinit, 'Total')

def get_tmpdir(file):

    with open(file) as f:
        lines = f.readlines()

    for line in lines:
        if "TMPDIR" in line:
             tmp = line.split('=')
             return tmp[1].rsplit()[0]

    return None

def get_error(file):

    with open(file) as f:
        lines = f.readlines()

    if "MASTER" in lines[-1]:
        return "MUSC execution"

    return "Unknown"

def exec_error(rep, config):
    """
    Copy, move a few log files to help interpretation of an execution error (exec.sh)"
    Try to interpret the error and provide the location of log files for further interpretation
    """

    logrun = 'run_{}.log'.format(config['name'])
    listing_dir = os.path.join(rep, 'listings')
    logrun_abs = os.path.join(listing_dir, logrun)

    logger.debug(f'Moving {logrun} to {logrun_abs}')
    shutil.move(logrun, logrun_abs)
    tmpdir = get_tmpdir(logrun_abs)
    logger.error(f'TMPDIR is {tmpdir}')
    os.symlink(os.path.join(tmpdir), 'exec_dir')
    error = get_error(logrun_abs)
    logger.debug('Copying lola and NODE.001_01 in listings directory')
    lola_file = os.path.join(tmpdir,'lola')
    if os.path.isfile(lola_file):
        shutil.copyfile(lola_file, os.path.join(listing_dir,'lola'))
    else:
        logger.error(f'No lola file. Look at {logrun_abs}')
        raise RuntimeError
    node_file = os.path.join(tmpdir,'NODE.001_01')
    if os.path.isfile(node_file):
        shutil.copyfile(os.path.join(tmpdir,'NODE.001_01'), os.path.join(listing_dir,'NODE.001_01'))
    else:
        logger.error(f'No NODE.001_01. Look at:')
        logger.error(f'    {logrun_abs}')
        logger.error(f'    {listing_dir}/lola')
        raise RuntimeError
    logger.error("Error during " + error)
    logger.error("First check in {}".format(logrun))
    logger.error("If it is an error truly occuring during MASTER/MASTERODB execution:")
    logger.error("Look at the following files:")
    logger.error(" "*4 + f"{rep}/listings/lola")
    logger.error(" "*4 + f"{rep}/listings/NODE.001_01")
    logger.error("You may also need to check in the MUSC execution (temporary) directory:")
    logger.error(" "*4 + tmpdir)
    logger.error("Note a symbolic link to this directory has been created under:")
    logger.error(" "*4 + os.path.join(rep, "exec_dir"))



