#!/usr/bin/env python
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import os
dirMUSC = os.getcwd()

import argparse
import importlib
import logging
logging.basicConfig(format='%(asctime)s - %(name)30s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

import ems
import ems.cases as CC
import ems.install_MUSC as install_MUSC


############# Some default

default = {
        # Binaries
        'ASCII2FA': os.path.join(ems._dirEMS, '../aux/ASCII2FA/bin/ascii2fa'),
        # Postprocessing
        'dirpost': os.path.join(dirMUSC,'post'),
        'variablesDict': 'variable.py',
        'defaultConfigPost': 'config_default.py',
        'caseDependent': True,
        #
        # EMS configuration
        'model': 'ARPCLIMAT',
        'lforc_ascii': True,
        'lsurfex': True,
        'loverwrite': False,
        'lupdate_ATM': True,
        'lupdate_SFX': True,
        'lupdate_RUN': False,
        #
        # RRTM
        'rrtm': os.path.join(ems._dirEMS, '../data/rrtm/rrtm.const.02.tgz'),
        #
        # SURFEX
        'ecoclimap': os.path.join(ems._dirEMS, '../data/ecoclimap_cnrm_cm6.02'),
        'sfxfmt': 'LFI'
        }

############# End editing

if __name__ == '__main__':

    # Definition of arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("-config", help="config file", type=str, required=True)
    parser.add_argument("-case", help="case", type=str, required=True)
    parser.add_argument("-subcase", help="subcase (default: REF)", type=str, default="REF")
    parser.add_argument("--atm-only", help="Only install ATM files",    dest='atm_only', action="store_true")
    parser.add_argument("--sfx-only", help="Only install SURFEX files", dest='sfx_only', action="store_true")
    parser.add_argument("--run-only", help="Only run the simulation",   dest='run_only', action="store_true")

    # Getting arguments
    args = parser.parse_args()
    case = args.case
    subcase = args.subcase
    config_file = args.config

    latm = args.atm_only
    lsfx = args.sfx_only    
    lrun = args.run_only
    lall = not(latm or lsfx or lrun)

    # check existence of config_file and then import it
    if not(os.path.isfile(config_file)):
        raise ValueError("The configuration file {0} does not exist".format(config_file))

    EXPID = config_file.split('/')[-1][7:-3]

    try:
        os.remove('./config.py')
    except OSError:
        pass
    except:
        raise

    os.symlink(config_file,"./config.py")
    CM = importlib.import_module('config') 

    # Get configuration:
    atts = {}
    atts['EXPID'] = EXPID
    # First loop over attributes which have a default (see above)
    for att in ['GROUP', 'model', 'ASCII2FA', 'lforc_ascii', 'lsurfex',
                'dirpost', 'variablesDict', 'defaultConfigPost', 'caseDependent',
                'sfxfmt', 'ecoclimap', 'rrtm',
                'loverwrite', 'lupdate_ATM', 'lupdate_SFX', 'lupdate_RUN']: 
        try:
            atts[att] = CM.__dict__[att]
        except KeyError:
            logger.warning('{0} was not given.'.format(att))
            logger.warning('=> Taking default: {0}'.format(default[att]))
            atts[att] = default[att]
            pass
        except:
            raise

    # Then loop over attributes that must be given
    attlist = ['MASTER', 'ATMNAM', 'vert_grid', 'timestep']
    if atts['lsurfex']:
        attlist += ['SFXNAM_prep', 'SFXNAM_run', 'PGD', 'PREP']
    for att in attlist:
        try:
            if att in ['SFXNAM_prep', 'SFXNAM_run'] and 'SFXNAM' in CM.__dict__.keys():
                atts[att] = CM.__dict__['SFXNAM']
            else:
                atts[att] = CM.__dict__[att]
        except KeyError:
            logger.error('{0} must be given in configuration file'.format(att))
            raise
        except:
            raise

    # dispatching variables
    GROUP = atts['GROUP']
    EXPID = atts['EXPID']

    model = atts['model']

    MASTER = atts['MASTER']
    ASCII2FA = atts['ASCII2FA']
    ATMNAM = atts['ATMNAM']
    vert_grid = atts['vert_grid']
    vert_grid_name = os.path.basename(vert_grid).split('.')[0]
    timestep = atts['timestep']

    lforc_ascii = atts['lforc_ascii']
    lsurfex = atts['lsurfex']
    if lsurfex:
        SFXNAM_prep = atts['SFXNAM_prep']
        SFXNAM_run = atts['SFXNAM_run']
        PGD = atts['PGD']
        PREP = atts['PREP']
    sfxfmt = atts['sfxfmt']
    ecoclimap = atts['ecoclimap']
    rrtm = atts['rrtm']

    dirpost = atts['dirpost']
    variablesDict = atts['variablesDict']
    defaultConfigPost = atts['defaultConfigPost']
    caseDependent = atts['caseDependent']

    loverwrite = atts['loverwrite']
    lupdate_ATM = atts['lupdate_ATM']
    lupdate_SFX = atts['lupdate_SFX']
    lupdate_RUN = atts['lupdate_RUN']

    # Check whether case/subcase is available
    CC.check(case,subcase)

    # Input netCDF case file
    data_input = CC.data_input[case][subcase]

    logger.info('#'*40)
    logger.info('Preparing and running {0}/{1}'.format(GROUP, EXPID))
    logger.info('For {0}/{1} case'.format(case,subcase))

    # Where to prepare atmospheric input data
    #repATM = os.path.join(dirMUSC, 'ATM', model)
    repATM = os.path.join(dirMUSC, 'ATM', GROUP, EXPID)
    # Where to prepare surfex input data
    repSFX = os.path.join(dirMUSC, 'SURFEX', GROUP, EXPID)
    # Where to install run data
    repRUN = os.path.join(dirMUSC, 'simulations', GROUP, EXPID)

    ########## Prepare Atmospheric input data

    if latm or lall:
        # To check whether installation has already been done
        install_file = os.path.join(repATM, case, subcase, 'installed_{0}_{1}s'.format(vert_grid_name, timestep))
        installed = os.path.isfile(install_file)

        if not(installed) or loverwrite or lupdate_ATM:
            # Installing
            install_MUSC.install_atm(model, case, subcase, data_input, 
                    repATM, vert_grid, timestep, 
                    ASCII2FA=ASCII2FA, lforc_ascii=lforc_ascii, lsurfex=lsurfex,
                    loverwrite=loverwrite, lupdate=lupdate_ATM)

            os.system('touch {0}'.format(install_file))
        else:
            logger.info('ATM data for {0}/{1} already installed, loverwrite={2}, lupdate={3}'.format(case, subcase, loverwrite, lupdate_ATM))

    ########## Prepare Surfex input data

    if lsurfex and (lsfx or lall):
        # To check whether installation has already been done
        install_file = os.path.join(repSFX, case, subcase, 'installed')
        installed = os.path.isfile(install_file)

        if not(installed) or loverwrite or lupdate_SFX:
            # Installing
            install_MUSC.install_sfx(model, case, subcase, data_input,
                    repSFX, PGD, PREP, SFXNAM_prep,
                    loverwrite=loverwrite, lupdate=lupdate_SFX, ecoclimap=ecoclimap, sfxfmt=sfxfmt)

            os.system('touch {0}'.format(install_file))
        else:
            logger.info('SURFEX data for {0}/{1} already installed, loverwrite={2}, lupdate={3}'.format(case, subcase, loverwrite, lupdate_SFX))

    ########## Run

    if lrun or lall:
        # Preparing run configuration
        config = {}
        config['name'] = EXPID
        config['MASTER'] = MASTER
        config['ecoclimap'] = ecoclimap
        config['rrtm'] = rrtm
        config['sfxfmt'] = sfxfmt
        config['vert_grid'] = vert_grid
        config['TSTEP'] = timestep
        config['lsurfex'] = lsurfex
        config['namATMref'] = ATMNAM
        config['initfile'] = os.path.join(repATM, case, subcase, 'initfile_{0}'.format(vert_grid_name))
        if model == 'ARPCLIMAT':
            config['forcingfiles'] = os.path.join(repATM, case, subcase, 'files_{0}_{1}s'.format(vert_grid_name,timestep))
        if lsurfex:
            config['namSFXref'] = SFXNAM_run
            config['PGDfile'] = os.path.join(repSFX, case, subcase, 'PGD.{0}'.format(sfxfmt.lower()))
            config['PREPfile'] = os.path.join(repSFX, case, subcase, 'PREP.{0}'.format(sfxfmt.lower()))

        # Preparing post-processing configuration
        configOut = {}
        configOut['dirpost'] = dirpost
        configOut['variablesDict'] = variablesDict

        if caseDependent:
            tmp = os.path.join(dirpost,'config_{0}.py'.format(case))
            if os.path.isfile(tmp):
                configOut['configpost'] = 'config_{0}.py'.format(case)
            else:
                logger.warning('no postprocessing configuration file for case {0} - using default'.format(case))
                configOut['configpost'] = defaultConfigPost
        else:
            configOut['configpost'] = defaultConfigPost

        # To check whether installation has already been done
        install_file = os.path.join(repRUN, case, subcase, 'installed')
        installed = os.path.isfile(install_file)

        # Installing
        if not(installed) or loverwrite or lupdate_RUN:
            install_MUSC.install_run(model,case,subcase,data_input,
                    repRUN,config,configOut,
                    loverwrite=loverwrite,lupdate=lupdate_RUN)
            os.system('touch {0}'.format(install_file))
        else:
            logger.info('Run data for {0}/{1} already installed, loverwrite={2}, lupdate={3}'.format(case, subcase, loverwrite, lupdate_RUN))

    os.remove("./config.py")
    os.remove("./config.pyc")
 
