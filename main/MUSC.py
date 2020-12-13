#!/usr/bin/env python
# -*- coding:UTF-8 -*-

import os
REP_EMS = os.getenv('REP_EMS')
REP_MUSC = os.getenv('REP_MUSC')

import argparse
import importlib

import EMS_cases as CC
import install_MUSC


############# Some default

default = {
        # Postprocessing
        'dirpost': os.path.join(REP_MUSC,'post'),
        'variablesDict': 'variable.py',
        'defaultConfigPost': 'config_default.py',
        'caseDependent': True,
        #
        # EMS configuration
        'model': 'ARPCLIMAT',
        'lsurfex': True,
        'loverwrite': False,
        'lupdate_ATM': True,
        'lupdate_SFX': True,
        'lupdate_RUN': False,
        #
        # ecoclimap data
        'ecoclimap': os.path.join(REP_EMS, 'UTIL/ecoclimap_cnrm_cm6.02'),
        }

############# End editing

if __name__ == '__main__':

    # Definition of arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("-config", help="config file", type=str, required=True)
    parser.add_argument("-case", help="case", type=str, required=True)
    parser.add_argument("-subcase", help="subcase (default: REF)", type=str, default="REF")

    # Getting arguments
    args = parser.parse_args()
    case = args.case
    subcase = args.subcase
    config_file = args.config

    # check existence of config_file and then import it
    if not(os.path.isfile(config_file)):
        raise ValueError("The configuration file {0} does not exist".format(config_file))

    configloc = config_file.split('/')[-1]

    try:
        os.remove('./{0}'.format(configloc))
    except OSError:
        pass
    except:
        raise

    os.symlink(config_file,"./{0}".format(configloc))
    
    CM = importlib.import_module(configloc[:-3]) 

    # Get configuration:
    atts = {}
    # First loop over attributes which have a default (see above)
    for att in ['GROUP','model','lsurfex','dirpost','variablesDict','defaultConfigPost','caseDependent','ecoclimap','loverwrite','lupdate_ATM','lupdate_SFX','lupdate_RUN']: 
        try:
            atts[att] = CM.__dict__[att]
        except KeyError:
            print 'WARNING: {0} was not given. Taking default: {1}'.format(att, default[att])
            atts[att] = default[att]
            pass
        except:
            raise

    # Then loop over attributes that must be given
    attlist = ['EXPID','MASTER','ATMNAM','vert_grid','timestep']
    if atts['lsurfex']:
        attlist += ['SFXNAM','PGD','PREP']
    for att in attlist:
        try:
            atts[att] = CM.__dict__[att]
        except KeyError:
            print 'ERROR: {0} must be given in configuration file'.format(att)
            raise
        except:
            raise

    # dispatching variables
    GROUP = atts['GROUP']
    EXPID = atts['EXPID']

    model = atts['model']

    MASTER = atts['MASTER']
    ATMNAM = atts['ATMNAM']
    vert_grid = atts['vert_grid']
    timestep = atts['timestep']

    lsurfex = atts['lsurfex']
    if lsurfex:
        SFXNAM = atts['SFXNAM']
        PGD = atts['PGD']
        PREP = atts['PREP']
    ecoclimap = atts['ecoclimap']

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

    print '#'*40
    print 'Preparing and running {0}/{1}'.format(GROUP, EXPID)
    print 'For {0}/{1} case'.format(case,subcase)


    ########## Prepare Atmospheric input data

    # Where to prepare atmospheric input data
    repATM = os.path.join(REP_MUSC, 'ATM', model)

    # To check whether installation has already been done
    vert_grid_name = os.path.basename(vert_grid).split('.')[0]
    install_file = os.path.join(repATM, case, subcase, 'installed_{0}_{1}s'.format(vert_grid_name, timestep))
    installed = os.path.isfile(install_file)

    if not(installed) or loverwrite or lupdate_ATM:
        # Installing
        install_MUSC.install_ATM(model, case, subcase, data_input, 
                repATM, vert_grid, timestep, 
                loverwrite=loverwrite, lupdate=lupdate_ATM)

        os.system('touch {0}'.format(install_file))
    else:
        print 'ATM data for {0}/{1} already installed, loverwrite={2}, lupdate={3}'.format(case, subcase, loverwrite, lupdate_ATM)

    ########## Prepare Surfex input data
    if lsurfex:
        # Where to prepare surfex input data
        repSFX = os.path.join(REP_MUSC, 'SURFEX', GROUP, EXPID)

        # To check whether installation has already been done
        install_file = os.path.join(repSFX, case, subcase, 'installed')
        installed = os.path.isfile(install_file)

        if not(installed) or loverwrite or lupdate_SFX:
            # Installing
            install_MUSC.install_SFX(model, case, subcase, data_input,
                    repSFX, PGD, PREP, SFXNAM,
                    loverwrite=loverwrite, lupdate=lupdate_SFX, ecoclimap=ecoclimap)

            os.system('touch {0}'.format(install_file))
        else:
            print 'SURFEX data for {0}/{1} already installed, loverwrite={2}, lupdate={3}'.format(case, subcase, loverwrite, lupdate_SFX)

    ########## Run

    # Where to install run data
    repRUN = os.path.join(REP_MUSC, 'simulations', GROUP, EXPID)

    # Preparing run configuration
    config = {}
    config['name'] = EXPID
    config['MASTER'] = MASTER
    config['ecoclimap'] = ecoclimap
    config['vert_grid'] = vert_grid
    config['TSTEP'] = timestep
    config['lsurfex'] = lsurfex
    config['namATMref'] = ATMNAM
    config['initfile'] = os.path.join(repATM, case, subcase, 'initfile_{0}'.format(vert_grid_name))
    if model == 'ARPCLIMAT':
        config['forcingfiles'] = os.path.join(repATM, case, subcase, 'files_{0}_{1}s'.format(vert_grid_name,timestep))
    if lsurfex:
        config['namSFXref'] = SFXNAM
        config['PGDfile'] = os.path.join(repSFX, case, subcase, 'PGD.lfi')
        config['PREPfile'] = os.path.join(repSFX, case, subcase, 'PREP.lfi')

    # Preparing post-processing configuration
    configOut = {}
    configOut['dirpost'] = dirpost
    configOut['variablesDict'] = variablesDict

    if caseDependent:
        tmp = os.path.join(dirpost,'config_{0}.py'.format(case))
        if os.path.isfile(tmp):
            configOut['configpost'] = 'config_{0}.py'.format(case)
        else:
            print 'WARNING: no postprocessing configuration file for case {0} - using default'.format(case)
            configOut['configpost'] = defaultConfigPost
    else:
        configOut['configpost'] = defaultConfigPost

    # To check whether installation has already been done
    install_file = os.path.join(repRUN, case, subcase, 'installed')
    installed = os.path.isfile(install_file)

    # Installing
    if not(installed) or loverwrite or lupdate_RUN:
        install_MUSC.install_Run(model,case,subcase,data_input,
                repRUN,config,configOut,
                loverwrite=loverwrite,lupdate=lupdate_RUN)
        os.system('touch {0}'.format(install_file))
    else:
        print 'Run data for {0}/{1} already installed, loverwrite={2}, lupdate={3}'.format(case, subcase, loverwrite, lupdate_RUN)

    os.remove("./{0}".format(configloc))
    os.remove("./{0}c".format(configloc))
 
