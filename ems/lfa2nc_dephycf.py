#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

import os
import glob
import logging
logger = logging.getLogger(__name__)

import numpy
import netCDF4
import numpy
import six

from ems.lfa import lfareader

isba_patch_variables = [x+'_patch' for x in ['area_fraction','lai','veg','tas','huss','hurs','sfcWind',
                                             'rsds','rsus','rss','rlds','rlus','rls','rnet',
                                             'hfls','hfsbl','hfss','hfdsl','hfdsn',
                                             'potet','et','tran',
                                             'ustar','tauu','tauv','tau',
                                             'tg','ts','qs','mrsol','wg','wgi',
                                             'albsrfc','z0','z0h']]
isba_ground_variables = [x+'_isba' for x in ['tg','wg','wgi','mrsol','mrsll','mrsfl']]

def lfa2nc(dirin, fileout, tosave=None, solib=None, varatts=None):
    """
    Converts LFA files into netcdf file
    :param dirin: directory in which LFA files are
    :param fileout: netcdf file to produce
    :param tosave: if it is a list, it contains the netcdf variable names to save
                   if it is a string, it is the name of a file containing the netcdf
                                      variable names to save (one variable name by line)
                   None to convert all variables
    :param solib: None to use epygram or
                  a path to a shared lib containing the following entries:
                  - wlfaouv / wlfafer
                  - wlfalecr / wlfaleci
                  - wlfacas
                  - wlfalaft
    Note: the shared lib can be compiled with EMS source code
          or compiled with gmkpack (-p libs4py)
          or taken in the epygram directory
    """

    _lfareader = lfareader(solib)

    # FLA filenames, and content of LFA file
    LFAs = glob.glob(os.path.join(dirin, '*.lfa'))

    # Get some parameters
    with _lfareader(LFAs[0]) as LFA0:
        variables = LFA0.listfields() #LFA variable names
        klev = LFA0.readfield('KLEV')[0] #levels number
        ncol = LFA0.readfield('NCOLUMNS')[0] if 'NCOLUMNS' in variables else None
        nground = LFA0.readfield('NGROUND_LAYER')[0] if 'NGROUND_LAYER' in variables else None
        npatch = LFA0.readfield('NPATCH')[0] if 'NPATCH' in variables else None

    # Special case of 4D variables which are written in lfa files over several 3D variables
    variables.extend([vv for vv in ['fracout', 'atb532',
                                    'cfadLidarsr532', 'dbze94',
                                    'cfadDbze94', 'clisccp',
                                    'clmodis', 'clMISR'] if vv + '_001' in variables])

    # Special case of ISBA variables with a patch dimension
    variables.extend([vv for vv in isba_patch_variables if vv + '01' in variables])
    if npatch is not None:
        for var in isba_patch_variables:
            for i in range(1,npatch+1):
                vv = var + '%(i)2.2i'%{"i": i}
                try:
                    variables.remove(vv)
                    #print(vv + ' removed')
                except ValueError:
                    pass
                except:
                    raise
    
    if varatts is None:
        varnames = {k: k for k in variables}
        names = {k: k for k in variables}
        units = {k: '-' for k in variables}
        coefs = {k: 1. for k in variables}
        inv_varnames = {k: k for k in variables}
    else:
        varnames = {k: v['varname'] for k, v in varatts.items()}
        names = {k: v['name'] for k, v in varatts.items()}
        units = {k: v['units'] for k, v in varatts.items()}
        coefs = {k: v['coef'] for k, v in varatts.items()}
        inv_varnames = {v['varname']: k for k, v in varatts.items()}

    # Which variables to save
    #logger.info('variables: ' + ' '.join(variables))
    #logger.info('tosave: ' + ' '.join(tosave))
    if tosave is None:
        tosave = variables
    else:
        if not isinstance(tosave, list):
            with open(tosave, 'r') as f:
                tosave = f.readlines()
        #convert netcdf names into LFA names
        for var in tosave:
            if var not in varnames:
                logger.warning(var + ' not in provided varnames')
                logger.warning('Adding generic information for ' + var)
                varnames[var] = var
                inv_varnames[var] = var
            elif varnames[var] not in variables:
                logger.warning('{0}/{1} not in LFA files'.format(var,varnames[var]))
        tosave = [varnames[var] for var in tosave if var in varnames and varnames[var] in variables]
        tosave = set(tosave + ['PAPRS','PAPRSF','PAPHI','PAPHIF']) #adds some LFA variables which are mandatory
    #logger.info('tosave: ' + ' '.join(tosave))

    # Do we create unsual axes ?
    llev2 = any([var in tosave for var in ['VETAF']]) # To complete
    lalt40 = any([var in tosave for var in ['clcalipso','clcalipso2','clcalipsoice','clcalipsoliq','clcalipsoun']])
    ltemp = any([var in tosave for var in ['clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun']])
    lsza5 = any([var in tosave for var in ['parasolRefl']])
    lcolumn = any([var in tosave for var in ['boxtauisccp','boxptopisccp','dbze94','atb532','fracout']])
    ldbze = any([var in tosave for var in ['cfadDbze94']])
    lsratio = any([var in tosave for var in ['cfadLidarsr532']])
    ltau = lplev7 = any([var in tosave for var in ['clisccp','clmodis']])
    lmisr = any([var in tosave for var in ['clMISR']])
    lalt40 = lalt40 or ldbze or lsratio
    lsdepth = any([var in tosave for var in isba_ground_variables])\
          or any([var in tosave for var in ['tg_patch','mrsol_patch','wg_patch','wgi_patch']])
    lpatch = any([var in tosave for var in isba_patch_variables])

    # Init nectdf file
    f = netCDF4.Dataset(fileout, 'w', format='NETCDF3_CLASSIC')
    
    level = f.createDimension('levf', klev)
    levels = f.createVariable('levf', 'i4', ('levf', ))
    levels.standard_name = 'full_level_number'
    levels.units = '-'
    levels.axis = 'Z'
    levels[:] = range(0, klev)
    
    Hlevel = f.createDimension('levh', klev + 1)
    Hlevels = f.createVariable('levh', 'i4', ('levh', ))
    Hlevels.standard_name = 'half_level_number'
    Hlevels.units = '-'
    Hlevels.axis = 'Z'
    Hlevels[:] = range(0, klev + 1)
    
    if llev2:
        level2 = f.createDimension('lev2', klev + 2)
        level2s = f.createVariable('lev2', 'i4', ('lev2', ))
        level2s.standard_name = 'Level number'
        level2s.units = '-'
        level2s[:] = range(0, klev + 2)
    
    if lalt40:
        alt40 = f.createDimension('alt40', 40)
        alt40s = f.createVariable('alt40', 'f4', ('alt40', ))
        alt40s.standard_name = 'altitude'
        alt40s.units = 'm'
        alt40s.axis = 'Z'
        alt40s[:] = [240., 720., 1200., 1680., 2160., 2640., 3120., 3600., 4080.,
                     4560., 5040., 5520., 6000., 6480., 6960., 7440., 7920., 8400.,
                     8880., 9360., 9840., 10320., 10800., 11280., 11760., 12240.,
                     12720., 13200., 13680., 14160., 14640., 15120., 15600.,
                     16080., 16560., 17040., 17520., 18000., 18480., 18960.]
    
    if ltemp:
        temp = f.createDimension('tempAxis', 40)
        temps = f.createVariable('tempAxis', 'f4', ('tempAxis', ))
        temps.standard_name = 'Temperature'
        temps.units = 'C'
        temps[:] = [-91.5, -88.5, -85.5, -82.5, -79.5, -76.5, -73.5, -70.5, -67.5, -64.5,
                    -61.5, -58.5, -55.5, -52.5, -49.5, -46.5, -43.5, -40.5, -37.5, -34.5,
                    -31.5, -28.5, -25.5, -22.5, -19.5, -16.5, -13.5, -10.5, -7.5, -4.5,
                    -1.5,  1.5,  4.5,  7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5]
    
    if lsza5:
        sza5 = f.createDimension('sza5', 5)
        sza5s = f.createVariable('sza5', 'f4', ('sza5', ))
        sza5s.standard_name = 'Solar Zenith Angle'
        sza5s.units = 'degree'
        sza5s[:] = [0., 20., 40., 60., 80.]
    
    if lcolumn: # or lvar4D['dbze94'] or lvar4D['atb532'] or lvar4D['fracout']:
        column = f.createDimension('column', ncol)
        columns = f.createVariable('column', 'i4', ('column'))
        columns.standard_name = 'column'
        columns.units = '-'
        columns[:] = [float(i) for i in range(0, ncol)]
    
    if ldbze: #lvar4D['cfadDbze94']:
        dbze = f.createDimension('dbze', 15)
        dbzes = f.createVariable('dbze', 'f4', ('dbze', ))
        dbzes.standard_name = 'CloudSat simulator equivalent radar reflectivity factor'
        dbzes.units = 'dBZ'  
        dbzes[:] = [-47.5, -42.5, -37.5, -32.5, -27.5, -22.5, -17.5, -12.5,
                    -7.5, -2.5, 2.5, 7.5, 12.5, 17.5, 22.5]
    
    if lsratio: #lvar4D['cfadLidarsr532']:
        sratio = f.createDimension('scatratio', 15)
        sratios = f.createVariable('scatratio', 'f4', ('scatratio', ))
        sratios.standard_name = 'lidar backscattering ratio'
        sratios.units = '1'
        sratios[:] = [0.005, 0.605, 2.1, 4., 6., 8.5, 12.5, 17.5, 22.5, 27.5, 35., 45., 55., 70., 50040.]
    
    if ltau: #lvar4D['clisccp'] or lvar4D['clmodis'] or lvar4D['clMISR']:  
        tau = f.createDimension('tau', 7)
        taus = f.createVariable('tau', 'f4', ('tau', ))
        taus.standard_name = 'cloud optical depth'
        taus.units = '1'  
        taus[:] = [0.15, 0.8, 2.45, 6.5, 16.2, 41.5, 100.]
    
    if lplev7: #lvar4D['clisccp'] or lvar4D['clmodis']:  
        plev7 = f.createDimension('plev7', 7)  
        plev7s = f.createVariable('plev7', 'f4', ('plev7', ))
        plev7s.standard_name = 'pressure'
        plev7s.units = 'Pa'
        plev7s.axis = 'Z'
        plev7s[:] = [90000., 74000., 62000., 50000., 37500., 24500., 9000.]
    
    if lmisr: #lvar4D['clMISR']:  
        cth16 = f.createDimension('cth16', 16)
        cth16s = f.createVariable('cth16', 'f8', ('cth16', ))
        cth16s.standard_name = 'altitude'
        cth16s.units = 'm'  
        cth16s.axis = 'Z'
        cth16s[:] = [1000. * x for x in [0., 0.25, 0.75, 1.25, 1.75, 2.25, 2.75,
                                         3.5, 4.5, 6., 8., 10., 12., 14.5, 16., 18.]]
    if nground is not None and lsdepth:
        if nground == 14:
            sdepth = f.createDimension('sdepth', nground)  
            sdepth = f.createVariable('sdepth', 'f4', ('sdepth', ))
            sdepth.standard_name = 'soil_depth'
            sdepth.units = 'm'
            #sdepth.axis = 'Z'
            sdepth[:] = [0.01,0.04,0.10,0.20,0.40,0.60,0.80,1.00,1.50,2.00,3.00,5.00,8.00,12.0]
        elif nground == 3:
            slayer = f.createDimension('slayer', nground)  
            slayer = f.createVariable('slayer', 'f4', ('slayer', ))
            slayer.standard_name = 'soil_layer_number'
            slayer.units = '-'
            #slayer.axis = 'Z'
            slayer[:] = [1, 2, 3]
            if 'tg_isba' in tosave or 'tg_patch' in tosave:
                slayer2 = f.createDimension('slayer2', 2)  
                slayer2 = f.createVariable('slayer2', 'f4', ('slayer2', ))
                slayer2.standard_name = 'soil_layer_number_for_temperature'
                slayer2.units = '-'
                #slayer2.axis = 'Z'
                slayer2[:] = [1, 2]
        elif nground == 2:
            slayer = f.createDimension('slayer', nground)  
            slayer = f.createVariable('slayer', 'f4', ('slayer', ))
            slayer.standard_name = 'soil_layer_number'
            slayer.units = '-'
            #slayer.axis = 'Z'
            slayer[:] = [1, 2]
        else:
            raise ValueError('nground value unexpected: ' + str(nground))

    if npatch is not None and lpatch:
        patch = f.createDimension('patch', npatch)  
        patch = f.createVariable('patch', 'f4', ('patch', ))
        patch.standard_name = 'patch_number'
        patch.units = '-'
        patch[:] = list(range(1,npatch+1))
 
    time = f.createDimension('time', None)
    times = f.createVariable('time', 'f4', ('time', ))
    times.calendar = 'gregorian'
    times.axis = 'T'


    # Read lfa files and write data in netcdf file
    data = {}
    shape = {}
    topdown = None
    for it, filename in enumerate(sorted(LFAs)):
        logger.info('##### Reading ' + filename.strip())
        r = _lfareader(filename)
        if it == 0:
            nindat = r.readfield('NINDAT')[0]
            year = nindat // 10000
            month = (nindat - 10000 * year) // 100
            day = nindat - 10000 * year - 100 * month
            times.units = 'seconds since %(year)4.4i-%(month)2.2i-%(day)2.2i 0:0:0.0' % \
                          {"year":year, "month":month, "day":day}

        nsssss = r.readfield('NSSSSS')[0]
        rstati = r.readfield('RSTATI')[0]
        times[it] = float(nsssss + rstati)

        
        for var in set(tosave):
            logger.debug('Reading variable {1}/{0}'.format(var,inv_varnames.get(var, var)))
            if var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']:
                raw = r.readfield(var + '_001')
            elif var in isba_patch_variables:
                raw = r.readfield(var + '01')
            else:
                try:
                    raw = r.readfield(var)
                    lcontinue = False
                except:
                    logger.info('pass variable ' + var)
                    lcontinue = True
            if lcontinue: continue
            #if f.data_model == 'NETCDF3_CLASSIC' and raw.dtype == numpy.dtype('int64'):
            #    #int64 not supported by this netcdf format
            #    raw = raw.astype(numpy.int32)
            if raw.dtype == numpy.dtype('int64'):
                raw = raw.astype(numpy.int32)
            if raw.dtype == numpy.dtype('float64'):
                raw = raw.astype(numpy.float32)
            #Netcdf variable creation
            if it == 0:
                if len(raw) == 1:
                    axis = ('time')
                if len(raw) == 2:
                    if var in isba_ground_variables:
                        axis = ('time', 'slayer2')
                    else:
                        axis = ('time', 'TOASurf')
                        if topdown is None:
                            topdown = f.createDimension('TOASurf', 2)
                if len(raw) == 3 and var in isba_ground_variables:
                    axis = ('time', 'slayer')
                if nground is not None and nground > 3 and len(raw) == nground and var in isba_ground_variables:
                    axis = ('time', 'sdepth')
                if len(raw) == klev and not(var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94',
                                                    'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']):
                    axis = ('time', 'levf')
                    coordinates = "time zfull"
                    shape[var] = (klev, )
                if len(raw) == klev + 1  and not(var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94',
                                                         'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']):
                    axis = ('time', 'levh')
                    coordinates = "time zhalf"
                    shape[var] = (klev + 1, )
                if len(raw) == klev + 2:
                    axis = ('time', 'lev2')
                    shape[var] = (klev + 2, )
                if var in ['clcalipso','clcalipso2','clcalipsoice','clcalipsoliq','clcalipsoun'] and len(raw) == 40:
                    axis = ('time', 'alt40')
                    shape[var] = (40, )
                if var in ['clcalipsotmp','clcalipsotmpice','clcalipsotmpliq','clcalipsotmpun'] and len(raw) == 40:
                    axis = ('time', 'temp')
                    shape[var] = (40, )
                if var in ['parasolRefl'] and len(raw) == 5:
                    axis = ('time', 'sza5')
                    shape[var] = (5, )
                if lcolumn and var in ['boxtauisccp', 'boxptopisccp'] and len(raw) == ncol:
                    axis = ('time', 'column')
                    shape[var] = (ncol, )
                if var in ['fracout', 'atb532', 'dbze94']:
                    axis = ('time', 'column', 'levf')
                    shape[var] = (ncol, klev)
                if var in ['cfadDbze94']:
                    axis = ('time', 'dbze', 'alt40')
                    shape[var] = (15, 40)
                if var in ['cfadLidarsr532']:
                    axis = ('time', 'sratio', 'alt40')
                    shape[var] = (15, 40)
                if var in ['clisccp', 'clmodis']:
                    axis = ('time', 'tau', 'plev7')
                    shape[var] = (7, 7)
                if var in ['clMISR']:
                    axis = ('time', 'tau', 'cth16')
                    shape[var] = (7, 16)
                if var in isba_patch_variables:
                    if var in ['tg_patch','mrsol_patch','wg_patch','wgi_patch']:
                        if nground == 2:
                            axis = ('time','patch','slayer')
                            coordinates = "time patch slayer"
                            shape[var] = (npatch, 2)
                        elif nground == 3:
                            if var == 'tg_patch':
                                axis = ('time','patch','slayer2')
                                coordinates = "time patch slayer2"
                                shape[var] = (npatch, 2)
                            else:
                                axis = ('time','patch','slayer')
                                coordinates = "time patch slayer"
                                shape[var] = (npatch, nground)
                        else:
                            axis = ('time','patch','sdepth')
                            coordinates = "time patch sdepth"
                            shape[var] = (npatch, nground)
                    else:
                        axis = ('time','patch')
                        coordinates = "time patch"
                        shape[var] = (npatch,)

                if not isinstance(raw[0], six.string_types):
                    try:
                        missingValue = numpy.iinfo(raw.dtype).max
                    except ValueError:
                        missingValue = numpy.finfo(raw.dtype).max
                    if 'isba' in var or var in isba_patch_variables:
                        missingValue = numpy.float(1.e20)

                    data[var] = f.createVariable(inv_varnames.get(var, var),
                                                 raw.dtype, axis, fill_value=missingValue)
                    data[var].missing_value = missingValue
                    data[var].standard_name = names.get(inv_varnames.get(var, None), var)
                    data[var].units = units.get(inv_varnames.get(var, None), '-')
                    try:
                        data[var].coordinates = coordinates
                    except:
                        logger.warning('Coordinates is not defined for variable', var)
                    data[var].original_name = var
                    

            #Variable filling
            if not isinstance(raw[0], six.string_types):
                if var in ['fracout', 'atb532', 'cfadLidarsr532', 'dbze94', 'cfadDbze94', 'clisccp', 'clmodis', 'clMISR']:
                    datatmp = numpy.zeros(shape[var], dtype=numpy.float)             
                    for i in range(0, shape[var][0]):
                        vv = var + '_%(i)3.3i'%{"i": i+1}
                        datatmp[i,:] = r.readfield(vv)
                elif var in isba_patch_variables:
                    datatmp = numpy.zeros(shape[var], dtype=numpy.float)             
                    for i in range(0, shape[var][0]):
                        vv = var + '%(i)2.2i'%{"i": i+1}
                        #print(vv)
                        #print(r.readfield(vv).shape)
                        #print(shape[var])
                        datatmp[i] = r.readfield(vv)
                else:
                    #print(vv, len(raw), axis)
                    datatmp = raw[0] if len(raw) == 1 else raw
                
                logger.info('Writing variable {1}/{0}: {2} ({3})'.format(var,
                            inv_varnames.get(var, var), data[var].standard_name, data[var].units))

                data[var][it] = datatmp * coefs.get(inv_varnames.get(var, None), 1)
        r.close() #LFA
    f.close() #netcdf
