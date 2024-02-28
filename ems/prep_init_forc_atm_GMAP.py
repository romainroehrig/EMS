#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

"""
Prepare restart and forcing files for ARPCLIMAT
"""

import os
import logging
logger = logging.getLogger(__name__)

import math
import numpy as np
import netCDF4 as nc

from ems.dephycf.Axis import Axis
from ems.dephycf.Variable import Variable
from ems.dephycf.Case import Case

lverbose = logger.getEffectiveLevel() == logging.DEBUG

variablesAux = {'SURFAEROS.SEA':       6.2E-3,
                'SURFAEROS.LAND':      2.2E-2,
                'SURFAEROS.SOOT':      1.53E-3,
                'SURFAEROS.DESERT':    2.6E-2,
                'SURFA.OF.OZONE':      7.1E-2,
                'SURFB.OF.OZONE':   3166.,
                'SURFC.OF.OZONE':      3.,
                'SURFTEMPERATURE':   292.,
                'SURFRESERV.NEIGE':    0.,
                'SURFRESERV.EAU':     10.,
                'SURFZ0.FOIS.G':      0.1,
                'SURFALBEDO':         0.07,
                'SURFEMISSIVITE':     0.98,
                'SURFET.GEOPOTENT':   0.,
                'SURFVAR.GEOP.ANI':   0.,
                'SURFVAR.GEOP.DIR':   0.,
                'SURFIND.TERREMER':   0.,
                'PROFTEMPERATURE':  292.,
                'PROFRESERV.EAU':  8000.,
                'PROFRESERV.GLACE':   0.,
                'SURFRESERV.INTER':   0.,
                'SURFRESERV.GLACE':   0.,
                'SURFIND.VEG.DOMI':   1.,
                'SURFRESI.STO.MIN':  50.,
                'SURFPROP.ARGILE':    3.944,
                'SURFPROP.SABLE':    85.09,
                'SURFEPAIS.SOL':      8.,
                'SURFIND.FOLIAIRE':   1.,
                'SURFRES.EVAPOTRA':   1.,
                'SURFGZ0.THERM':      0.1,
                'SURFPROP.VEGETAT':   0.,
                'SURFALBEDO NEIGE':   0.65,
                'SURFDENSIT.NEIGE':   0.30,
                'SURFALBEDO.SOLNU':   0.75,
                'SURFALBEDO.VEG':     0.1,
                'SURFALBEDO.COMPL':   0.1,
                'SURFZ0REL.FOIS.G':   0.1
               }


def prep_init_forc_atm(
        timestep, vertical_grid,
        nam1d='nam1d', 
        ncfile='data_input.nc',
        logps=False, lsurfex=False,
        floatfmt="{:.8}", 
        dirforc=None, dirdiags=None,
        save_init=False, file_init='init.nc',
        save_forc=False, file_forc='forc.nc', **kwargs):

    #---------------------------------------------------------------
    # A few initialisation
    #---------------------------------------------------------------

    #lforc = dirforc is not None
    lplot = dirdiags is not None
    if lplot:
        rep_images = {}
        rep_images['orig'] = os.path.join(dirdiags,'orig')
        rep_images['MUSC'] = os.path.join(dirdiags,'MUSC')
        rep_images['comp'] = os.path.join(dirdiags,'comp')
        for v in rep_images.keys():
            if not(os.path.exists(rep_images[v])):
                os.makedirs(rep_images[v])

    #---------------------------------------------------------------
    # Reading case information
    #---------------------------------------------------------------

    case = Case('tmp')
    case.read(ncfile)

    if lverbose:
        case.info()

    attributes = case.attributes

    lat = case.variables['lat'].data[0]
    lon = case.variables['lon'].data[0]

    zorog = case.variables['orog'].data[0]

    startDate = case.start_date
    year = startDate.year
    month = startDate.month
    day = startDate.day
    hour = startDate.hour
    minute = startDate.minute
    second = startDate.second

    nt, = case.variables['ps_forc'].data.shape

    #---------------------------------------------------------------
    # Surface temperature
    #---------------------------------------------------------------

    if 'ts_forc' in case.variables:
        variablesAux['SURFTEMPERATURE'] = case.variables['ts_forc'].data[0]
    elif 'tskin' in case.variables:
        variablesAux['SURFTEMPERATURE'] = case.variables['tskin'].data[0]
    elif 'ta' in case.variables:
        variablesAux['SURFTEMPERATURE'] = case.variables['ta'].data[0, 0]

    #---------------------------------------------------------------
    # Half-level pressure
    #---------------------------------------------------------------

    with open(vertical_grid, 'r') as f:
        lines = f.readlines()
    nlev_out = len(lines) - 1

    vah = np.zeros((nlev_out + 1), dtype=np.float32)
    vbh = np.zeros((nlev_out + 1), dtype=np.float32)
    for ilev in range(nlev_out + 1):
        line = lines[ilev].split()
        vah[ilev] = float(line[0])
        vbh[ilev] = float(line[1])

    pph = np.zeros((nt,nlev_out + 1),dtype=np.float32)
    for ilev in range(nlev_out + 1):
        pph[:,ilev] = vah[ilev] + vbh[ilev]*case.variables['ps_forc'].data[:]	  

    #---------------------------------------------------------------
    # Full-level pressure
    #---------------------------------------------------------------

    ppf = np.zeros((nt, nlev_out), dtype=np.float32)
    pph = np.where(pph < 0.1,0.1,pph)
    for ilev in range(nlev_out):
        # For reproductibility with CNRM machines
        for it in range(0,nt):
            ppf[it,ilev] = (pph[it, ilev + 1] * math.log(pph[it, ilev + 1]) - \
                            pph[it, ilev] * math.log(pph[it, ilev])) \
                            / (pph[it, ilev + 1] - pph[it, ilev]) - 1.
    ppf = np.exp(ppf)	  

    #---------------------------------------------------------------
    # Interpolation verticale vers les niveaux modeles (pph)
    #---------------------------------------------------------------

    levin = Axis('lev', case.variables['pa'].data[0,:], name='pa', units='Pa')
    levout = Axis('lev', ppf[0,:], name='pa', units='Pa')

    datain = {}
    dataout = {}
    for var in ['ua','va','ta','theta','qv','ql','qi','tke']:
        datain[var] = Variable(var,
            data=case.variables[var].data,
            units=case.variables[var].units,
            name=case.variables[var].name,
            level=levin,
            time=case.variables[var].time,
            pressure=case.variables['pa'].data,
            plotcoef=case.variables[var].plotcoef,
            plotunits=case.variables[var].plotunits)
        if lverbose:
            logger.debug('data before interpolation for ' + var)
            datain[var].info()

        dataout[var] = datain[var].interpol_vert(pressure=levout.data)
        if lverbose:
            logger.debug('data after interpolation for ' + var)
            dataout[var].info()

        if lplot:
            dataout[var].plotcoef = case.variables[var].plotcoef
            dataout[var].plotunits = case.variables[var].plotunits
            dataout[var].plot(rep_images=rep_images['comp'],
                              timeunits='hours', levunits='hPa',
                              var2=datain[var], label="MUSC", label2="Orig")

    #---------------------------------------------------------------
    # Preparing forcings
    #---------------------------------------------------------------

    timein = case.variables['pa_forc'].time
    nt_f = timein.length
    if nt_f <= 1:
        dt = 0.
    else:  
        dt = timein.data[1]-timein.data[0]

    def prep_forcing(var):
        din = Variable(var,
            data=case.variables[var].data,
            units=case.variables[var].units,
            name=case.variables[var].name,
            level=levin,
            time=case.variables[var].time,
            pressure=case.variables['pa_forc'],
            plotcoef=case.variables[var].plotcoef,
            plotunits=case.variables[var].plotunits)
        if lverbose:
            din.info()
        dout = din.interpol_vert(pressure=levout.data)
        if lverbose:
            dout.info()
        if lplot:
            din.plot(rep_images=rep_images['orig'], timeunits='hours',levunits='hPa')
            dout.plotcoef = case.variables[var].plotcoef
            dout.plotunits = case.variables[var].plotunits
            dout.plot(rep_images=rep_images['MUSC'],timeunits='hours',levunits='hPa')
        return dout

    dataout_forc = {}

    # Computing number of forcing fields
    nb_f = 0

    # Not yet defined in DEPHY format
    case.attributes['adv_ua'] = 0
    case.attributes['adv_va'] = 0
    if case.attributes['adv_ua'] == 1 or case.attributes['adv_va'] == 1:
        raise NotImplementedError('U/V advection not yet validated')
        nb_f += 2
        for var in ['tnua_adv', 'tnva_adv']:
            dataout_forc[var] = prep_forcing(var)

    if case.attributes['adv_ta'] == 1 or case.attributes['radiation'] == 'tend':
        nb_f += 1
        if case.attributes['adv_ta'] == 1:
            dataout_forc['tnta_adv'] = prep_forcing('tnta_adv') 
        else:
            dataout_forc['tnta_adv'] = 0
        if case.attributes['radiation'] == 'tend':
            dataout_forc['tnta_rad'] = prep_forcing('tnta_rad')
            if dataout_forc['tnta_adv'] == 0:
                dataout_forc['tnta_adv'] = dataout_forc['tnta_rad']
            else:
                dataout_forc['tnta_adv'].data += dataout_forc['tnta_rad'].data

    if case.attributes['adv_qv'] == 1:
        nb_f += 1
        dataout_forc['tnqv_adv'] = prep_forcing('tnqv_adv')

    if case.attributes['forc_geo'] == 1:
        nb_f += 2
        for var in ['ug','vg']:
            dataout_forc[var] = prep_forcing(var)

    if case.attributes['forc_wa'] == 1 or case.attributes['forc_wap'] == 1:
        nb_f += 1
        if case.attributes['forc_wa'] == 1:
            dataout_forc['wa'] = prep_forcing('wa')
        elif case.attributes['forc_wap'] == 1:
            dataout_forc['wap'] = prep_forcing('wap')

    if case.attributes['nudging_ua'] > 0. or case.attributes['nudging_va'] > 0.:
        nb_f += 2
        for var in ['ua_nud','va_nud']:
            dataout_forc[var] = prep_forcing(var)

    if case.attributes['nudging_ta'] > 0.:
        nb_f += 1
        dataout_forc['ta_nud'] = prep_forcing('ta_nud')

    if case.attributes['nudging_qv'] > 0.:
        nb_f += 1
        dataout_forc['qv_nud'] = prep_forcing('qv_nud')

    # Computing number of surface forcing fields
    nb_fs = 0
    if not(lsurfex):
        if case.attributes['surface_forcing_temp'] == 'surface_flux':
            nb_fs += 1
            dataout_forc['hfss'] = case.variables['hfss']
            nb_fs +=1
            if 'ts' in case.variables:
                dataout_forc['ts'] = case.variables['ts']
            elif 'tskin' in case.variables:
                dataout_forc['ts'] = case.variables['tskin']
            else:
                logger.warning('No surface temperature provided. It is supposed to be constant in time and equal to 300 K')
                dataout_forc['ts'] = case.variables['hfss']*0. + 300.
        else:
            raise NotImplementedError('surface_forcing_temp == {0} not implemented yet'.format(case.attributes['surface_forcing_temp']))

        if case.attributes['surface_forcing_moisture'] == 'surface_flux':
            nb_fs += 1
            dataout_forc['hfls'] = case.variables['hfls']
        else:
            raise NotImplementedError('surface_forcing_moisture == {0} not implemented yet'.format(case.attributes['surface_forcing_moisture']))

    if case.attributes['surface_forcing_wind'] == 'ustar':
        if not(lsurfex):
            nb_fs += 1
            dataout_forc['ustar'] = case.variables['ustar']
        z0 = None
        z0h = None
    elif case.attributes['surface_forcing_wind'] == 'z0':
        logger.warning('z0 is supposed to be constant in time')
        z0 = case.variables['z0'].data[0]
        if 'z0h' in case.variables:
            logger.warning('z0h is supposed to be constant in time')
            z0h = case.variables['z0h'].data[0]
        else:
            logger.warning('z0h is supposed to be constant in time and equal to z0/10')
            z0h = z0/10.
    elif case.attributes['surface_forcing_wind'] == 'none':
        # wind and z0 are computed interactively (ocean)
        z0 = None
    else:
        raise NotImplementedError('surface_forcing_wind == {0} not implemented yet'.format(case.attributes['surface_forcing_wind']))

    #---------------------------------------------------------------
    # Writing nam1D
    #---------------------------------------------------------------

    def write_forcing_in_nam1d(f, a, name, wl):
        for it in range(nt_f): 
            f.write(' ' + name + '  ' + str(int(it * dt)) + ' s\n')
            if wl:
                #vertical profile
                for ilev in range(nlev_out):
                    f.write(floatfmt.format(a[it, ilev]) + '\n')
            else:
                #surface value
                f.write(floatfmt.format(a[it]) + '\n')

    def write_profile_in_nam1d(f, a, name, wl):
        f.write(name + '\n')
        if wl:
            #vertical profile
            for ilev in range(len(a)):
                g.write(floatfmt.format(a[ilev]) + '\n')
        else:
            #surface value
            f.write(floatfmt.format(a) + '\n')

    with open(nam1d, 'w') as g:
        g.write('&NAM1D\n')
        g.write('  LMAP    = .FALSE.,\n')
        g.write('  IFLEV   = {0},\n'.format(int(nlev_out)))
        g.write('  ZDELY   = 250000.,\n')
        g.write('  LNHDYN  = .FALSE.,\n')
        g.write('  LALAPHYS= .TRUE.,\n')
        g.write('  LREASUR = .TRUE.,\n')
        g.write('  NFORC   = {0},'.format(nb_f * nt_f) + '\n')
        g.write('  NFORCS  = {0},'.format(nb_fs * nt_f) + '\n')
        g.write('  LQCGRP  = .TRUE.,\n')
        g.write('  LQIGRP  = .TRUE.,\n')
        g.write('  LQRGRP  = .FALSE.,\n')
        g.write('  LQSGRP  = .FALSE.,\n')
        g.write('  LQGGRP  = .FALSE.,\n')
        g.write('  LCFGRP  = .FALSE.,\n')
        g.write('  LSRCGRP = .FALSE.,\n')
        g.write('  LTKEGRP = .TRUE.,\n')
        g.write('  IYEAR   = {0},\n'.format(int(year)))
        g.write('  IMONTH  = {0},\n'.format(int(month))) 
        g.write('  IDAY    = {0},\n'.format(int(day)))
        g.write('  IHH     = {0},\n'.format(int(hour)))
        g.write('  IMIN    = {0},\n'.format(int(minute)))
        g.write('/\n')

        g.write('ETA\n')

        write_profile_in_nam1d(g, vah, 'vah', True)
        write_profile_in_nam1d(g, vbh, 'vbh', True)

        g.write('ATMOSPHERE\n')
        write_profile_in_nam1d(g, zorog, 'zorog', False)
        if logps:
            write_profile_in_nam1d(g, math.log(case.variables['ps'].data[0]), 'ps (Pa)', False)
        else:
            write_profile_in_nam1d(g, case.variables['ps'].data[0], 'ps (Pa)', False)
        
        write_profile_in_nam1d(g, dataout['ua'].data[0,:],   'U',           True)
        write_profile_in_nam1d(g, dataout['va'].data[0,:],   'V',           True)
        write_profile_in_nam1d(g, dataout['ta'].data[0, :],  'T',           True)
        write_profile_in_nam1d(g, dataout['qv'].data[0, :],  'QV',          True)
        write_profile_in_nam1d(g, dataout['ql'].data[0, :],  'CLOUD_WATER', True)
        write_profile_in_nam1d(g, dataout['qi'].data[0, :],  'ICE_CRYSTAL', True)
        write_profile_in_nam1d(g, dataout['tke'].data[0, :], 'TKE',         True)

        g.write('FORCING\n')

        if case.attributes['adv_ua'] == 1:
            raise NotImplementedError('U/V advection not yet validated')
            write_forcing_in_nam1d(g, dataout_forc['tnua_adv'].data, 'U ADV', wl=True)
            write_forcing_in_nam1d(g, dataout_forc['tnva_adv'].data, 'V ADV', wl=True)

        if case.attributes['adv_ta'] == 1 or case.attributes['radiation'] == 'tend':
            write_forcing_in_nam1d(g, dataout_forc['tnta_adv'].data, 'T ADV', wl=True)

        if case.attributes['adv_qv'] == 1:
            write_forcing_in_nam1d(g, dataout_forc['tnqv_adv'].data, 'Qv ADV', wl=True)

        if case.attributes['forc_geo'] == 1:
            write_forcing_in_nam1d(g, dataout_forc['ug'].data, 'PFUG', wl=True)
            write_forcing_in_nam1d(g, dataout_forc['vg'].data, 'PFVG', wl=True)

        if case.attributes['forc_wa'] == 1:
            write_forcing_in_nam1d(g, dataout_forc['wa'].data, 'W', wl=True)
        elif case.attributes['forc_wap'] == 1:
            write_forcing_in_nam1d(g, dataout_forc['wap'].data, 'OMEGA', wl=True)

        if case.attributes['nudging_ua'] > 0.:
            write_forcing_in_nam1d(g, dataout_forc['ua_nud'].data, 'U NUDGING', wl=True)
            write_forcing_in_nam1d(g, dataout_forc['va_nud'].data, 'V NUDGING', wl=True)

        if case.attributes['nudging_ta'] > 0.:
            write_forcing_in_nam1d(g, dataout_forc['ta_nud'].data, 'T NUDGING', wl=True)

        if case.attributes['nudging_qv'] > 0.:
            write_forcing_in_nam1d(g, dataout_forc['qv_nud'].data, 'Qv NUDGING', wl=True)

        g.write('SURF.FORC\n') 
        if not(lsurfex):
            if case.attributes['surface_forcing_temp'] == 'surface_flux':
                write_forcing_in_nam1d(g, dataout_forc['hfss'].data, 'FCS', wl=False)
            if case.attributes['surface_forcing_moisture'] == 'surface_flux':
                write_forcing_in_nam1d(g, dataout_forc['hfls'].data, 'FLE', wl=False)

            if case.attributes['surface_forcing_wind'] == 'ustar':
                write_forcing_in_nam1d(g, dataout_forc['hfls'].data, 'USTAR', wl=False)

            if case.attributes['surface_forcing_temp'] == 'surface_flux':
                if 'ts' in dataout_forc:
                    write_forcing_in_nam1d(g, dataout_forc['ts'].data, 'RTS', wl=False)
                elif 'tskin' in dataout_forc:
                    write_forcing_in_nam1d(g, dataout_forc['tskin'].data, 'RTS', wl=False)

        for var in variablesAux.keys():
            if var == 'SURFZ0.FOIS.G' and z0 is not None:
                write_profile_in_nam1d(g, 9.80665 * z0, var, False)
            elif var == 'SURFGZ0.THERM' and z0h is not None:
                write_profile_in_nam1d(g, 9.80665 * z0h, var, False)
            elif var == 'SURFIND.TERREMER':
                if case.attributes['surface_type'] == 'ocean':
                    lsm = 0.
                elif case.attributes['surface_type'] in ['land','landice']:
                    lsm = 1.
                write_profile_in_nam1d(g, lsm, var, False)
            else:
                write_profile_in_nam1d(g, variablesAux[var], var, False)
        
        g.write('STOP\n')

    #---------------------------------------------------------------
    # Saving initial state and forcings in netCDF files
    #---------------------------------------------------------------

    if save_init:
        with nc.Dataset(file_init, 'w', format='NETCDF3_CLASSIC') as g:
            for var in dataout.keys():
                dataout[var].write(g)

    if save_forc:
        with nc.Dataset(file_forc, 'w', format='NETCDF3_CLASSIC') as g:
            for var in dataout_forc.keys():
                dataout_forc[var].write(g)
