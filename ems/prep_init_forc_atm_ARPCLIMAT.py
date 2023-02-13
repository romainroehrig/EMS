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
        lforc_ascii=True,
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
    # Writing nam1D
    #---------------------------------------------------------------

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
        g.write('  NFORC   = 0,\n')
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
        write_profile_in_nam1d(g, case.variables['ps'].data[0], 'ps (Pa)', False)
        
        write_profile_in_nam1d(g, dataout['ua'].data[0,:],   'U',           True)
        write_profile_in_nam1d(g, dataout['va'].data[0,:],   'V',           True)
        write_profile_in_nam1d(g, dataout['ta'].data[0, :],  'T',           True)
        write_profile_in_nam1d(g, dataout['qv'].data[0, :],  'QV',          True)
        write_profile_in_nam1d(g, dataout['ql'].data[0, :],  'CLOUD_WATER', True)
        write_profile_in_nam1d(g, dataout['qi'].data[0, :],  'ICE_CRYSTAL', True)
        write_profile_in_nam1d(g, dataout['tke'].data[0, :], 'TKE',         True)

        g.write('FORCING\n')

        for var in variablesAux.keys():
            if var == 'SURFZ0.FOIS.G' and 'z0' in case.variables.keys():
                write_profile_in_nam1d(g, 9.80665 * case.variables['z0'].data[0], var, False)
            elif var == 'SURFGZ0.THERM'  and 'z0' in case.variables.keys():
                write_profile_in_nam1d(g, 9.80665 * case.variables['z0'].data[0] / 10., var, False)
            else:
                write_profile_in_nam1d(g, variablesAux[var], var, False)
        
        g.write('STOP\n')

    #---------------------------------------------------------------
    # Saving initial state in netCDF file
    #---------------------------------------------------------------

    if save_init:
        with nc.Dataset(file_init, 'w', format='NETCDF3_CLASSIC') as g:
            for var in dataout.keys():
                dataout[var].write(g)


    #---------------------------------------------------------------
    # Ecriture des forcages ARPEGE-Climat
    #---------------------------------------------------------------

    if lforc_ascii:
        timein = case.variables['ps_forc'].time
        tmin = timein.data[0]
        tmax = timein.data[-1]
        timeout = np.arange(tmin, tmax+timestep, timestep, dtype=np.float64)
        nt_out, = timeout.shape
        timeout = Axis('time',timeout,name='time',units=case.tunits)

        files_names = {'ps_forc':'Ps',
                       'ug':'ug',
                       'vg':'vg',
                       'wa':'W',
                       'wap':'Omega',
                       'tnua_adv':'du',
                       'tnva_adv':'dv',
                       'tnta_adv':'dT',
                       'tnta_rad':'dT',
                       'tnqv_adv':'dq',
                       'ua_nud':'u',
                       'va_nud':'v',
                       'ta_nud':'T',
                       'qv_nud':'q'
                       }

        def prep_forcing(var, wl):
            if wl:
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
                dout = din.interpol_time(time=timeout)
                dout = dout.interpol_vert(pressure=levout.data)
                if lverbose:
                    dout.info()
                if lplot:
                    din.plot(rep_images=rep_images['orig'], timeunits='hours',levunits='hPa')
                    dout.plotcoef = case.variables[var].plotcoef
                    dout.plotunits = case.variables[var].plotunits
                    dout.plot(rep_images=rep_images['MUSC'],timeunits='hours',levunits='hPa')
                return dout
            else:
                return case.variables[var].interpol_time(time=timeout)

        def write_forcing_in_files(a, name, wl):
            for ii in range(nt_out):
                with open('{0}/{1}_forcing_{2:0>5}.txt'.format(dirforc, name, ii), 'w') as g: 
                    if wl:
                        #vertical profile
                        for ilev in range(nlev_out):
                            g.write(floatfmt.format(a[ii, ilev]) + '\n')
                    else:
                        #surface value
                        g.write(floatfmt.format(a[ii]) + '\n')

        dataout_forc = {}
        
        var = 'ps_forc'
        dataout_forc[var] = prep_forcing(var, wl=False)
        write_forcing_in_files(dataout_forc[var].data, files_names[var], wl=False)

        if attributes['forc_geo']:
            for var in ['ug','vg']:
                dataout_forc[var] = prep_forcing(var, wl=True)
                write_forcing_in_files(dataout_forc[var].data, files_names[var], wl=True)

        if attributes['forc_wap']:
            var = 'wap'
            dataout_forc[var] = prep_forcing(var, wl=True)
            write_forcing_in_files(dataout_forc[var].data, files_names[var], wl=True)

        if attributes['forc_wa']:
            var = 'wa'
            dataout_forc[var] = prep_forcing(var, wl=True)
            write_forcing_in_files(dataout_forc[var].data, files_names[var], wl=True)

        for var in ['qv']: #['u','v','qv']:
            attloc = 'adv_{0}'.format(var)
            varloc = 'tn{0}_adv'.format(var)
            if attributes[attloc]:
                dataout_forc[varloc] = prep_forcing(varloc, wl=True)
                write_forcing_in_files(dataout_forc[varloc].data, files_names[varloc], wl=True)

        if attributes['adv_ta'] and (attributes['radiation'] == 'tend'): 
            var1 = 'tnta_adv'
            dataout_forc[var1] = prep_forcing(var1, wl=True)
            var2 = 'tnta_rad'
            dataout_forc[var2] = prep_forcing(var2, wl=True)
            write_forcing_in_files(dataout_forc[var1].data + dataout_forc[var2].data,
                                   files_names[var1], wl=True)
        elif attributes['adv_ta']:
            var = 'tnta_adv'
            dataout_forc[var] = prep_forcing(var, wl=True)
            write_forcing_in_files(dataout_forc[var].data, files_names[var], wl=True)

        elif attributes['radiation'] == 'tend':
            var = 'tnta_rad'
            dataout_forc[var] = prep_forcing(var, wl=True)
            write_forcing_in_files(dataout_forc[var].data, files_names[var], wl=True)


        for var in ['ua','va','ta','qv']:
            attloc = 'nudging_{0}'.format(var)
            varloc = '{0}_nud'.format(var)
            if attributes[attloc]:
                dataout_forc[varloc] = prep_forcing(varloc, wl=True)
                write_forcing_in_files(dataout_forc[varloc].data, files_names[varloc], wl=True)

        if save_forc:
            with nc.Dataset(file_forc, 'w', format='NETCDF3_CLASSIC') as g:
                for var in dataout_forc.keys():
                    dataout_forc[var].write(g)
    else:
        raise NotImplementedError('lforc_ascii=False not yet implemented')
