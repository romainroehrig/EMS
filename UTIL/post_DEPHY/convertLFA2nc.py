#!/usr/bin/env python
# -*- coding:UTF-8 -*-
import os, sys
import config

if not(os.path.exists('netcdf')):
    os.makedirs('netcdf')
else:
    os.system('rm -f netcdf/*')

os.system('python lfa2nc_part1.py')
os.system('cdat lfa2nc_part2.py')

os.system('mv Out_klevel.nc netcdf/')

if config.convertk1h:
    os.system('cdat convertk_to_1hourly.py')

if config.convertk3h:
    print 'convertk3h not coded yet'
    pass #sys.exit()
    #os.system('cdat convertk_to_3hourly.py')

if config.convertkday:
    os.system('cdat convertk_to_daily.py')


if config.convert2p:
    os.system('cdat convert2p.py')
    if config.convertp1h:
        os.system('cdat convertp_to_1hourly.py')

    if config.convertp3h:
        os.system('cdat convertp_to_3hourly.py')

    if config.convertpday:
        os.system('cdat convertp_to_daily.py')


if config.convert2z:
    os.system('cdat convert2z.py')
    if config.convertz1h:
        os.system('cdat convertz_to_1hourly.py')

    if config.convertz3h:
        print 'convertz3h not coded yet'
        pass #sys.exit()
        #os.system('cdat convertz_to_3hourly.py')

    if config.convertzday:
        os.system('cdat convertz_to_daily.py')

