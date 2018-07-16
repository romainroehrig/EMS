#!/usr/bin/env python
# -*- coding:UTF-8 -*-
import os, sys
import config

if not(os.path.exists('netcdf')):
    os.makedirs('netcdf')
else:
    os.system('rm -f netcdf/*')

os.system('python lfa2nc_part1.py')
os.system('python lfa2nc_part2.py')

os.system('mv Out_klevel.nc netcdf/')

if config.convertk1h:
    os.system('python convertk_to_1hourly.py')

if config.convertk3h:
    print 'convertk3h not coded yet'
    pass #sys.exit()
    #os.system('python convertk_to_3hourly.py')

if config.convertkday:
    os.system('python convertk_to_daily.py')


if config.convert2p:
    os.system('python convert2p.py')
    if config.convertp1h:
        os.system('python convertp_to_1hourly.py')

    if config.convertp3h:
        os.system('python convertp_to_3hourly.py')

    if config.convertpday:
        os.system('python convertp_to_daily.py')


if config.convert2z:
    os.system('python convert2z.py')
    if config.convertz1h:
        os.system('python convertz_to_1hourly.py')

    if config.convertz3h:
        print 'convertz3h not coded yet'
        pass #sys.exit()
        #os.system('python convertz_to_3hourly.py')

    if config.convertzday:
        os.system('python convertz_to_daily.py')

