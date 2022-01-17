#!/usr/bin/env python
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

#var2save = ['pfull','zfull','theta','qv','cl']

var2save = ['pfull','zfull','theta','ta','qv','hur','ua','va','wap','tke',
        'ql','qi','qr','qsn','qlc','qic','qrc','qsnc',
        'cl','clt','prw','lwp',
        'pr','hfls','hfss','ustar','ts','z0',
        'Q1','tnta_turb','tnta_conv','tnta_micro',
        'tnua_turb','tnva_turb',
        'ECT0','ECT1','TNECT_DYN','TNECT_BUO','TNECT_DIF','TNECT_DIS','KU'
        ]

convert2p = False
convert2z = True #False

convertkday = False
convertpday = False
convertzday = False

convertk1h = False
convertp1h = False
convertz1h = True #False

convertk3h = False # not coded for True
convertp3h = False
convertz3h = False # not coded for True

# Pour convert2p, niveau en hPa
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

# Pour convert2z, niveau en m
levoutz = range(0,402,2)

# Niveau de print (0, 1 ou 2)
verbose = 2
