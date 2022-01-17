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
        'mf','alpha_up','wa_up','ta_up','qv_up','b_up','dTv_up',
        'ent_up','ent_up_org','ent_up_turb','det_up','det_up_org','det_up_turb',
        'dw_buoy','dw_fric','dw_Kd','dw_entr','dw_transp'
        'Q1','tnta_turb','tnta_conv','tnta_micro',
        'Q2','tnqv_turb','tnqv_micro','tnqv_conv',
        ]

convert2p = False
convert2z = False
#convert2z = False

convertkday = False
convertpday = False
convertzday = False

convertk1h = False
convertp1h = False
convertz1h = False

convertk3h = False # not coded for True
convertp3h = False
convertz3h = False # not coded for True

# Pour convert2p, niveau en hPa
levout = []
for i in range(1,41):
  levout.append(i*25.)
levout.reverse()

# Pour convert2z, niveau en m
levoutz = []
for i in range(0,401):
  levoutz.append(i*50.)
levoutz.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2
