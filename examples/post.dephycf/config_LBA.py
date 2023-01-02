#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False


var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','shf','tsurf','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke','w_up','Mf','alpha_up','ustar','z0']

var2save = var2save + ['tntrlw','tntrsw','tntpbl','tntlscp','tntc','tntd','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd','tntadv','tntnudg','tnugeo','tnvgeo','tendu','tendv','tendq','tendh']

var2save = var2save + ['delta_t','delta_q','dt_wake','dq_wake','dt_wake2','dq_wake2','omega_up','alpha_up','wake_fip','Cstar','hw','ZMWAKE','ZCWAKE','ZS12','ZCTH','ZMTH','ZALPTH','ZWUP','ZALPHAUP','sigmaw']

var2save = var2save + ['buoy','w_up_bud','dw_buoy','dw_fric','dw_Kd','dw_entr','dw_transp','dTv_up','ZUDALWAKE','ZBUOWAKE','ZALFW','ZWWMWX','ZVVER','ZDOMEGA','ZWW','ZWX','ZMW','ZMX','ZMTOT']


convert2p = False
convert2z = False

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
