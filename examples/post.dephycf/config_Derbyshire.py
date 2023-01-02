#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

#var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','shf','tsurf','prw','tnqadv','tnqvpbl','tnqvc','tnqvlscp','tnqvd','tntadv','tntrlw','tntrsw','tntlscp','tntc','tntpbl','tntd','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd']

#var2save = var2save + ['alpha_up','w_up','omega_up','alpha_dn','w_dn','omega_dn','cape','T_up','qv_up','omega_ref','w_up_bud','dw_buoy','dw_fric','dw_Kd','dw_entr','dw_transp','buoy','Mf','eps_u','eps_u_org','eps_u_tur','entr_u','detr_u','dTv_up','wpqp_pbl','wpthp_pbl','wpqp_conv','wpthp_conv','aipcmt','knnd','knlab','wpup_conv','wpup_pbl','wpvp_conv','wpvp_pbl']

var2save = ['pf','zf','ph','zh','theta','qv','ql','qlc','rneb','w_up','Mf','qrc','qic','qr','qi','qsn','qsnc','precls','precc','snowls','snowc','Q1','Q2']

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

levoutz = []
for i in range(0,401):
  levoutz.append(i*50.)
levoutz.reverse()

# Niveau de print (0, 1 ou 2)
verbose = 2
