#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

var2save = ['pfull','zfull','theta','ta','qv','ua','va','wap','ql','qi','qr','qsn','cl','clt','prw','pr','hfls','hfss','ts','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke','wa_up','mf_up','alpha_up','ustar','z0','dTv_up','ta_up','qv_up','ent_up','det_up','b_up','ent_up_org','ent_up_turb','det_up_org','det_up_turb','lwp','iwp','tnqt','tnthl']

convert2p = True #False
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
