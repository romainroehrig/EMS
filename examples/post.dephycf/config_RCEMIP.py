#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

var2save = ['pfull','phalf','zfull','zhalf','dp','theta','ta','qv','ua','va','wap','ql','qi','qr','qsn','cl','clt','prw','pr','hfls','hfss','ts','prw','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke']

#var2save = var2save + ['tnta_radlw','tnta_radsw','tnta_turb','tnta_micro','tnta_conv','tnta_other','tnua_turb','tnua_conv','tnua_other','tnva_turb','tnva_conv','tnva_other','tnqv_conv','tnqv_turb','tnqv_micro','tnqv_other']

#var2save = var2save + ['t2m','huss','uas','vas','O3','rO3','rCO2','rCH4','rN2O','rsdt','rsut','rlut','rsds','rsus','rsdscs','rsuscs','rlds','rlus','rldscs']

var2save += ['PFP','evap','evapn','ZFP_CPTEND_NEW','ZEVAP_CPTEND_NEW','ZLIFT2_CPTEND_NEW','PEVAP_CPTEND_NEW','tnqt','tnql','tnqi','tnqr','tnqs','tnqlc','tnqic','tnqrc','tnqsnc','TOTAL_WATER','TOTAL_MASS','DRY_MASS']

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
for i in range(0,50):
  levoutz.append(10*i)
for i in range(0,11):
  levoutz.append(500+100*i)

# Niveau de print (0, 1 ou 2)
verbose = 2
