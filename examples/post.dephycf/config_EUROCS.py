#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False


var2save = ['pfull','zfull']\
    + ['theta','ta','qv','hur','ua','va','wap','ql','qi','qr','qsn','qlc','qic','qrc','qsnc','tke']\
    + ['cl','clt','prw','iwp','lwp','rwp','swp','cwp']\
    + ['iwp_rad','lwp_rad','cwp_rad','iwp_conv','lwp_conv','rwp_conv','swp_conv','cwp_conv']\
    + ['pr','hfls','hfss','ustar','z0','ts',]\
    + ['wa_up','mf_up','alpha_up','b_up','dTv_up','ent_up','det_up']\
    + ['Q1','Q2']\
    + ['tnta_diab','tnta_turb','tnta_micro','tnta_conv','tnta_radlw','tnta_radsw','tnta_other']\
    + ['tnta_adv','tnta_nud']\
    + ['tnqv_diab','tnqv_turb','tnqv_micro','tnqv_conv','tnqv_other']\
    + ['tnqv_adv','tnqv_nud']\
    + ['tnua_turb','tnua_conv','tnua_other','tnua_geo']\
    + ['tnva_turb','tnva_conv','tnva_other','tnva_geo']

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
