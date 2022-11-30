#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

var2save = ['pfull','zfull','theta','ta','qv','hur','ua','va','wap','cl','clt','pr','hfls','hfss','ustar','ts','prw','hur','Q1','Q2']


var2save = var2save + ['rsut','rsdt','rlut','rsds','rsus','rlus','rlds','rsutcs','rlutcs','rsdscs','rsuscs','rldscs','cltl','cltm','clth','lwp','iwp']


convert2p = True
convert2z = False

convertkday = False
convertpday = True
convertzday = False

convertk1h = False
convertp1h = False
convertz1h = False

convertk3h = False # not coded for True
convertp3h = False
convertz3h = False # not coded for True

# Pour convert2p, niveau en hPa
levout = []
for i in range(2,41):
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
