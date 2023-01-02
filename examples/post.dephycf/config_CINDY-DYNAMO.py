#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

#var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','lhfn','shf','tsurf','prw','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke']
var2save = ['pf','zf','theta','temp','qv','hur','u','v','wap','rneb','cc','rain','lhf','shf','ustar','tsurf','prw','hur','Q1','Q2']

#var2save = var2save + ['tntrlw','tntrsw','tntpbl','tntlscp','tntc','tntd','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd','tntadv','tntnudg','tnugeo','tnvgeo','tendu','tendv','tendq','tendh']

var2save = var2save + ['rsut','rsdt','rlut','rsds','rsus','rlus','rlds','rsutcs','rlutcs','rsdscs','rsuscs','rldscs','cltl','cltm','clth','lwp','iwp']

#var2save = ['pf','zf','theta','qv']

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
