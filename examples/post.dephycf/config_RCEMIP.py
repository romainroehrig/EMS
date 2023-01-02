#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

var2save = ['pf','zf','theta','temp','qv','u','v','wap','ql','qi','qr','qsn','rneb','cc','prw','rain','lhf','lhfn','shf','tsurf','prw','hur','Q1','Q2','qlc','qic','qrc','qsnc','tke']

var2save = var2save + ['tntrlw','tntrsw','tntpbl','tntlscp','tntc','tntd','tnupbl','tnuc','tnud','tnvpbl','tnvc','tnvd','tntadv','tntnudg','tnugeo','tnvgeo','tendu','tendv','tendq','tendh']

var2save = var2save + ['ZTAU_ECUME','t2m','huss','uas','vas','O3','rO3','rCO2','rCH4','rN2O','rsdt','rsut','rlut','rsds','rsus','rsdscs','rsuscs','rlds','rlus','rldscs']

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
