#!/usr/bin/env python3
# -*- coding:UTF-8 -*-
# Copyright (c) Météo France (2014-)
# This software is governed by the CeCILL-C license under French law.
# http://www.cecill.info

#saveall = True
saveall = False

var2save = ['pf','zf','theta','temp','qv','hur','u','v','ql','qi','qr','qsn','rneb','cc','prw','rain','ustar','lhf','shf','tsurf','prw','qlc','qic','qrc','qsnc','tke']

var2save = var2save + ['Q1','Q2',\
                       'tntpbl','tntlscp','tntc','tntd','tntadv','tntnudg',\
                       'tnqvpbl','tnqvlscp','tnqvc','tnqvd','tnqvnudg',\
                       'tnupbl','tnuc','tnud','tnunudg','tendu',\
                       'tnvpbl','tnvc','tnvd','tnvnudg','tendv']

#var2save = var2save + ['ZT_RELAX','ZQ_RELAX','ZFQ_ADV','ZFT_ADV','O3']

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
