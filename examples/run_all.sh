#!/bin/bash

config=config/config_arp631_CMIP6.py
#config=config/config_arp631d_CMIP6.py
#config=config/config_arp631d_CMIP6.300s.py
#config=config/config_arp631d_CMIP6.300s.L137.py
#config=config/config_arp631d_CMIP6.L137.py
#config=config/config_arp641ps8d_PS8.py

OPT=''

MUSC.py -config $config -case ARMCU  -subcase REF      $OPT
MUSC.py -config $config -case AYOTTE -subcase 00SC     $OPT
MUSC.py -config $config -case AYOTTE -subcase 00WC     $OPT
MUSC.py -config $config -case AYOTTE -subcase 03SC     $OPT
MUSC.py -config $config -case AYOTTE -subcase 05SC     $OPT
MUSC.py -config $config -case AYOTTE -subcase 05WC     $OPT
MUSC.py -config $config -case AYOTTE -subcase 24SC     $OPT
MUSC.py -config $config -case IHOP   -subcase REF      $OPT
MUSC.py -config $config -case SCMS   -subcase REF      $OPT
MUSC.py -config $config -case RICO   -subcase SHORT    $OPT
MUSC.py -config $config -case BOMEX  -subcase REF      $OPT
MUSC.py -config $config -case MPACE  -subcase REF      $OPT
MUSC.py -config $config -case FIRE   -subcase REF      $OPT
MUSC.py -config $config -case SANDU  -subcase REF      $OPT
MUSC.py -config $config -case SANDU  -subcase SLOW     $OPT
MUSC.py -config $config -case SANDU  -subcase FAST     $OPT
MUSC.py -config $config -case AMMA   -subcase REF      $OPT
MUSC.py -config $config -case LBA    -subcase REF      $OPT
MUSC.py -config $config -case EUROCS -subcase REF      $OPT
MUSC.py -config $config -case KB2006 -subcase MESONH   $OPT
MUSC.py -config $config -case DYNAMO -subcase NSA3A_D1 $OPT
