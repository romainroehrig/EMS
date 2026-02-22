#!/bin/bash

#config=config/config_arp632_CMIP6.py
config=config/config_arp632_CMIP6.300s.py

OPT=''

cases=( "ARMCU:REF"
        "AYOTTE:00SC 00WC 03SC 05SC 05WC 24SC"
        "IHOP:REF"
        "SCMS:REF"
        "RICO:SHORT"
        "BOMEX:REF"
        "MPACE:REF"
        "FIRE:REF"
        "SANDU:REF SLOW FAST"
        "AMMA:REF"
        "LBA:REF"
        "EUROCS:REF"
        "KB2006:MESONH"
        "DYNAMO:NSA3A_D1")

for tmp in "${cases[@]}"
do
    CC="${tmp%%:*}"
    subcases="${tmp##*:}"
    for SC in $subcases
    do
        echo "##################################################################"
        echo "################## $CC $SC"
        echo "MUSC.py -config $config -case $CC -subcase $SC $OPT"
        MUSC.py -config $config -case $CC -subcase $SC $OPT
    done
done
