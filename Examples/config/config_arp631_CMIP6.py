import os

rep0 = os.getenv('REP_MUSC')

config = 'CMIP6'
cycle = 'arp631_macRR'
MASTER = '/home/common/pack/arp603_export.01.GFORTRAN610.cx/bin/MASTER'
PGD = '/home/common/pack/arp603_export.01.GFORTRAN610.cx/bin/PGD'
PREP = '/home/common/pack/arp603_export.01.GFORTRAN610.cx/bin/PREP'
namATMref = rep0 + '/namelist/ARPCLIMAT/nam.atm.tl127l91r.CMIP6.v631'
namSFXref = rep0 + '/namelist/SURFEX/nam.sfx.tl127.CMIP6.v631'
