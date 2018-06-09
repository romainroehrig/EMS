import os

rep0 = os.getenv('REP_MUSC')

config = 'CMIP6'
cycle = 'arp631_macRR'
MASTER = rep0 + '/binaries/' + cycle + '/bin/MASTERODB'
PGD = rep0 + '/binaries/' + cycle + '/bin/PGD'
PREP = rep0 + '/binaries/' + cycle + '/bin/PREP'
namATMref = rep0 + '/namelist/ARPCLIMAT/nam.atm.tl127l91r.CMIP6.v631'
namSFXref = rep0 + '/namelist/SURFEX/nam.sfx.tl127.CMIP6.v631'
