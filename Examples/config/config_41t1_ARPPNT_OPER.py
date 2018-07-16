import os

rep0 = os.getenv('REP_MUSC')

config = 'ARPPNT_OPER'
cycle = '41t1_op1.11_MUSC'
MASTER = '/home/roehrig/pack/' + cycle + '/bin/MASTERODB'
PGD = '/home/roehrig/pack/' + cycle + '/bin/PGD'
PREP = '/home/roehrig/pack/' + cycle + '/bin/PREP'
namATMref = rep0 + '/main/namelist/ARPPNT/namarp_41t1_ARPEGE_OPER'
#namSFXref = rep0 + '/main/namelist/SURFEX/nam.sfx.tl127.CMIP6.v631'
