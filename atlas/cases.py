import cdtime

cases = ['AYOTTE','IHOP','ARMCU','FIRE','BOMEX','RICO','AMMA','SANDU','ASTEX','CINDY-DYNAMO']
subcases = {}
for case in cases:
    subcases[case] = ['REF',] 

subcases['AYOTTE'] = ['00SC','00WC','03SC','05SC','05WC','24F','24SC']
subcases['SANDU'] =  ['REF','FAST','SLOW']
subcases['ASTEX'] = ['EUCLIPSE',]
subcases['CINDY-DYNAMO'] = ['NSA3aflux',]

tmin = {}
tmax = {}
for case in cases:
    tmin[case] = {}
    tmax[case] = {}

# AYOTTE
case = 'AYOTTE'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(2009,12,11,0)
  tmax[case][subcase] = cdtime.comptime(2009,12,11,6)

# IHOP
case = 'IHOP'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(2002,6,14,6)
  tmax[case][subcase] = cdtime.comptime(2009,6,14,12)


# BOMEX
case = 'BOMEX'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(1969,6,24,0)
  tmax[case][subcase] = cdtime.comptime(1969,6,24,14)

# RICO
case = 'RICO'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(2004,12,16,0)
  tmax[case][subcase] = cdtime.comptime(2004,12,17,0)

# ARMCU
case = 'ARMCU'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(1997,6,21,11)
  tmax[case][subcase] = cdtime.comptime(1997,6,22,3)

# FIRE
case = 'FIRE'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(1987,7,14,8)
  tmax[case][subcase] = cdtime.comptime(1987,7,15,8)

# SANDU
case = 'SANDU'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(2007,7,15,0)
  tmax[case][subcase] = cdtime.comptime(2007,7,18,0)

# ASTEX
case = 'ASTEX'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(1992,6,13,0)
  tmax[case][subcase] = cdtime.comptime(1992,6,14,16)

# CINDY-DYNAMO
case = 'CINDY-DYNAMO'
for subcase in subcases[case]:
  tmin[case][subcase] = cdtime.comptime(2011,10,1)
  tmax[case][subcase] = cdtime.comptime(2011,12,31)   
