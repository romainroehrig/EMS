import math

# Boltzman constant
RKBOL = 1.380658e-23
# Avogadro number
RNAVO = 6.0221367e23

# Perfect Gas constant
R = RNAVO*RKBOL
# Dry air molar mass
RMD = 28.9644
# Water vapor molar mass
RMV = 18.0153
# Dry air constant
RD = 1000.*R/RMD
# Water vapor constant
RV = 1000.*R/RMV
# Cpd
RCPD = 3.5*RD
# Cvd
RCVD = RCPD - RD
# Cpv
RCPV = 4.*RV
# Cvv
RCVV = RCPV - RV
# Rd/Cpd
RKAPPA = RD/RCPD
# Rv/Rd - 1
RETV = RV/RD - 1.

# Cw
RCW = 4218.
# Cs
RCS = 2106

# Fusion point temperature
RTT = 273.16
# RTT - Tx(ew-ei)
RDT = 11.82
# Lv condensation at RTT
RLVTT = 2.5008e6
# Ls fusion at RTT
RLSTT = 2.8345e6
# Lv at 0
RLVZER = RLVTT + RTT*(RCW-RCPV)
# Ls at 0
RLSZER = RLSTT + RTT*(RCS-RCPV)
RLMLT = RLSTT-RLVTT
RATM = 100000.

# es at RTT
RESTT = 611.14
RGAMW = (RCW-RCPV)/RV
RBETW = RLVTT/RV+RGAMW*RTT
RALPW = math.log(RESTT) + RBETW/RTT+RGAMW*math.log(RTT)
RGAMS = (RCS-RCPV)/RV
RBETS = RLSTT/RV+RGAMS*RTT
RALPS = math.log(RESTT) + RBETS/RTT + RGAMS*math.log(RTT)
RGAMD = RGAMS - RGAMW
RBETD = RBETS - RBETW
RALPD = RALPS - RALPW


def es(temp,ice=0):
  """
     Compute water vapor saturation pressure
     at temperature temp, against liquid water (ice = 0, default)
     or ice wate (ice = 1)
     input : temp in K
     output : es in Pa
  """

  tmp = RALPW + ice*RALPD - (RBETW+ice*RBETD) / temp - (RGAMW+ice*RGAMD) * math.log(temp)

  return math.exp(tmp)

def qv(rh,temp, pres,ice=0):
  """
     Compute specific humidity at temperature temp, 
     knowing relative humidity rh at pressure pres
     against liquid water (ice = 0, default) or ice wate (ice = 1)
     input : temp in K
             rh in %
             pres in Pa
     output : qv in kg/kg
  """

  esat = es(temp,ice=ice)
  e = esat*rh/100.
  w = e* RD / (RV*(pres - e))

  return w/(1.+w)
