****************************************
* CSU Array-Averaged Analysis Products *
* Version 3a                           *
****************************************

Release date: 30 September 2014
------------

Note:  this version supercedes V0, V1, and V2
----
   - Version 0 (6-hr) was based on a mixture of partially corrected hi-res
     sonde data and GTS-resolution uncorrected data.
   - Version 1a (6-hr) was based on fully corrected hi-res sonde data.
   - Versions 2a/2b (3-hr) were based on versions 2a/2b of the gridded dataset.
   - Versions 3a/3b (3-hr) are based on versions 3a/3b of the gridded dataset.
   - 21 Nov. 2014:  the dse, msep, msec files were updated due to a problem
     with the meridional advection term
   - 16 Jan. 2015:  in field files (both nsa and ssa), a few times contained
     very large RH at upper-levels so RH was reset to 150%. Also a few times 
     (11121512 and 11121706 contained bad sfc pressure and some missing data at
     100 hPa because of issues with interpolation scheme using sparse data. 
     These times were fixed in all nsa files. 
   - 29 Sep. 2015:  a few minor corrections to surface pressures in nsa files,
     and fields files (both nsa and ssa) recomputed using corrected gridded
     dataset (corrections primarily to vorticity values).


   *** Some minor errors were detected in versions 2a and 2b of the gridded
       dataset (bad surface temperature at 4 times and bad surface water vapor
       mixing ratio at 03Z, 09Z, 15Z, and 21Z at several times in version 2b).
       These latter errors were seen primarily over the Southern Sounding Array
       (SSA). All of these errors have been corrected in versions 3a and 3b of
       the gridded dataset.  Note that the change from V2 to V3 did not affect
       any of the budget results presented in Johnson et al. 2014, so depending
       on your analyses the corrections in V3 will likely have only a very
       minor impact on any V2 results that you have.

Averaging areas: 
---------------
   - Northern Sounding Array (NSA) bounded by core sites (Male, Colombo, Gan,
     and Revelle) 
   - Southern Sounding Array (SSA) bounded by core sites (Mirai, Gan, Diego, and
     Revelle) 
   - Figure "dynamo_map.png" shows averaging array and sonde network 

Time period:  1 October - 31 December 2011 
-----------
   *** Important caveat:  array-averaged budget analyses are most reliable when
       the ships (Mirai and Revelle) were on site.  If one wants to use the NSA
       analyses after 7 December (Revelle offsite) or the SSA analyses after
       30 November (Mirai offsite), we suggest use of the V3b analyses which
       supplements observation in data sparse regions with ECMWF operational
       analyses.

Resolution:  25-hPa in vertical, 3 hour in time
----------

Array-averaged analyses include: 
-------------------------------
   - basic fields (z, u, v, omega, T, theta, q, rh, div, and vort)
   - large-scale forcing fields (i.e., advective tendencies of T and q) 
   - Q1 and Q2 (i.e., apparent heat source and moisture sink)
   - moist static energy terms (based on primitive eqns. with no g*z in
     tendency or horizontal advection terms)
   - moist static energy terms (with g*z in tendency and horizontal advection
     terms)
   - dry static energy terms 
   - budget-derived quantities (rainfall and <Qr> which is column integrated
     net-radiation) along with surface latent and sensible heat WHOI fluxes at
     daily-resolution and 3-hourly CERES <Qr>
   - TRMM 3-hourly rainfall estimates (based on 3B42v7 product)

Details for version 3a products:
-------------------------------
   - The version 3a array-averaged products were computed using the version 3a
     gridded dataset which was constructed without using supplemental model
     analysis in data sparse regions.

   - The version 3a gridded dataset is based on the following data sources:

        - Hi-res (Level 4) sounding data were used at the following core sites
          (Male, Gan, Diego, Revelle, Mirai, Colombo) and 26 other Priority
          Sounding Sites (See Ciesielski et al. 2013).

        - Level 4 sounding data (uniform 5-hPa resolution with QC flags) are
          based on Level 3 (L3) sounding data (L3 is native resolution, humidity
          corrections applied as needed); see Ciesielski et al. (2013) for
          details of sounding dataset.

        - Hi-res P3 dropsonde data were used.

        - GTS-resolution (10-25hPa) sounding data were used at 54 other sites.

        - Soundings at Male' and Colombo were linearly time interpolated to
          3-hr resolution to match temporal resolution of sites in SSA.

        - Because of the large station spacing between sites over the Northern
          Sounding Array, the strong diurnal cycle and flow-blocking at
          low-levels at Colombo are aliased onto the large-scale analyses and
          adversely affect the large-scale budgets. This version of the analyses
          used adjusted Colombo soundings which mute these effects. To mitigate
          these local island effects on the large-scale budgets, a procedure was
          designed which used low-level ECMWF-analyzed fields in the vicinity
          of Sri Lanka to estimate open-ocean conditions at Colombo’s location
          as if the island were not present. These “unperturbed” ECMWF fields
          at low-levels were then merged with the observed Colombo soundings.
          Details can be found in Ciesielski et al. (2014).

        - Sounding data were supplemented with CIMSS cloud drift winds, ASCAT
          surface winds, and COSMIC thermodynamic profiles (COSMIC moisture data
          were not used below 850 hPa due to issues with these data at
          lower-levels).

   - Johnson and Ciesielski (2013) describes the V1 gridded dataset (6hr,
     soundings only) and some preliminary findings.

   - Johnson et al. (2014, submitted to JAS) describes the V2 gridded dataset
     and atmospheric budgets.

   - While analyses are provided from 1 Oct to 31 Dec, keep in mind the
     following:
        - Analyses are most reliable when ships are on-site.
        - Revelle was onsite (nominal position 80.5E,Eq.) from 4 Oct. - 29 Oct.,
          10 Nov. - 04 Dec., and 18 Dec. - 31 Dec.
        - Mirai was on site (nominal position 80.5E,8S) from 1 Oct. - 24 Oct.
          and 1 Nov. - 27 Nov. 
        - Analyses over NSA are less reliable after 5 Dec. when Colombo went
          from 4/day to 1/day sonde observations.
        - Male' ended sonde operation on 15 Dec. 2011 at 00Z so in effect
          there was no NSA after this date even though the Revelle returned on
          18 Dec.

   - See figure "invent_vis.png" which shows a visual inventory of soundings
     for 6 core Indian Ocean sites (Fig. 2 of Ciesielski et al. 2013).

   - If one wants to use the NSA analyses after 7 December (when Revelle went
     offsite) or the SSA analyses after 30 November, we suggest using the V3b
     analyses which supplements observations in data sparse regions with ECMWF
     operational analyses.

Additional comments:
-------------------
   - These data products were created by objectively analyzing upper-air
     soundings and other data sources onto a regular 1-degree, 25-hPa grid
     using multiquadric interpolation. The gridded data fields were then 
     averaged over the Northern Sounding Array (NSA) and the Southern Sounding
     Arrays (SSA).
   - As discussed in Johnson and Ciesielski (2013), the NSA analyses were
     strongly modulated by the MJO signal, while the SSA analyses were modulated
     more by ITCZ convection. 

References:
----------
   Ciesielski, P. E. and coauthors, 2014:
       Quality-controlled upper-air sounding dataset for DYNAMO/CINDY/AMIE:
       Development and corrections.  J. Atmos. Oceanic Technol., 31, 741-764.

   Ciesielski, P. E., R. H. Johnson, K. Yoneyama, and R. K. Taft, 2014:
       Mitigation of Sri Lanka island effects in Colombo sounding data and its
       impact on DYNAMO analyses.  To appear in J. Meteor. Soc. Japan.

   Johnson, R. H., and P. E. Ciesielski, 2013: 
       Structure and properties of Madden-Julian Oscillations deduced from
       DYNAMO sounding arrays.  J. Atmos. Sci., 70, 3157-3179.

   Johnson, R. H. and coauthors, 2014:
       Sounding-Based Thermodynamic Budgets for DYNAMO.  Submitted to J. Atmos.
       Sci.

Register to obtain future updates:
---------------------------------
   - To register to receive any future updates of these analyses, send an email
     to Paul Ciesielski (paulc@atmos.colostate.edu).

Problems or questions:
---------------------
   - Should be referred to Paul Ciesielski at: paulc@atmos.colostate.edu

********************************************************************************

File format for BASIC fields:  fields.nsa_{ver}
============================   fields.ssa_{ver}

   - For each 3 hour period there is one line with date/time information 
     followed by 40 lines with data as a function of pressure:

       - Line 1:  year(yy) month(mm) day(dd) hour(hh)
                  Fortran format:  (4(1x,i2))

       - Lines 2 - 41:  p(hPa) z(m) u(m/s) v(m/s) omega(mb/hr) T(C) theta(K) \
                          wmr(g/kg) rh(%) div(10^[-6] 1/s) vort(10^[-5] 1/s) \
                          num_pts_used
                        Fortran format:  (11(1x,f8.2),i5)

   - To convert omega (mb/hr) to w (mm/s) use the following approximate eqn:
       omega/(-g*rho)/3600 = w 

       - With:  g = 9.8 m/s^2
                rho = p/RT can be computed from fields provided
                      (for example near 1000 mb, rho  ~ 1.15 kg/m^3, 
                                   near  100 mb, rho  ~ 0.18 kg/m^3)

       - So that:  1 mb/hr near 1000 mb is ~2.5 mm/s, while
                   1 mb/hr near  100 mb is ~16 mm/s 


File format for LARGE-SCALE FORCING fields:  lsf.nsa_{ver}
==========================================   lsa.ssa_{ver}

   - Contain 736 periods of 3-hourly forcing data (01 Oct. - 31 Dec. 2011).

   - For each 3 hour period there is one line with date/time information 
     followed by 40 lines with data as a function of pressure:

       - Line 1:  year(yy) month(mm) day(dd) hour(hh)
                  Fortran format:  (4(1x,i2))

       - Lines 2 - 41:  p(hPa) hT(C/s) vT(C/s) hq(gr/(kg*s)) vq(gr/(kg*s))
                        Fortran format:  (f8.2,1p,4e11.3)

                        where:  hT - horizontal advection of T
                                vT -   vertical advection of T
                                hq - horizontal advection of q
                                vq -   vertical advection of q

   - These horizontal and vertical advection terms were computed using centered
     differences as follows:

        horizontal advection of "f":  h(f) = u*df/dx + v*df/dy
                              where:  dx     = a cos(phi)*d(lambda)
                                      dy     = a d(phi)
                                      phi    = latitude
                                      lambda = longitude

        vertical advection of "f":  v(f) = omega*df/dp

   - Note:  vt is only part of vertical temperature forcing (vtf)

        vtp = (p/po)**cappa * omega * d(theta)/dp = omega * (dT/dp - alpha/cp)

        where:   alpha is specific volume
                 alpha = RT/p can be computed from basic fields


File format for MOIST STATIC ENERGY (mse) TERMS (complete form):  msec.nsa_{ver}
===============================================================   msec.ssa_{ver}

   - Contain 736 periods of 3-hourly mse terms (01 Oct. - 31 Dec. 2011).

   - For each 3 hour period there is one line with date/time information 
     followed by 40 lines with data as a function of pressure:

       - Line 1:  year(yy) month(mm) day(dd) hour(hh)
                  Fortran format:  (4(1x,i2))

       - Lines 2 - 41:  p(hPa) th(m^2/s^3) uh(m^2/s^3) vh(m^2/s^3) wh(m^2/s^3) \
                          h(m^2/s^2)
                        Fortran format:  (f8.2,1p,5e11.3)

                    where:  th - local time tendency of h (moist static energy)
                            uh - zonal advection of h
                            vh - meridional advection of h
                            wh - vertical advection of h
                             h - moist static energy  (cp*T + Lv*q + g*z) 

                            cp = 1004 m^2/(s^2*K)
                            Lv = 2.5e6 m^2/s^2
                            g  = 9.81 m/s^2

   - Note:  g*z is used in computing all terms for moist static energy equation 

   - These horizontal and vertical advection terms were computed using centered
     differences.


File format for MOIST STATIC ENERGY TERMS (primitive eqn. form):  msep.nsa_{ver}
===============================================================   msep.ssa_{ver}

   - Contain 736 periods of 3-hourly mse terms (01 Oct. - 31 Dec. 2011).

   - For each 3 hour period there is one line with date/time information 
     followed by 40 lines with data as a function of pressure:

       - Line 1:  year(yy) month(mm) day(dd) hour(hh)
                  Fortran format:  (4(1x,i2))

       - Lines 2 - 41:  p(hPa) th(m^2/s^3) uh(m^2/s^3) vh(m^2/s^3) wh(m^2/s^3) \
                          h(m^2/s^2)
                        Fortran format:  (f8.2,1p,5e11.3)

                    where:  th - local time tendency of (cp*T+Lv*q) 
                            uh - zonal advection of (cp*T+Lv*q)
                            vh - meridional advection of (cp*T+Lv*q)
                            wh - vertical advection of h (moist static energy)
                             h - moist static energy  (cp*T + Lv*q + g*z) 

                            cp = 1004 m^2/(s^2*K)
                            Lv = 2.5e6 m^2/s^2
                            g  = 9.81 m/s^2

   - Note:  moist static energy equation is based on primitive equations where 
            g*z is used only for computing the vertical advection term 
            (for details see Neelin 2007;
             http://www.atmos.ucla.edu/~csi/REF/pdfs/gencircrev.pdf) 

   - These horizontal and vertical advection terms were computed using centered
     differences.


File format for DRY STATIC ENERGY (dse) TERMS:  dse.nsa_{ver}
=============================================   dse.ssa_{ver}

   - Contain 736 periods of 3-hourly dse terms (01 Oct. - 31 Dec. 2011).

   - For each 3 hour period there is one line with date/time information 
     followed by 40 lines with data as a function of pressure:

       - Line 1:  year(yy) month(mm) day(dd) hour(hh)
                  Fortran format:  (4(1x,i2))

       - Lines 2 - 41:  p(hPa) ts(m^2/s^3) us(m^2/s^3) vs(m^2/s^3) ws(m^2/s^3) \
                          s(m^2/s^2)
                        Fortran format:  (f8.2,1p,5e11.3)

                    where:  ts - local time tendency of s
                            us - zonal advection of s
                            vs - meridional advection of s
                            ws - vertical advection of s
                             s - dry static energy  (cp*T + g*z) 

                            cp = 1004 m^2/(s^2*K)
                            g  = 9.81 m/s^2

   - These horizontal and vertical advection terms were computed using centered
     differences.


File format for ARRAY-AVERAGED Q1 and Q2:  q1q2.nsa_{ver}
========================================   q1q2.ssa_{ver}

   - Contain 736 periods of 3-hourly data (01 Oct. - 31 Dec. 2011).

   - For each 3 hour period there is one line with date/time information 
     followed by 40 lines with data as a function of pressure:

       - Line 1:  year(yy) month(mm) day(dd) hour(hh)
                  Fortran format:  (4(1x,i2))

       - Lines 2 - 41:  p(hPa) Q1(K/day) Q2(K/day)
                        Fortran format:  (3f8.2)

                            cp = 1004 m^2/(s^2*K)
                            g  = 9.81 m/s^2

       - The apparent heat source, Q1, and moisture sink, Q2 (Yanai et al. 1973)
         were computed as:

             Q1/cp = [dT/dt + h(T) + (p/po)**kappa * omega * d(theta)/dp]
             Q2/cp = -Lv/cp * [dq/dt + h(q) + v(q)]

             where:  dt = 12 hours   po = 1000 mb   kappa = .286
                     cp=1004         Lv = 2.5e6         g = 9.81


File format for BUDGET-DERIVED QUANTITIES:  eopo.nsa_{ver}
=========================================   eopo.ssa_{ver}

   - Contain 736 lines of 3-hourly data (01 Oct. - 31 Dec. 2011).

   - Each data line:
        year(yy) month(mm) day(dd) hour(hh) eo(mm/day) po2(mm/day) sh(mm/day) \
          po1(mm/day) qr(mm/day) qrnet_ceres(mm/day)
        Fortran format:  (4i3,6f8.2)

   - Where:
        eo  - LH flux from WHOI TropFlux product averaged over sounding array
        po2 - Q2-budget derived rainfall computed using eo estimate
        sh  - SH flux from WHOI TropFlux product averaged sounding array
        po1 - Q1-budget derived rainfall computed using sh and qrnet_ceres
                estimates
        qr  - column net radiation computed from combine Q1/Q2-budget residual 
                using LH and SH from WHOI OAFLUX
        qrnet_ceres - column net radiation from CERES

   - Note:  100 W/m^2 = 3.45 mm/day = 0.93 K/day


File format for TRMM RAINFALL OVER VARIOUS REGIONS:  trmm_rain
==================================================

   - Contains 736 lines of 3-hourly trmm 3b42V7 area averaged rainfall data
     in mm/day (01 Oct. - 31 Dec. 2011) plus one final data line giving the
     temporal mean for each area.

   - Each data line:
        yymmddhh nsa ssa colombo male gan revelle diego mirai manus \
          (np(i),i=1,9)
        Fortran format:  (4i2,9f9.4,9i4)

   - Where:  nsa     - over northern enhanced sounding array
             ssa     - over southern enhanced sounding array
             colombo - area with 1 degree radius centered on Colombo
             male    - area with 1 degree radius centered on Male
             gan     - area with 1 degree radius centered on Gan
             revelle - area with 1 degree radius centered on Revelle
                         (at nominal position:  0.1N, 80.5E)
             diego   - area with 1 degree radius centered on Diego
             mirai   - area with 1 degree radius centered on Mirai
                         (at nominal position:  8.0S, 80.5E)
             manus   - area with 1 degree radius centered on Manus

             np(9)   - number of 0.25 degree points in each averaging area
