===================================================================
THE FOLLOWING CONCERNS THE FILES IN "kit_ideal.tar" : 

FILES FOR THE EUROCS IDEALIZED DIURNAL CASE (2-DAY RUN)
ADAPTED FROM THE 27/28 JUNE 1997 DATA OF ARM/GCSS CASE3 SUBCASEA
===================================================================

-------------------------------------------
directory ./KIT_IDEAL/DATA/ : the data files 
--------------------------------------------

The directory ./KIT_IDEAL/DATA/ contains ascii data files written with 
a similar format as the one provided for GCSS Case3 except for a few 
differences recapitulated below:

1) time series are shorter : 2 days instead of 4 (1 day repeated twice)
                         number of dates = 8*2+1=17 instead of 8*4+1=33

2) the files "layer_*_with_prescribed_rad.dat" contain one additional field 
   as compared to  the file "layer_9707.dat" of ARM/GCSS case3 and as compared 
   to the files "layer_*.dat" also provided.
   This additional field is the prescribed radiative heating rate 
   interpolated on the same vertical grid as the other fields of the file.
     (this additional field is for those who don't want to use the 
     "qrad_eurocs_idea_*days_*levels.dat" file)

3) "qrad_eurocs_idea_*days_*levels.dat" are similar to 
   Total_radiative_heating_ECMWF_adjusted_to_Zhang_integrals_SubcaseA.dat
   except  for "qrad_eurocs_idea_*days_18levels.dat" the vertical grid 
   is the same as the one of the fields in "layer_*.dat"

   --------------------
   some specific points
   --------------------

1) the simulation begins at 1130 UTC on day 27 June 1997 , not 0000 UTC!!!
                            ----                           ---------------

2) "layer_idea_2days_advtot.dat" and "layer_idea_2days_advver.dat" 
are similar except the horizontal advection of s, T and q have been 
set to 0.
As a first step we decided to force with the vertical advection only, 
thought this is subject to dicussion. So, use  in priority the files
"layer_idea_2days_advver*.dat" rather than "layer_idea_2days_advtot*.dat"

3) in "surface_idea_2days.dat", some of the data have been set to 
"-999" as this  idealized case cannot be compared to site observations 
as straightforwardly.

"surface_idea_2days.dat" contains fields function of time only 
(e.g. surface latent and sensible heat flux...) necessary  
to run the simulation

4) "skin_t_idea_2days.dat" : skin temperature with a 30 mn sampling 
(necessary for radiative calculations)

   ------------------------------------------
   some details about the "layer*.dat" files
   ------------------------------------------

Some fields correspond to "composites", e.g., large scale advection, 
but not all of them, e.g. the temperature is not a composite. It 
corresponds to the ARM/GCSS Case3 values beginning the 27 June 1997 
0012 UTC

not composite: Temp_(K)
not composite: H2O_Mixing_Ratio_(g/kg)
    composite: u_wind_(m/s)
    composite: v_wind_(m/s)
    composite: omega_(mb/hour)
    composite: Wind_Div_(1/s)
    composite: Horizontal_Temp_Advec_(K/hour)
    composite: Vertical_T_Advec(K/hour)
    composite: Horizontal_q_Advec_(g/kg/hour)
    composite: Vertical_q_Advec(g/kg/hour)
not composite: s(Dry_Static_Energy)(K)
    composite: Horizontal_s_Advec_(K/hour)
    composite: Vertical_s_Advec(K/hour)
    composite: ds/dt(K/hour)
    composite: DT/dt(K/hour)
    composite: dq/dt_(g/kg/hour)
    composite: Q1_(k/hour)
    composite: Q2_(K/hour)
    composite: Qrad(K/hour)

   --------------------------------------
   details about "surface_idea_2days.dat"
   --------------------------------------

many fields are set to  '-999' as it does not 
really make sense to composite such things.

not composite        : Calday
not composite        : Year
not composite        : Month
not composite        : Day
not composite        : Hour
not composite        : Minute
not composite (-999) : Prec(mm/hour)
    composite        : LH (upward W/m2)
    composite        : SH (upward W/m2)
    composite        : Area Mean Ps(mb)
    composite        : Central Facility Ps(mb)
    composite        : Ts Air(C)
    composite        : Tg Soil(C)
    composite        : Sfc Air RH(%)
    composite        : Srf wind speed(m/s)
    composite        : u wind (m/s)
    composite        : v wind(m/s)
not composite (-999) : Srf Net Dn Rad(W/m2)
not composite (-999) : TOA LW Up(W/m2)
not composite (-999) : TOA SW Dn(W/m2)
not composite (-999) : TOA Ins(W/m2)
not composite (-999) : GOES Lowcld(%)
not composite (-999) : GOES Midcld(%)
not composite (-999) : GOES Hghcld(%)
not composite (-999) : GOES Totcld(%)
not composite (-999) : Cld Thickness(km)
not composite (-999) : Cld Top ht(km)
not composite (-999) : MWR Cld liquid(cm)
not composite (-999) : d(Column H2O)/dt (mm/hour)
not composite (-999) : Column H2O Advection (mm/hour)
not composite (-999) : Srf Evaporation (mm/hour)
not composite (-999) : d(Column Dry Static Energy)/dt (W/m2)
not composite (-999) : Column Dry Static Energy Advection (W/m2)
not composite (-999) : Column Radiative Heating (W/m2)
not composite (-999) : Column Latent heating (W/m2)

-------------------------------------------
directory ./KIT_IDEAL/PLOTS/ : some figures 
-------------------------------------------

The 2 poscript files surface_idea_2days.ps and layer_idea_2days_advtot.ps
illustrate what the files "qrad_eurocs_idea_*days_*levels.dat" 
and "surface_idea_2days.dat" contain respectively.

I have also added the PV-WAVE programs who generated the plots in case you 
are familiar with PV-WAVE or IDL (*.pro files).

F. Guichard, November 2001
