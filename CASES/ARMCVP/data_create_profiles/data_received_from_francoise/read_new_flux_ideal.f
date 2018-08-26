c     parameter(NT=233,NVAR=35)  !JUL 97
c     parameter(NT=33,NVAR=35)  !JUL 97 subcaseA
      parameter(NT=97,NVAR=8)   !JUL 97 ideal case new surface heat fluxes
      dimension d(nvar,nt)
      character*50 var_name(nvar)
c      data var_name/
c     +     'Calday',          'Year',                  'Month',  
c     +     'Day',             'Hour',                  'Minute', 
c     +     'Prec(mm/hour)',   'LH_(upward_W/m2)',      'SH_(upward_W/m2)', 
c     +     'Area_Mean_Ps(mb)','Central_Facility_Ps(mb)', 'Ts_Air(C)', 
c     +     'Tg_Soil(C)',      'Sfc_Air_RH(%)',    'Srf_wind_speed(m/s)', 
c     +     'u_wind_(m/s)',    'v_wind(m/s)',      'Srf_Net_Dn_Rad(W/m2)', 
c     +     'TOA_LW_Up(W/m2)', 'TOA_SW_Dn(W/m2)',  'TOA_Ins(W/m2)',       
c     +     'GOES_Lowcld(%)',  'GOES_Midcld(%) ',   'GOES_Hghcld(%)',     
c     +     'GOES_Totcld(%)',  'Cld_Thickness(km)', 'Cld_Top_ht(km)',   
c     +     'MWR_Cld_liquid(cm)', 'd(Column_H2O)/dt_(mm/hour)',       
c     +     'Column_H2O_Advection_(mm/hour)','Srf_Evaporation_(mm/hour)',
c     +     'd(Column_Dry_Static_Energy)/dt_(W/m2)', 
c     +     'Column_Dry_Static_Energy_Advection_(W/m2)', 
c     +     'Column_Radiative_Heating_(W/m2)',        
c     +     'Column_Latent_heating_(W/m2)'/ 

c All relevant variables represent area-means in a domain enclosed by the
c following 12 grids centered around the ARM Central Facility.
c (These 12 grids are boundary facilities, profiler stations and analysis grids)
c longitude 
c   -97.2956     -98.3165     -99.0911     -99.3289     -99.2175     -98.5725
c   -97.5186     -96.5619     -95.8633     -95.5513     -95.6347     -96.3023
c latitude
c    38.3092      38.2207      37.6522      36.8885      36.0719      35.2734
c    34.9797      35.1246      35.6825      36.5223      37.3800      38.0408
c Central Facility
c     -97.4900     36.6100
 

c     open(unit=1,file='surface_9707.dat',status='old')
c     open(unit=1,file='surface_ca3a.dat',status='old')
      open(unit=1,file='new_flux_ideal.dat',status='old')
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)

      do i=1,nvar
       read(1,11)var_name(i)
       read(1,12)(d(i,j),j=1,nt)
       print *,'You have just read the ', i, ' th field d(nt) as :  '
       print *,'             ', var_name(i)
       print *,'             ', (d(i,j),j=1,nt)
      enddo
 11    format(a50)
 12    format(5e15.7)
      stop
      end 

    

