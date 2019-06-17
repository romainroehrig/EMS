PROGRAM create_1D_LMDZ

  ! Intrinsic
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: sr => REAL32, dr => REAL64, &
                                           si => INT32, di => INT64

  ! Other module
  USE mod_alert, ONLY: check
  USE mod_sst_ostia, ONLY: get_sst_ostia
  USE jumble, ONLY: new_unit

  ! Libraries
  USE netcdf

  IMPLICIT NONE

  !---------------------------------------------------------------------
  !--------------------- DECLARATION -----------------------------------
  !---------------------------------------------------------------------
  ! kind
  INTEGER, PARAMETER                         :: kr = dr
  ! Files unit number
  INTEGER                                    :: unit_in, unit_out
  ! Constant
  REAL(KIND = kr), PARAMETER                 :: CtoK = 273.15_kr
  REAL(KIND = kr), PARAMETER                 :: htos = 3600._kr
  REAL(KIND = kr), PARAMETER                 :: gtokg = 1000._kr
  REAL(KIND = kr), PARAMETER                 :: hecto = 100._kr
  ! Dimensions
  INTEGER, PARAMETER                         :: NLAT = 1, NLON = 1
  INTEGER, PARAMETER                         :: NLEV = 40
  INTEGER, PARAMETER                         :: SHIFT = 8, ISHIFT = SHIFT + 1
  INTEGER, PARAMETER                         :: INTIME = 728
  INTEGER, PARAMETER                         :: ONTIME = INTIME + SHIFT
  INTEGER, PARAMETER                         :: N4D = 35, N3D = 4
  ! NC dim infos
  CHARACTER(LEN = *), PARAMETER              :: LON_NOUT = "lon"
  CHARACTER(LEN = *), PARAMETER              :: LAT_NOUT = "lat"
  CHARACTER(LEN = *), PARAMETER              :: LEV_NOUT = "lev"
  CHARACTER(LEN = *), PARAMETER              :: TIME_NOUT = "time"
  CHARACTER(LEN = *), PARAMETER              :: LON_NIN = LON_NOUT
  CHARACTER(LEN = *), PARAMETER              :: LAT_NIN = LAT_NOUT
  CHARACTER(LEN = *), PARAMETER              :: LEV_NIN = LEV_NOUT
  CHARACTER(LEN = *), PARAMETER              :: TIME_NIN = "time_offset"
  INTEGER                                    :: LAT_dimid, LON_dimid
  INTEGER                                    :: LEV_dimid, TIME_dimid
  INTEGER                                    :: LAT_varid, LON_varid
  INTEGER                                    :: LEV_varid, TIME_varid
  REAL(KIND = kr), DIMENSION(NLAT)           :: LAT_val
  REAL(KIND = kr), DIMENSION(NLON)           :: LON_val
  REAL(KIND = kr), DIMENSION(NLEV)           :: LEV_val
  REAL(KIND = kr), DIMENSION(ONTIME)         :: TIME_val
  REAL(KIND = kr)                            :: TSHIFT
  ! attribute
  CHARACTER (LEN = *), PARAMETER             :: LNAME = "long_name"
  CHARACTER (LEN = *), PARAMETER             :: LON_LN = "longitude"
  CHARACTER (LEN = *), PARAMETER             :: LAT_LN = "latitude"
  CHARACTER (LEN = *), PARAMETER             :: LEV_LN = "pressure levels"
  CHARACTER (LEN = *), PARAMETER             :: TIME_LN = "time"
  CHARACTER (LEN = *), PARAMETER             :: UNITS = "units"
  CHARACTER (LEN = *), PARAMETER             :: LON_UNITS = "degrees_west"
  CHARACTER (LEN = *), PARAMETER             :: LAT_UNITS = "degrees_north"
  CHARACTER (LEN = *), PARAMETER             :: LEV_UNITS = "Pa"
  CHARACTER (LEN = *), PARAMETER             :: TIME_UNITS = "seconds since" &
                                                // " 2011-10-01 00:00:00"
  ! _Fillvalue and missing_value
  CHARACTER (LEN = *), PARAMETER             :: FV = "_FillValue"
  CHARACTER (LEN = *), PARAMETER             :: MV = "missing_value"
  REAL(KIND = kr), PARAMETER                 :: FV_MV = -9999._kr
  ! dimids, start, count
  INTEGER, DIMENSION(4)                      :: dimids_4d
  INTEGER, DIMENSION(3)                      :: dimids_3d
  ! NC type read data
  TYPE NC_4D
    INTEGER                                  :: varid
    CHARACTER(LEN = 100)                     :: vname
    CHARACTER(LEN = 100)                     :: vlname
    CHARACTER(LEN = 100)                     :: vunits
    REAL(KIND = kr), DIMENSION(NLON, NLAT, NLEV, ONTIME) :: val
  END TYPE

  TYPE NC_3D
    INTEGER                                  :: varid
    CHARACTER(LEN = 100)                     :: vname
    CHARACTER(LEN = 100)                     :: vlname
    CHARACTER(LEN = 100)                     :: vunits
    REAL(KIND = kr), DIMENSION(NLON, NLAT, ONTIME)       :: val
  END TYPE

  ! Define netcdf variables
  TYPE(NC_4D), DIMENSION(N4D)                :: nc_out_4d
  TYPE(NC_3D), DIMENSION(N3D)                :: nc_out_3d

  ! Define logical for filled fields
  LOGICAL, DIMENSION(N4D)                    :: lfill_4d
  LOGICAL, DIMENSION(N3D)                    :: lfill_3d
  ! Define var_name
  CHARACTER(LEN = 100), DIMENSION(N4D)       :: vname_4d, vunits_4d
  CHARACTER(LEN = 100), DIMENSION(N3D)       :: vname_3d, vunits_3d
  CHARACTER(LEN = 100), DIMENSION(N4D)       :: vlname_4d
  CHARACTER(LEN = 100), DIMENSION(N3D)       :: vlname_3d
  ! Temporary variables
  INTEGER                                    :: temp_varid
  REAL(KIND = kr), DIMENSION(INTIME)         :: temp_1d
  REAL(KIND = kr), DIMENSION(NLEV, INTIME)   :: temp_2d

  ! File names
  CHARACTER(LEN = *), PARAMETER              :: FILE_IN = "rev180varanaecmwf" &
                                                // "anaradar50kmC1.c1." &
                                                // "20111001.000000.cdf"
  CHARACTER(LEN = *), PARAMETER              :: BASE_OUT = "cas_revelle_"
  CHARACTER(LEN = *), PARAMETER              :: EXT_OUT = ".nc"
  CHARACTER(LEN = *), PARAMETER              :: FILE_NML = "create_1D_LMDZ.nml"
  CHARACTER(LEN = 8)                         :: SST_NAME = "sst-gen_"
  CHARACTER(LEN = 11)                        :: PS_NAME = "psurf-varia"
  CHARACTER(LEN = 100)                       :: FILE_OUT

  ! Pressure constant
  LOGICAL                                    :: lsst_ost = .FALSE.
  LOGICAL                                    :: lpsurf = .FALSE.
  REAL(KIND = kr)                            :: psurf = 100900._kr

  ! Increment
  INTEGER                                    :: vinc, tinc

  NAMELIST /MAIN_NML/ lsst_ost, lpsurf, psurf
  !---------------------------------------------------------------------
  !--------------------- END DECLARATION -------------------------------
  !---------------------------------------------------------------------
  ! Load namelist
  CALL new_unit(unit_in)
  OPEN(UNIT = unit_in, FILE = FILE_NML, ACTION = 'READ', POSITION = 'REWIND')
  READ(UNIT = unit_in, NML = MAIN_NML)
  CLOSE(UNIT = unit_in)
  ! Define the output file name
  IF ( lsst_ost ) SST_NAME = "sst-ost_"
  IF ( lpsurf ) PS_NAME = "psurf-const"
  FILE_OUT = BASE_OUT // SST_NAME // PS_NAME // EXT_OUT 
  ! Netcdf variables name
  lfill_4d = [ .TRUE., .FALSE., .FALSE., .FALSE.,  .TRUE.,  .TRUE., &
              .FALSE., .FALSE., .FALSE.,  .TRUE.,  .TRUE., .FALSE., &
               .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE., &
              .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., &
               .TRUE.,  .TRUE.,  .TRUE., .FALSE., .FALSE., .FALSE., &
               .TRUE., .FALSE., .FALSE.,  .TRUE.,  .TRUE.]
  lfill_3d = [.FALSE., .FALSE., .FALSE.,  .TRUE.]

  vname_4d = ['zz   ', 'pp   ', 'temp ', 'qv   ', 'rh   ', 'theta', &
              'rv   ', 'u    ', 'v    ', 'ug   ', 'vg   ', 'w    ', &
              'advu ', 'hu   ', 'vu   ', 'advv ', 'hv   ', 'vv   ', &
              'advT ', 'hT   ', 'vT   ', 'advq ', 'hq   ', 'vq   ', &
              'advth', 'hth  ', 'vth  ', 'advr ', 'hr   ', 'vr   ', &
              'radT ', 'q1   ', 'q2   ', 'uw   ', 'vw   ']
  vname_3d = ['sens ', 'flat ', 'ts   ', 'ustar']

  vunits_4d = ['m      ', 'Pa     ', 'K      ', 'kg/kg  ', '%      ', 'K      ', &
               'kg/kg  ', 'm/s    ', 'm/s    ', 'm/s    ', 'm/s    ', 'Pa/s   ', &
               'm/s/s  ', 'm/s/s  ', 'm/s/s  ', 'm/s/s  ', 'm/s/s  ', 'm/s/s  ', &
               'K/s    ', 'K/s    ', 'K/s    ', 'kg/kg/s', 'kg/kg/s', 'kg/kg/s', &
               'K/s    ', 'K/s    ', 'K/s    ', 'kg/kg/s', 'kg/kg/s', 'kg/kg/s', &
               'K/s    ', 'K/s    ', 'K/s    ', 'm2/s2  ', 'm2/s2  ']
  vunits_3d = ['W/m2   ', 'W/m2   ', 'K      ', 'm/s    ']

  vlname_4d = ['Height                      ', 'Pressure                    ', &
               'Temperature                 ', 'Specific humidity (q)       ', &
               'Relative humidity           ', 'Potential temperature       ', &
               'Water vapor mixing ratio (r)', 'Horizontal wind U component ', &
               'Horizontal wind V component ', 'Horizontal U geostrophic    ', &
               'Horizontal V geostrophic    ', 'Vertical velocity           ', &
               'Total U advection           ', 'Horizontal U advection      ', &
               'Vertical U advection        ', 'Total V advection           ', &
               'Horizontal V advection      ', 'Vertical V advection        ', &
               'Total Temp. advection       ', 'Horizontal Temp. advection  ', &
               'Vertical Temp. advection    ', 'Total q advection           ', &
               'Horizontal q advection      ', 'Vertical q advection        ', &
               'Total theta advection       ', 'Horizontal theta advection  ', &
               'Vertical theta advection    ', 'Total r advection           ', &
               'Horizontal r advection      ', 'Vertical r advection        ', &
               '-                           ', 'Apparent heat sources       ', &
               'Apparent moisture sinks     ', 'Vertical flux U momentum    ', &
               'Vertical flux V momentum    ']
  vlname_3d = ['Surf. sensible heat flux, upward positive', &
               'Surf. latent heat flux, upward positive  ', &
               'Surf. skin temperature                   ', &
               'Shear velocity                           ']

  DO vinc = 1, N4D
    nc_out_4d(vinc)%vname = vname_4d(vinc)
    nc_out_4d(vinc)%vlname = vlname_4d(vinc)
    nc_out_4d(vinc)%vunits = vunits_4d(vinc)
    nc_out_4d(vinc)%val = FV_MV
  END DO

  DO vinc = 1, N3D
    nc_out_3d(vinc)%vname = vname_3d(vinc)
    nc_out_3d(vinc)%vlname = vlname_3d(vinc)
    nc_out_3d(vinc)%vunits = vunits_3d(vinc)
    nc_out_3d(vinc)%val = FV_MV
  END DO

  ! Create netcdf output file
  CALL check( nf90_create(TRIM(FILE_OUT), nf90_noclobber, unit_out) )
  ! Define the dimensions
  CALL check( nf90_def_dim(unit_out, LON_NOUT, NLON, LON_dimid) )
  CALL check( nf90_def_dim(unit_out, LAT_NOUT, NLAT, LAT_dimid) )
  CALL check( nf90_def_dim(unit_out, LEV_NOUT, NLEV, LEV_dimid) )
  CALL check( nf90_def_dim(unit_out, TIME_NOUT, ONTIME, TIME_dimid) )
  ! Define the coordinate variables
  CALL check( nf90_def_var(unit_out, LON_NOUT, NF90_DOUBLE, LON_dimid, LON_varid) )
  CALL check( nf90_def_var(unit_out, LAT_NOUT, NF90_DOUBLE, LAT_dimid, LAT_varid) )
  CALL check( nf90_def_var(unit_out, LEV_NOUT, NF90_DOUBLE, LEV_dimid, LEV_varid) )
  CALL check( nf90_def_var(unit_out, TIME_NOUT, NF90_DOUBLE, TIME_dimid, TIME_varid) )
  ! Assign units attribute
  CALL check( nf90_put_att(unit_out, LON_varid, UNITS, LON_units) )
  CALL check( nf90_put_att(unit_out, LAT_varid, UNITS, LAT_units) )
  CALL check( nf90_put_att(unit_out, LEV_varid, UNITS, LEV_units) )
  CALL check( nf90_put_att(unit_out, TIME_varid, UNITS, TIME_UNITS) )
  ! Assigne longname attribute
  CALL check( nf90_put_att(unit_out, LON_varid, LNAME, LON_LN) )
  CALL check( nf90_put_att(unit_out, LAT_varid, LNAME, LAT_LN) )
  CALL check( nf90_put_att(unit_out, LEV_varid, LNAME, LEV_LN) )
  CALL check( nf90_put_att(unit_out, TIME_varid, LNAME, TIME_LN) )
  ! Create dimids to pass the dimids of the dimensions
  dimids_4d = [LON_dimid, LAT_dimid, LEV_dimid, TIME_dimid]
  dimids_3d = [LON_dimid, LAT_dimid, TIME_dimid]
  ! Create the variables and assign their attributes
  DO vinc = 1, N4D
    CALL check( nf90_def_var(unit_out, TRIM(nc_out_4d(vinc)%vname), NF90_DOUBLE, &
         dimids_4d, nc_out_4d(vinc)%varid) )
    CALL check( nf90_put_att(unit_out, nc_out_4d(vinc)%varid, UNITS, &
         TRIM(nc_out_4d(vinc)%vunits)) )
    CALL check( nf90_put_att(unit_out, nc_out_4d(vinc)%varid, LNAME, &
         TRIM(nc_out_4d(vinc)%vlname)) )
    CALL check( nf90_put_att(unit_out, nc_out_4d(vinc)%varid, FV, FV_MV) )
    CALL check( nf90_put_att(unit_out, nc_out_4d(vinc)%varid, MV, FV_MV) )
  END DO
  DO vinc = 1, N3D
    CALL check( nf90_def_var(unit_out, TRIM(nc_out_3d(vinc)%vname), NF90_DOUBLE, &
         dimids_3d, nc_out_3d(vinc)%varid) )
    CALL check( nf90_put_att(unit_out, nc_out_3d(vinc)%varid, UNITS, &
         TRIM(nc_out_3d(vinc)%vunits)) )
    CALL check( nf90_put_att(unit_out, nc_out_3d(vinc)%varid, LNAME, &
         TRIM(nc_out_3d(vinc)%vlname)) )
    CALL check( nf90_put_att(unit_out, nc_out_3d(vinc)%varid, FV, FV_MV) )
    CALL check( nf90_put_att(unit_out, nc_out_3d(vinc)%varid, MV, FV_MV) )
  END DO
  ! End define mode
  CALL check( nf90_enddef(unit_out) )

  ! Open in file to get the variables:
  CALL check( nf90_open(FILE_IN, nf90_nowrite, unit_in) )
  CALL check( nf90_inq_varid(unit_in, LAT_NIN, LAT_dimid) )
  CALL check( nf90_inq_varid(unit_in, LON_NIN, LON_dimid) )
  CALL check( nf90_inq_varid(unit_in, LEV_NIN, LEV_dimid) )
  CALL check( nf90_inq_varid(unit_in, TIME_NIN, TIME_dimid) )

  CALL check( nf90_get_var(unit_in, LAT_dimid, LAT_val ) )
  CALL check( nf90_get_var(unit_in, LON_dimid, LON_val ) )
  CALL check( nf90_get_var(unit_in, LEV_dimid, LEV_val, [1], [NLEV]) )
  CALL check( nf90_get_var(unit_in, TIME_dimid, TIME_val(1:INTIME) ) )
  TSHIFT = TIME_val(2) - TIME_val(1)
  DO tinc = INTIME + 1, ONTIME
    TIME_val(tinc) = TIME_val(tinc - 1) + TSHIFT
  END DO

  ! Get and affect variables
  LEV_val = LEV_val * hecto ! hPa -> Pa
  ! pp
  IF ( lpsurf ) THEN
    nc_out_4d(2)%val(1,1,1,ISHIFT:ONTIME) = psurf
  ELSE
    CALL check( nf90_inq_varid(unit_in, 'p_srf_aver', temp_varid) )
    CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
    nc_out_4d(2)%val(1,1,1,ISHIFT:ONTIME) = temp_1d * hecto ! hPa -> Pa
  END IF
  nc_out_4d(2)%val(1,1,2:NLEV,ISHIFT:ONTIME) = SPREAD(LEV_val(2:NLEV), 2, INTIME)
  ! temp
  CALL check( nf90_inq_varid(unit_in, 'T', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  nc_out_4d(3)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  CALL check( nf90_inq_varid(unit_in, 'T_srf', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  nc_out_4d(3)%val(1,1,1,ISHIFT:ONTIME) = temp_1d + CtoK ! C -> K
  ! qv and rv
  CALL check( nf90_inq_varid(unit_in, 'q', temp_varid) ) 
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  nc_out_4d(7)%val(1,1,:,ISHIFT:ONTIME) = temp_2d / gtokg ! g/kg -> kg/kg 
  CALL check( nf90_inq_varid(unit_in, 'q_srf', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  nc_out_4d(7)%val(1,1,1,ISHIFT:ONTIME) = temp_1d
  nc_out_4d(4)%val = nc_out_4d(7)%val / ( 1._kr + nc_out_4d(7)%val )
  ! u
  CALL check( nf90_inq_varid(unit_in, 'u', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  nc_out_4d(8)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  CALL check( nf90_inq_varid(unit_in, 'u_srf', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  nc_out_4d(8)%val(1,1,1,ISHIFT:ONTIME) = temp_1d
  ! v
  CALL check( nf90_inq_varid(unit_in, 'v', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  nc_out_4d(9)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  CALL check( nf90_inq_varid(unit_in, 'v_srf', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  nc_out_4d(9)%val(1,1,1,ISHIFT:ONTIME) = temp_1d
  ! w
  CALL check( nf90_inq_varid(unit_in, 'omega', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  nc_out_4d(12)%val(1,1,:,ISHIFT:ONTIME) = temp_2d / htos * hecto ! mb/hr -> Pa/s
  CALL check( nf90_inq_varid(unit_in, 'omega_srf', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  nc_out_4d(12)%val(1,1,1,ISHIFT:ONTIME) = temp_1d / htos * hecto ! mb/hr -> Pa/s
  ! adv temp
  CALL check( nf90_inq_varid(unit_in, 'T_adv_h', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  temp_2d(1,:) = 0._kr ! there is no srf values we fix it to 0.
  temp_2d = - temp_2d / htos ! K/hr -> K/s
  nc_out_4d(19)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  nc_out_4d(20)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  CALL check( nf90_inq_varid(unit_in, 'T_adv_v', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  temp_2d(1,:) = 0._kr ! there is no srf values we fix it to 0.
  temp_2d = - temp_2d / htos ! K/hr -> K/s
  nc_out_4d(19)%val(1,1,:,ISHIFT:ONTIME) = nc_out_4d(19)%val(1,1,:,ISHIFT:ONTIME) + &
                                           temp_2d ! tot = h + v
  nc_out_4d(21)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  ! adv rv
  CALL check( nf90_inq_varid(unit_in, 'q_adv_h', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  temp_2d(1,:) = 0._kr ! there is no srf values we fix it to 0.
  temp_2d = - temp_2d / gtokg / htos ! g/kg/hr -> kg/kg/s
  nc_out_4d(28)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  nc_out_4d(29)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  CALL check( nf90_inq_varid(unit_in, 'q_adv_v', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  temp_2d(1,:) = 0._kr ! there is no srf values we fix it to 0.
  temp_2d = - temp_2d / gtokg / htos ! g/kg/hr -> kg/kg/s
  nc_out_4d(28)%val(1,1,:,ISHIFT:ONTIME) = nc_out_4d(28)%val(1,1,:,ISHIFT:ONTIME) + &
                                           temp_2d ! tot = h + v
  nc_out_4d(30)%val(1,1,:,ISHIFT:ONTIME) = temp_2d
  ! adv qv
  temp_2d = (1._kr + nc_out_4d(7)%val(1,1,:,ISHIFT:ONTIME))**2
  nc_out_4d(22)%val(1,1,:,ISHIFT:ONTIME) = nc_out_4d(28)%val(1,1,:,ISHIFT:ONTIME) / &
                                           temp_2d
  nc_out_4d(23)%val(1,1,:,ISHIFT:ONTIME) = nc_out_4d(29)%val(1,1,:,ISHIFT:ONTIME) / &
                                           temp_2d
  nc_out_4d(24)%val(1,1,:,ISHIFT:ONTIME) = nc_out_4d(30)%val(1,1,:,ISHIFT:ONTIME) / &
                                           temp_2d
  ! q1
  CALL check( nf90_inq_varid(unit_in, 'q1', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  nc_out_4d(32)%val(1,1,:,ISHIFT:ONTIME) = temp_2d / htos ! K/hr -> K/s
  ! q2
  CALL check( nf90_inq_varid(unit_in, 'q2', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_2d, [1, 1], [NLEV, INTIME]) )
  nc_out_4d(33)%val(1,1,:,ISHIFT:ONTIME) = temp_2d / htos ! K/hr -> K/s
  ! sens
  CALL check( nf90_inq_varid(unit_in, 'SH', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  nc_out_3d(1)%val(1,1,ISHIFT:ONTIME) = temp_1d
  ! flat
  CALL check( nf90_inq_varid(unit_in, 'LH', temp_varid) )
  CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  nc_out_3d(2)%val(1,1,ISHIFT:ONTIME) = temp_1d
  ! ts
  IF ( lsst_ost ) THEN
    CALL get_sst_ostia(temp_1d)
  ELSE
    CALL check( nf90_inq_varid(unit_in, 'T_skin', temp_varid) )
    CALL check( nf90_get_var(unit_in, temp_varid, temp_1d) )
  END IF
  nc_out_3d(3)%val(1,1,ISHIFT:ONTIME) = temp_1d + CtoK ! C -> K


  CALL check( nf90_close(unit_in) )

  ! Write the latitude and the longitude
  CALL check( nf90_put_var(unit_out, LON_varid, LON_val) )
  CALL check( nf90_put_var(unit_out, LAT_varid, LAT_val) )
  CALL check( nf90_put_var(unit_out, LEV_varid, LEV_val) )
  CALL check( nf90_put_var(unit_out, TIME_varid, TIME_val) )

  DO vinc = 1, N4D
    CALL check( nf90_put_var(unit_out, nc_out_4d(vinc)%varid, nc_out_4d(vinc)%val) )
  END DO
  DO vinc = 1, N3D
    CALL check( nf90_put_var(unit_out, nc_out_3d(vinc)%varid, nc_out_3d(vinc)%val) )
  END DO

  CALL check( nf90_close(unit_out) )

END PROGRAM create_1D_LMDZ
