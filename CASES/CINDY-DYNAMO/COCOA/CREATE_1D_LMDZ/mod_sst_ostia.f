MODULE mod_sst_ostia

  ! Intrinsic
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: sr => REAL32, dr => REAL64, &
                                           si => INT32, di => INT64

  ! Other modules
  USE mod_time, ONLY: update_date
  USE mod_alert, ONLY: check

  ! Libraries
  USE netcdf

  IMPLICIT NONE

  INTEGER, PARAMETER                :: kr = dr
  INTEGER, PARAMETER                :: hshift = 3
  INTEGER, PARAMETER                :: NLAT = 3600, NLON = 7200, ND_SST = 3
  REAL(KIND = kr), PARAMETER        :: LAT_MIN = -0.8, LON_MIN = 79.7
  REAL(KIND = kr), PARAMETER        :: LAT_MAX = 0.8, LON_MAX = 81.3
  CHARACTER(LEN = *), PARAMETER     :: SST_path = '/bdd/OSTIA_SST_NRT/' &
                                       // 'SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/' &
                                       // 'METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2/'
  CHARACTER(LEN = *), PARAMETER     :: SST_ext = '120000-UKMO-L4_GHRSST-SSTfnd' &
                                       // '-OSTIA-GLOB-v02.0-fv02.0.nc'
  CHARACTER(LEN = *), PARAMETER     :: FMT1 = "(I4)"
  CHARACTER(LEN = *), PARAMETER     :: FMT2 = "(I2.2)"
  CHARACTER(LEN = *), PARAMETER     :: LON_NAME = "lon"
  CHARACTER(LEN = *), PARAMETER     :: LAT_NAME = "lat"
  CHARACTER(LEN = *), PARAMETER     :: TP_NAME = "analysed_sst"
  CHARACTER(LEN = *), PARAMETER     :: AO_NAME = "add_offset"
  CHARACTER(LEN = *), PARAMETER     :: SF_NAME = "scale_factor"

CONTAINS

  ELEMENTAL REAL(KIND = kr) FUNCTION UPACK(FIELD, add_offset, scale_factor)

    INTEGER, INTENT(IN)               :: FIELD
    REAL(KIND = kr), INTENT(IN)       :: add_offset, scale_factor

    UPACK = REAL(FIELD, KIND = kr) * scale_factor + add_offset

  END FUNCTION UPACK



  SUBROUTINE get_sst_ostia(temp_1d)

    REAL(KIND = kr), DIMENSION(:), INTENT(OUT) :: temp_1d
    INTEGER                                    :: unit_in
    INTEGER                                    :: iyear = 2011, imonth = 10
    INTEGER                                    :: iday = 2, ihour = 0
    INTEGER                                    :: tyear, tmonth, tday, thour
    ! year, month, day and forecast hour character 
    CHARACTER(LEN = 4)                         :: cyear
    CHARACTER(LEN = 2)                         :: cmonth, cday
    CHARACTER(LEN = 200)                       :: FILE_NAME
    INTEGER                                    :: LAMI, LAMA, NLA
    INTEGER                                    :: LOMI, LOMA, NLO
    INTEGER                                    :: LAT_dimid, LON_dimid, TP_varid
    INTEGER, DIMENSION(ND_SST)                 :: nc_start, nc_count
    REAL(KIND = kr), DIMENSION(NLAT)           :: LAT_val
    REAL(KIND = kr), DIMENSION(NLON)           :: LON_val
    REAL(KIND = kr)                            :: add_offset, scale_factor
    REAL(KIND = kr)                            :: old_val, new_val
    INTEGER, DIMENSION(:,:), ALLOCATABLE       :: temp_val
    INTEGER                                    :: iw, it

    WRITE(UNIT = cyear, FMT = FMT1) iyear
    WRITE(UNIT = cmonth, FMT = FMT2) imonth
    WRITE(UNIT = cday, FMT = FMT2) iday - 1

    FILE_NAME = SST_PATH // cyear // '/' // cmonth // '/' // cyear // cmonth &
                // cday // SST_ext

    CALL check( nf90_open(TRIM(FILE_NAME), nf90_nowrite, unit_in) )

    CALL check( nf90_inq_varid(unit_in, LAT_NAME, LAT_dimid) )
    CALL check( nf90_inq_varid(unit_in, LON_NAME, LON_dimid) )
    CALL check( nf90_inq_varid(unit_in, TP_NAME, TP_varid) )

    CALL check( nf90_get_att(unit_in, TP_varid, AO_NAME, add_offset) )
    CALL check( nf90_get_att(unit_in, TP_varid, SF_NAME, scale_factor) )

    CALL check( nf90_get_var(unit_in, LAT_dimid, LAT_val) )
    CALL check( nf90_get_var(unit_in, LON_dimid, LON_val) )

    ! Get the increment values and initialize the start and count vectors
    LAMI = MINLOC(ABS(LAT_val - LAT_MIN), DIM = 1)
    LAMA = MINLOC(ABS(LAT_val - LAT_MAX), DIM = 1)
    LOMI = MINLOC(ABS(LON_val - LON_MIN), DIM = 1)
    LOMA = MINLOC(ABS(LON_val - LON_MAX), DIM = 1)
    IF ( LAMI > LAMA ) THEN
      NLA = LAMI - LAMA + 1
      nc_start(2) = LAMA
    ELSE
      NLA = LAMA - LAMI + 1
      nc_start(2) = LAMI
    END IF
    IF ( LOMI > LOMA ) THEN
      NLO = LOMI - LOMA + 1
      nc_start(1) = LOMA
    ELSE
      NLO = LOMA - LOMI + 1
      nc_start(1) = LOMI
    END IF
    nc_start(3) = 1
    nc_count = [NLO, NLA, 1]
    ALLOCATE(temp_val(NLO, NLA))
    CALL check( nf90_get_var(unit_in, TP_varid, temp_val, start = nc_start, &
         count = nc_count) )
    CALL check( nf90_close(unit_in) )
    old_val = UPACK(SUM(temp_val), add_offset, scale_factor) / &
              REAL(SIZE(temp_val), KIND = kr)

    WRITE(UNIT = cyear, FMT = FMT1) iyear
    WRITE(UNIT = cmonth, FMT = FMT2) imonth
    WRITE(UNIT = cday, FMT = FMT2) iday

    FILE_NAME = SST_PATH // cyear // '/' // cmonth // '/' // cyear // cmonth &
                // cday // SST_ext

    CALL check( nf90_open(TRIM(FILE_NAME), nf90_nowrite, unit_in) )
    CALL check( nf90_get_var(unit_in, TP_varid, temp_val, start = nc_start, &
         count = nc_count) )
    CALL check( nf90_close(unit_in) )
    new_val = UPACK(SUM(temp_val), add_offset, scale_factor) / &
              REAL(SIZE(temp_val), KIND = kr)

    DO it = 1, SIZE(temp_1d)
    
      IF ( ihour == 12 ) THEN
        temp_1d(it) = new_val
        old_val = new_val

        tyear = iyear
        tmonth = imonth
        tday = iday
        thour = 0
        CALL update_date(tyear, tmonth, tday, thour, 24)

        WRITE(UNIT = cyear, FMT = FMT1) tyear
        WRITE(UNIT = cmonth, FMT = FMT2) tmonth
        WRITE(UNIT = cday, FMT = FMT2) tday

        FILE_NAME = SST_PATH // cyear // '/' // cmonth // '/' // cyear // cmonth &
                    // cday // SST_ext

        CALL check( nf90_open(TRIM(FILE_NAME), nf90_nowrite, unit_in) )
        CALL check( nf90_get_var(unit_in, TP_varid, temp_val, start = nc_start, &
             count = nc_count) )
        CALL check( nf90_close(unit_in) )
        new_val = UPACK(SUM(temp_val), add_offset, scale_factor) / &
                  REAL(SIZE(temp_val), KIND = kr)

      ELSE
        iw = MOD( ihour + 12, 24)
        temp_1d(it) = ( REAL(24 - iw, KIND = kr) * old_val + &
                      REAL(iw, KIND = kr) * new_val ) / REAL(24, KIND = kr)
      END IF

      CALL update_date(iyear, imonth, iday, ihour, hshift)

    END DO


  END SUBROUTINE get_sst_ostia

END MODULE mod_sst_ostia
