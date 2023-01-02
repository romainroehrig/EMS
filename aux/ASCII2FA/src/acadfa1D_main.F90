      PROGRAM ACADFA1D_MAIN

!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!
!     PURPOSE.
!     --------
!        Prepare academic file for 1D arpege/aladin/alaro/arome/hirlam.
!
!     INTERFACE.
!     ----------
!        PROGRAM ACADFA1D_MAIN
!
!     METHOD.
!     -------
!        The output is an aladin file containing data as unpacked fields.
!
!     EXTERNALS.
!     ----------
!        FA package
!        ACADFA_SUEFRAME
!
!     REFERENCE.
!     ----------
!        Meteo-France CNRM/GMAP
!
!     AUTHOR.
!     -------
!        Sylvie Malardel from a 2D version by Pierre Benard and Co
!
!     MODIFICATIONS.
!     --------------
!        original: 10-08-1998
!        modified: 07-09-2001 by J. Masek - some cleaning
!        1D version : 12/2005
!
!     ------------------------------------------------------------------
!
!     Parameters for 1D:
!     --------------------------------------------
!
!     IDGUX     = number of latitudes (C+I)
!     IDGL      = number of latitudes (C+I+E)
!     IFLEV     = number of levels
!     INSMAX    = meridional truncation (3*INSMAX < IDGL)
!     ZDELY     = meridional grid spacing
!     ZDELZ     = vertical spacing of half levels
!     LSPRT     = .F./.T.: Q_v in spectral / grid point
!     LALAPHYS    = .T./.F.: prepare file to run with Arpege/Aladin physics
!     LREASUR   = .T./.F.: prepare file to run with ISBA
!
!     ZP00      = surface reference pressure
!
!     Characteristics of the aladin geometry:
!     ---------------------------------------
!
!     IROTEQ       : rotation parameter
!     ZLONR, ZLATR : longitude, latitude of reference point of rotation
!     ZBETA        : angle of rotation
!     ISOTRP       : isotropy parameter
!     ZLON1, ZLAT1 : longitude, latitude of South-West corner
!     ZLON2, ZLAT2 : longitude, latitude of North-East corner
!     ZRPK         : projection parameter
!     IGIV0        : reference point for projection parameter
!     ZLON0, ZLAT0 : longitude, latitude of reference point of projection
!     IDGUX        : number of latitudes for the domain (C+I)
!     IDGL         : number of latitudes for the domain (C+I+E)
!     IBZONG       : number of latitudes for relaxation zone
!     ZDELX, ZDELY : grid spacing
!
!     INART        : number of articles for FA file
!     ZVALH, ZVBH  : vertical functions A, B
!     LMAP         : should be .FALSE. for academic experiments
!     ZWXN, ZWYN   : wave numbers in x, y
!
!     Additional variables for the file auto-documentation:
!     -----------------------------------------------------
!
!     IDOM         : written domain parameter
!     ISUBTR       : unpacked subtruncation
!     CLMCA        : frame name
!
!     Miscellaneous tunable variables:
!     --------------------------------
!
!     CLFILEOUT    : output file name
!
!     Logical unit numbers:
!     ---------------------
!
!     IULNAM       : namelist unit number
!     IULOUT       : output file unit number
!
!     ------------------------------------------------------------------

      IMPLICIT NONE

      ! constants

      REAL, PARAMETER :: ZPI  = 3.14159265358979
      REAL, PARAMETER :: ZRTD = 180./ZPI  ! rad to deg conversion
      REAL, PARAMETER :: ZG = 9.81

      ! variables

      INTEGER IBZONG, IFLEV, INSMAX
      INTEGER IDGUX, IDGL
      INTEGER IULNAM, IULOUT
      INTEGER IROTEQ, ISOTRP, IGIV0
      INTEGER IREP, IUFD, IBARI, INIMES, IBARP
      INTEGER IFACTM
      INTEGER INGRIB, INBPDG, INBCSP, ISTRON
      INTEGER IPUILA, IDMOPL, INGRIG, IDOM, ISUBTR, INART
      INTEGER IDATE(11), IFAX(20)
      INTEGER I, J, JLEV, JFORC
      INTEGER NSEFRE
      INTEGER NFORC, NFORCS
      INTEGER IYEAR,IMONTH,IDAY,IHH,IMIN

      REAL ZDELX, ZDELY, ZLX, ZLY
      REAL ZP00, ZT, ZU, ZV, ZQ, ZZ
      REAL ZRPK, ZWXN, ZWYN
      REAL ZBETA, ZLON0, ZLAT0, ZLON1, ZLAT1, ZLON2, ZLAT2, ZLONR, ZLATR
      REAL ZOROG, ZSPT0, ZTS0, ZQS0, ZZ0G, ZALB0, ZEMIS0

      LOGICAL LMAP, LNHDYN, LSPRT
      LOGICAL LALAPHYS, LREASUR,lplussurf
      LOGICAL LQVSP, LQVGRP, LQCGRP, LQIGRP, LQRGRP, LQSGRP, LQGGRP
      LOGICAL LCFGRP, LSRCGRP, LTKEGRP

      LOGICAL LLNOMM, LLERFA, LLIMST
      LOGICAL LLCOSP, LLOUT, LLGARD


      CHARACTER CLMCA*16, CLFILEOUT*40
      CHARACTER CBLOCK*12, YRECFM*8
      CHARACTER CHAMP*16
      CHARACTER FORCAGE*32

      ! allocatable arrays

      INTEGER, ALLOCATABLE :: NESN0 (:), NCPL4N(:)
      INTEGER, ALLOCATABLE :: IKNTMP(:), IKMTMP(:)

      REAL, ALLOCATABLE :: ZVALH(:),  ZVBH(:)
      REAL, ALLOCATABLE :: ZOUT (:), ZWORK(:)
      REAL, ALLOCATABLE :: ZFFT (:), TRIGS(:)

      NAMELIST/NAM1D/IFLEV,ZDELY,LALAPHYS,LREASUR,LMAP,LNHDYN, LSPRT, &
     &                LQVSP, LQVGRP, LQCGRP, LQIGRP, LQRGRP, LQSGRP, LQGGRP, &
     &                LCFGRP, LSRCGRP, LTKEGRP, &
     &                NFORC,NFORCS,IYEAR,IMONTH,IDAY,IHH,IMIN,&
     &               ZLON0, ZLAT0, ZLON1, ZLAT1, ZLON2, ZLAT2, ZLONR, ZLATR

      ! ----------------------------
      ! 1. Set up internal constants
      ! ----------------------------

      ! logical unit numbers:
      IULNAM = 10
      IULOUT = 11
      LLGARD = .FALSE.
      !
      ! not tunable aladin variables => compulsary values:
      CLMCA  = 'CADRE.STANDARD.E'
      IROTEQ = 0
      ISOTRP = 1
      IGIV0  = 0
      ZRPK   = 1.
      ZWXN   = 0.
      ZWYN   = 0.

      ! --------------------
      ! 2. Read the namelist
      ! --------------------

      ! 2.1 Set default values for 1D

      IDGUX     = 4
      IDGL      = 4
      IBZONG    = 0
      INSMAX    = 1
      IFLEV     = 200
      ZDELY     = 250000.
      CLFILEOUT = '1D.file'
      LMAP      = .FALSE.
      LNHDYN    = .FALSE.
      LSPRT     = .TRUE.
      LALAPHYS  = .FALSE.
      LREASUR   = .FALSE.
      ZP00      = 101325.
      LQVSP     = .FALSE.
      LQVGRP    = .TRUE.
      LQCGRP    = .FALSE.
      LQIGRP    = .FALSE.
      LQRGRP    = .FALSE.
      LQSGRP    = .FALSE.
      LQGGRP    = .FALSE.
      LCFGRP    = .FALSE.
      LSRCGRP   = .FALSE.
      LTKEGRP   = .FALSE.
      NFORC     = 0
      NFORCS=0
      IYEAR = 2007
      IMONTH=1
      IDAY=31
      IHH= 0
      IMIN=0
      ZLON0 = 0.
      ZLAT0 = 44.
      ZLON1 = 0.
      ZLAT1 = 43.
      ZLON2 = 0.
      ZLAT2 = 45.
      ZLONR = 0.
      ZLATR = 45.
      ! 2.2 Read namelist
      !
      OPEN(IULNAM,FILE='nam1D',STATUS='OLD')
      READ(IULNAM,NAM1D)
      ZLON0 = ZLON0 /ZRTD
      ZLAT0 = ZLAT0 /ZRTD
      ZLON1 = ZLON1 /ZRTD
      ZLAT1 = ZLAT1 /ZRTD
      ZLON2 = ZLON2 /ZRTD
      ZLAT2 = ZLAT2 /ZRTD
      ZLONR = ZLONR /ZRTD
      ZLATR = ZLATR /ZRTD


      ! 2.3 Compute additional parameters

      ! isotropic grid
      ZDELX = ZDELY

      ! domain size
      ZLX = ZDELX
      ZLY = ZDELY*IDGL

      ! 2.4 FFT trigs

      ALLOCATE(TRIGS(IDGL))
      CALL SET99(TRIGS,IFAX,IDGL)

      ! 2.5 Set up namelist-deductible variables

      IDOM   = 1
      ISUBTR = 0
      INART  = 6*IFLEV+1

      ! ---------------------------------------
      ! 3. prepare the frame of output FA file
      ! ---------------------------------------

      READ(10,*) CBLOCK

      IF (CBLOCK.NE.'ETA') THEN
        print*,'problem : the block ETA (A and B functions) is not present'
        STOP
      ENDIF

      ! 3.2 Read vertical coefficients A,B

      ALLOCATE(ZVALH(0:IFLEV))
      ALLOCATE(ZVBH (0:IFLEV))
      READ(10,*)CBLOCK
      DO JLEV = 0,IFLEV
        READ(10,*)ZVALH(JLEV)
        ZVALH(JLEV) = ZVALH(JLEV) / ZP00
        WRITE(6,*)'JLEV ZVALH=',JLEV,ZVALH(JLEV)
      ENDDO
      READ(10,*)CBLOCK
      DO JLEV = 0,IFLEV
        READ(10,*)ZVBH (JLEV)
        WRITE(6,*)'JLEV ZVBH=',JLEV,ZVBH(JLEV)
      ENDDO

      ! 3.3 Computing meridional limit wavenumbers along zonal wavenumbers

      ALLOCATE(IKNTMP(0:0))
      IKNTMP(0) = INSMAX

      ! 3.4 Computing zonal limit wavenumbers along meridional wavenumbers

      ALLOCATE(IKMTMP(0:INSMAX))
      IKMTMP(0:INSMAX) = 0

      ! 3.5 Computing resulting indexation

      ALLOCATE(NCPL4N(0:INSMAX))
      ALLOCATE(NESN0 (0:INSMAX))

      NCPL4N(0:INSMAX) = 4

      NESN0(0) = 1
      DO J = 1,INSMAX
        NESN0(J) = NESN0(J-1)+NCPL4N(J-1)
      ENDDO

      NSEFRE = 4*(INSMAX+1)

      ZBETA = 0. /ZRTD

      ! ----------------------
      ! 4. Prepare output file
      ! ----------------------
      print*,'ZP00=',ZP00
      ! 4.1 Define the frame for fa file

      CALL ACADFA_SUEFRAME(CLMCA,ZBETA,LMAP,0,INSMAX,IDGL,1,     &
     &               LLGARD,IROTEQ,ZLONR,ZLATR,ZLON1,ZLAT1,      &
     &               ZLON2,ZLAT2,ZLON0,ZLAT0,ZRPK,ISOTRP,        &
     &               IGIV0,ZLX,ZLY,ZDELX,ZDELY,ZWXN,ZWYN,        &
     &               1,1,1,IDGUX,0,IBZONG,IFLEV,                 &
     &               ZP00,ZVALH,ZVBH,IDOM,ISUBTR)


      ! 4.2 Open the fa file

      IUFD   = IULOUT
      LLNOMM = .TRUE.
      LLERFA = .TRUE.
      LLIMST = .FALSE.
      INIMES = 1
      IBARP  = INART

!      IFACTM = 1
!      CALL LFIFMD(IFACTM)

      IBARI = 0

      CALL FAITOU(IREP,IUFD,LLNOMM,CLFILEOUT,'NEW', &
     &            LLERFA,LLIMST,INIMES,IBARP,IBARI,CLMCA)

      IF (IREP.NE.0) STOP


      ! 4.4 Impose no GRIB encoding (no packing)

      INGRIG = 0
      CALL FAVEUR(IREP,IUFD,INGRIB,INBPDG,INBCSP, &
     &  ISTRON,IPUILA,IDMOPL)
      IF (IREP.NE.0) STOP
        print*,'IREP=',IREP
        print*,'IUFD=',IUFD
        print*,'INGRIB=',INGRIB
        print*,'INBPDG=',INBPDG
        print*,'INBCSP=',INBCSP
        print*,'ISTRON=',ISTRON
        print*,'IPUILA=',IPUILA
        print*,'IDMOPL=',IDMOPL
      CALL FAGOTE(IREP,IUFD,INGRIG,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)
      IF (IREP.NE.0) STOP

      ! 4.3 Initialize the date

      IDATE(1)  = IYEAR
      IDATE(2)  = IMONTH
      IDATE(3)  = IDAY
      IDATE(4)  = IHH
      IDATE(5)  = IMIN
      IDATE(6)  = 1
      IDATE(7)  = 0
      IDATE(8)  = 0
      IDATE(9)  = 0
      IDATE(10) = 0
      IDATE(11) = 0

      CALL FANDAR(IREP,IUFD,IDATE)
      IF (IREP.NE.0) STOP

      ! ----------------------------
      ! 5. Write data for ATMOSPHERE
      ! ----------------------------

      READ(10,*) CBLOCK

      IF (CBLOCK.NE.'ATMOSPHERE') THEN
        print*,'problem : the block ATMOSPHERE is not present'
        STOP
      ENDIF

      !  Preparation for GP fields
      !
      LLCOSP = .FALSE.
      ALLOCATE(ZOUT (IDGL))
      ZOUT(:) = 0.


      ! 5.1 Grid point surface geopotential
      !
      ! orography / surface geopotential
      !
      READ(10,*)
      READ(10,*)ZOROG
      DO J = 1,IDGL
        ZOUT (J) = ZOROG*ZG
      ENDDO

      CALL FAIENC(IREP,IUFD,'SURF',1,'GEOPOTENTIEL',ZOUT,LLCOSP)
      IF (IREP.NE.0) STOP

      ! write orography
      WRITE(6,'(A)') '--------------------'
      WRITE(6,'(A)') 'GRIDPOINT OROGRAPHY:'
      WRITE(6,'(A)') '--------------------'
      DO J = 1,IDGL
        WRITE(6,'(I3,1X,3F10.2)') J,ZOUT(J)
      ENDDO
      !
      DEALLOCATE (ZOUT)

      ! Preparation for spectral fields

      ALLOCATE(ZOUT (NSEFRE))
      ALLOCATE(ZFFT (IDGL)  )

      LLCOSP =.TRUE.

      !
      ! 5.2 Spectral Surface Geopotential
      !

      DO J = 1,IDGL
        ZFFT(J) = ZOROG*ZG
      ENDDO

      call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)

      CALL FAIENC (IREP,IUFD,'SPECSURF',1,'GEOPOTEN',ZOUT,LLCOSP)

      ! 5.3 Spectral Surface Pressure

      READ(10,*)
      READ(10,*)ZSPT0
      DO J = 1,IDGL
        ZFFT(J) = ZSPT0
      ENDDO

      call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)
      !WRITE(6,*)'Ps=',ZFFT(1)
      !WRITE(6,*)'ZOUT=',ZOUT
      CALL FAIENC(IREP,IUFD,'SURF',1,'PRESSION',ZOUT,LLCOSP)

      !
      ! 5.4 Spectral U-Wind
      !
      READ(10,*)
!
      DO JLEV =1, IFLEV
        READ(10,*) ZU
        DO J = 1,IDGL
          ZFFT(J) = ZU
        ENDDO
        WRITE (6,*)'JLEV UWIND',JLEV,ZFFT(1)

        call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)

        CALL FAIENC (IREP,IUFD,'S',JLEV,'WIND.U.PHYS',ZOUT,LLCOSP)
      ENDDO

      !
      ! 5.5 Spectral V-Wind
      !
      READ(10,*)
!
      DO JLEV =1, IFLEV
        READ(10,*) ZV
        DO J = 1,IDGL
          ZFFT(J)=ZV
        ENDDO
        WRITE (6,*)'JLEV VWIND',JLEV,ZFFT(1)

        call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)

        CALL FAIENC (IREP,IUFD,'S',JLEV,'WIND.V.PHYS',ZOUT,LLCOSP)
      ENDDO

      !
      ! 5.6 Spectral Temperature
      !
      READ(10,*)
      !
      DO JLEV =1, IFLEV
        READ(10,*)ZT
        DO J = 1,IDGL
          ZFFT(J)=ZT
        ENDDO
        WRITE(6,*)'JLEV TEMPE',JLEV,ZFFT(1)
        !
        call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)
        !
        CALL FAIENC (IREP,IUFD,'S',JLEV,'TEMPERATURE',ZOUT,LLCOSP)
      ENDDO


        ! 5.7 NH fields if LNHDYN

      IF (LNHDYN) THEN
      !
        DO JLEV =1, IFLEV
          !
          ! Pressure Departure
          !
          DO J = 1,IDGL
            ZFFT(J) = 0.0
          ENDDO
          !
          call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)
          !
          CALL FAIENC (IREP,IUFD,'S',JLEV,'PRESS.DEPARTURE',ZOUT,LLCOSP)
          !
          ! Vertical divergence
          !
          DO J = 1,IDGL
            ZFFT(J) = 0.0
          ENDDO
          !
          call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)
          !
          CALL FAIENC (IREP,IUFD,'S',JLEV,'VERTIC.DIVERGENC',ZOUT,LLCOSP)
          !
        ENDDO
      ENDIF
      ! Finish 3D-spectral data

      DEALLOCATE (ZOUT)
      DEALLOCATE (ZFFT)
      !
      ! 5.7 Grid point or Spectral Humidity
      !
      print*,'LQVGRP=',LQVGRP
      print*,'LSPRT=',LSPRT
      !IF(LQVGRP) THEN
      !
      IF ( LSPRT ) THEN
        ! Grid point specific humidity
        LLCOSP = .FALSE.
        ALLOCATE(ZOUT (IDGL))
        ZOUT(:) = 0.
        !
        READ(10,*)
        DO JLEV=1,IFLEV
          READ(10,*) ZQ
          DO J = 1,IDGL
            ZOUT(J)=ZQ
          ENDDO
          WRITE (6,*)' JLEV HUMI',JLEV,ZOUT(1)
          CALL FAIENC (IREP,IUFD,'S',JLEV,'HUMI.SPECIFI',ZOUT,LLCOSP)
        ENDDO
        DEALLOCATE (ZOUT)
      ELSEIF (.NOT. LSPRT) THEN
        !
        ! 5.6 Spectral Humidity
        !
        print*,'QV en spectral'
        WRITE(6,*)' Q_v en spectral'
        ALLOCATE(ZOUT (NSEFRE))
        ALLOCATE(ZFFT (IDGL)  )
        ALLOCATE(ZWORK(IDGL+1))
        !
        LLCOSP =.TRUE.
        !
        READ(10,*)
        !
        DO JLEV =1, IFLEV
          READ(10,*)ZQ
          DO J = 1,IDGL
            ZFFT(J)=ZQ
          ENDDO
          WRITE(6,*)'JLEV HUMI',JLEV,ZFFT(1)
          !
          call real2spec(idgl,nsefre,insmax,trigs, ifax, ZFFT, ZOUT)
          !
          CALL FAIENC (IREP,IUFD,'S',JLEV,'HUMI.SPECIFI',ZOUT,LLCOSP)
        ENDDO
        !
        DEALLOCATE (ZOUT)
        DEALLOCATE (ZFFT)
        DEALLOCATE (ZWORK)
      ENDIF
      !
      !ENDIF
      !
      !
      ! 5.8 Grid point specific cloud content
      !
      IF(LQCGRP) THEN 
        !
        READ(10,*) CBLOCK
        print*,'CBLOCK=',CBLOCK
        !
        IF (CBLOCK.NE.'CLOUD_WATER') THEN
          print*,&
         & 'the block CLOUD_WATER is not present while it is expected'
          STOP
        ELSE
          LLCOSP = .FALSE.      
          ALLOCATE(ZOUT (IDGL))
          ZOUT(:) = 0.                
          DO JLEV=1,IFLEV
            READ(10,*) ZQ
            DO J = 1,IDGL
              ZOUT(J)=ZQ
            ENDDO
            WRITE (6,*)' JLEV CLOUD_WATER',JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'CLOUD_WATER',ZOUT,LLCOSP)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'LIQUID_WATER',ZOUT,LLCOSP)
          ENDDO 
          DEALLOCATE (ZOUT)        
        ENDIF
      ENDIF
      !
      ! 5.9 Grid point specific ice cristal content
      ! 
      IF(LQIGRP) THEN
        !
        READ(10,*) CBLOCK
        print*,'CBLOCK=',CBLOCK
        !
        IF (CBLOCK.NE.'ICE_CRYSTAL') THEN
          print*, &
         & 'the block ICE_CRYSTAL is not present while it is expected'
          STOP
        ELSE
          LLCOSP = .FALSE.      
          ALLOCATE(ZOUT (IDGL))
          ZOUT(:) = 0.          
          DO JLEV=1,IFLEV
            READ(10,*) ZQ
            DO J = 1,IDGL
              ZOUT(J)=ZQ
            ENDDO
            WRITE (6,*)' JLEV ICE_CRYSTAL',JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'ICE_CRYSTAL',ZOUT,LLCOSP)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'SOLID_WATER',ZOUT,LLCOSP)
          ENDDO 
          DEALLOCATE (ZOUT)           
        ENDIF
      ENDIF 
      !
      ! 5.10 Grid point specific rain content
      !
      IF(LQRGRP) THEN 
        !
        READ(10,*) CBLOCK
        print*,'CBLOCK=',CBLOCK
        !
        IF (CBLOCK.NE.'RAIN') THEN
          print*,'the block RAIN is not present while it is expected'
          STOP
        ELSE
          LLCOSP = .FALSE.      
          ALLOCATE(ZOUT (IDGL))
          ZOUT(:) = 0.
          DO JLEV=1,IFLEV
            READ(10,*) ZQ          
            DO J = 1,IDGL
              ZOUT(J)=ZQ
            ENDDO
            WRITE (6,*)' JLEV RAIN',JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'RAIN',ZOUT,LLCOSP)
          ENDDO
          DEALLOCATE (ZOUT)
        ENDIF
      ENDIF
      !
      ! 5.11 Grid point specific snow content
      !
      IF(LQSGRP) THEN  
        !
        READ(10,*) CBLOCK
        print*,'CBLOCK=',CBLOCK
        !
        IF (CBLOCK.NE.'SNOW') THEN
          print*,'the block SNOW is not present while it is expected'
          STOP
        ELSE   
          LLCOSP = .FALSE.      
          ALLOCATE(ZOUT (IDGL))
          ZOUT(:) = 0.         
          DO JLEV=1,IFLEV
            READ(10,*) ZQ
            DO J = 1,IDGL
              ZOUT(J)=ZQ
            ENDDO
            WRITE (6,*)' JLEV SNOW',JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'SNOW',ZOUT,LLCOSP)
          ENDDO
          DEALLOCATE (ZOUT)
        ENDIF
      ENDIF
      !
      ! 5.12 Grid point specific graupel content
      !
      IF(LQGGRP) THEN 
        !
        READ(10,*) CBLOCK
        print*,'CBLOCK=',CBLOCK
        !
        IF (CBLOCK.NE.'GRAUPEL') THEN
          print*,'the block GRAUPEL is not present while it is expected'
          STOP
        ELSE                 
          LLCOSP = .FALSE.      
          ALLOCATE(ZOUT (IDGL))
          ZOUT(:) = 0.        
          DO JLEV=1,IFLEV
            READ(10,*) ZQ          
            DO J = 1,IDGL
              ZOUT(J)=ZQ
            ENDDO
            WRITE (6,*)' JLEV GRAUPEL',JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'GRAUPEL',ZOUT,LLCOSP)
          ENDDO
          DEALLOCATE (ZOUT) 
        ENDIF
      ENDIF
      !
      ! 5.13 Grid point cloud fraction
      !
      IF(LCFGRP) THEN 
        !
        READ(10,*) CBLOCK
        print*,'CBLOCK=',CBLOCK
        !
        IF (CBLOCK.NE.'CLOUD FRACTION') THEN
          print*,&
        & 'the block CLOUD FRACTION is not present while it is expected'
          STOP
        ELSE                 
          LLCOSP = .FALSE.      
          ALLOCATE(ZOUT (IDGL))
          ZOUT(:) = 0.      
          DO JLEV=1,IFLEV
            READ(10,*) ZQ          
            DO J = 1,IDGL
              ZOUT(J)=ZQ
            ENDDO
            WRITE (6,*)' JLEV CLF',JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,' CLOUD FRACTION',ZOUT,LLCOSP)
          ENDDO
          DEALLOCATE (ZOUT)
        ENDIF
      ENDIF
      !
      ! 5.14 Grid point SRC
      !
      IF(LSRCGRP) THEN 
        !
        READ(10,*) CBLOCK
        print*,'CBLOCK=',CBLOCK
        !
        IF (CBLOCK.NE.'SRC') THEN
          print*,'the block SRC is not present while it is expected'
          STOP
        ELSE                   
          LLCOSP = .FALSE.      
          ALLOCATE(ZOUT (IDGL))
          ZOUT(:) = 0.        
          DO JLEV=1,IFLEV
            READ(10,*) ZQ           
            DO J = 1,IDGL
              ZOUT(J)=ZQ
            ENDDO
            WRITE (6,*)' JLEV SRC',JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,'SRC',ZOUT,LLCOSP)
          ENDDO
          DEALLOCATE (ZOUT)
        ENDIF
      ENDIF
      !
      ! 7.15 Grid point TKE
      !
      IF(LTKEGRP) THEN 
         !
         READ(10,*) CBLOCK
         print*,'CBLOCK=',CBLOCK
         !
         IF (CBLOCK.NE.'TKE') THEN
            print*,'the block TKE is not present'
           !
         ELSE
           LLCOSP = .FALSE.      
           ALLOCATE(ZOUT (IDGL))
           ZOUT(:) = 0.                 
           DO JLEV=1,IFLEV
             READ(10,*) ZQ
             DO J = 1,IDGL
               !ZOUT(J)=MAX(0.000001,ZQ)
               ZOUT(J)=ZQ
             ENDDO
             WRITE (6,*)' JLEV TKE',JLEV,ZOUT(1)
             CALL FAIENC (IREP,IUFD,'S',JLEV,'TKE',ZOUT,LLCOSP)
           ENDDO
           DEALLOCATE (ZOUT)
         ENDIF
      ENDIF
      ! ----------------------------
      ! 6. Write data for FORCING
      ! ----------------------------
      READ(10,*) CBLOCK
      print*,'CBLOCK=',CBLOCK
      !
      IF (CBLOCK.NE.'FORCING') THEN
         print*,'the block FORCING is not present'
         !
      ELSE
        !
        !  Preparation for GP fields
        !
        LLCOSP = .FALSE.
        ALLOCATE(ZOUT (IDGL))
        ZOUT(:) = 0.
        !
        ! loop for forcing
        !
        DO JFORC=1,NFORC
          !  FORCING no JFORC
          WRITE(YRECFM,'(A4,I4.4)') 'FORC',JFORC
          READ(10,*) FORCAGE
          print*,'YRECFM=',YRECFM, FORCAGE
          DO JLEV=1,IFLEV
            READ(10,*) ZZ
            DO J = 1,IDGL
              ZOUT(J)=ZZ
            ENDDO
            WRITE (6,*)YRECFM,JLEV,ZOUT(1)
            CALL FAIENC (IREP,IUFD,'S',JLEV,YRECFM,ZOUT,LLCOSP)
          ENDDO            
        !
        ENDDO
        !
        DEALLOCATE (ZOUT)
      ENDIF
      ! ----------------------------
      ! 6. Write data for SURFACE FORCING
      ! ----------------------------
      READ(10,*) CBLOCK
      print*,'CBLOCK=',CBLOCK
      !
      IF (CBLOCK.NE.'SURF.FORC') THEN
        print*,'the block SURF. FORC is not present'
        print*,'just backspace and continue'
        BACKSPACE(10)
      ELSE
        !
        !  Preparation for GP fields
        !
        LLCOSP = .FALSE.      
        ALLOCATE(ZOUT (IDGL))
        ZOUT(:) = 0.
        !
        ! loop for forcing
        !
        DO JFORC=1,NFORCS
          !  FORCING no JFORC
          WRITE(YRECFM,'(A4,I4.4)') 'FORC',JFORC
          print*,'YRECFM=',YRECFM
          READ(10,*)
          READ(10,*) ZZ
          DO J = 1,IDGL
            ZOUT(J)=ZZ
          ENDDO
          WRITE (6,*)YRECFM,JLEV,ZOUT(1)
          CALL FAIENC (IREP,IUFD,'SURF',1,YRECFM,ZOUT,LLCOSP)
        ENDDO
        !
        DEALLOCATE (ZOUT)
      ENDIF
      ! ----------------------------
      ! 7. Write data for SURFACE MAX=100.
      ! ----------------------------
      !
      ! Surface fields if Arpege/Aladin physics
      !
      ! Champs pour le rayonnement: aerosol et ozone
      !
      LLCOSP = .FALSE.
      ALLOCATE(ZOUT (IDGL))
      ZOUT(:) = 0.
      DO I=1,100
        READ(10,'(A16)')CHAMP
        IF (CHAMP.EQ.'STOP') THEN
          GOTO 999
        ENDIF
        READ(10,*)ZZ
        !WRITE(6,*) 'ZZ',ZZ
        print*,CHAMP
        print*,CHAMP(5:16),ZZ
        DO J = 1,IDGL
          ZOUT(J) = ZZ
        ENDDO
        CALL FAIENC(IREP,IUFD,CHAMP(1:4),JLEV,CHAMP(5:16),ZOUT,LLCOSP)
      ENDDO
      DEALLOCATE (ZOUT)
      !
      ! ---------
      ! 8. Finish
      ! ---------
      !
      ! 8.1 Close the FA file
 999  WRITE(6,*)'FERMETURE DU FICHIER FA'
      LLOUT = .FALSE.
      CALL LFILAF(IREP,IUFD,LLOUT)
      CALL FAIRME(IREP,IUFD,'UNKNOWN')
      !
      ! 8.2 The end
      !
      IUFD   = IULOUT
      LLNOMM = .TRUE.
      LLERFA = .TRUE.
      LLIMST = .FALSE.
      INIMES = 2
      IBARP  = INART
      !
      CALL FAITOU(IREP,IUFD,LLNOMM,CLFILEOUT,'UNKNOWN', &
     &            LLERFA,LLIMST,INIMES,IBARP,IBARI,CLMCA)
      !
      DEALLOCATE(TRIGS)
      DEALLOCATE(NESN0)
      DEALLOCATE(NCPL4N)
      DEALLOCATE(IKNTMP)
      DEALLOCATE(IKMTMP)
      DEALLOCATE(ZVALH)
      DEALLOCATE(ZVBH)
      !
      !
      WRITE(6,*)
      WRITE(6,'(A)') 'END OF ACADFA'
      CLOSE (6)
      CLOSE(10)
      STOP
      END PROGRAM ACADFA1D_MAIN
