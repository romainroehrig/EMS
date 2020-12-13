      SUBROUTINE ACADTR(IDGL, IDLON,  &
                      INSMAX, NESN0, NCPL4N, NSEFRE, &
                      ZFFT, ZOUT, ZWORK, &
                      TRIGS,IFAX)
!
!**** *ACADTR*  
!
!     PURPOSE.
!     --------
!        TO REAL-> SPECTRAL TRANSFORMS FOR ACADFA (Meridional)
!
!**   INTERFACE.
!     ----------
!       *SUBROUTINE ACADTR*
!
!     METHOD.
!     -------
!        SEE DOCUMENTATION
!        The input array is ZFFT (containing real data)
!        The output array is the spectral ZOUT(NSEFRE)
!        with coefficients ordered readily for Aladin file
!
!     EXTERNALS.
!     ----------
!         FFT package
!         
!     REFERENCE.
!     ----------
!        METEO-FRANCE CNRM/GMAP
!
!     AUTHOR.
!     -------
!        Pierre Benard
!
!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 98-10-98
!     ------------------------------------------------------------------
! (in)  IDGL    = Number of latitudes
! (in)  IDLON   = Number of longitudes (1 or 4?)
! (in)  IFAX    = Prime factors list of the Fourier vectors length
! (in)  INSMAX  = Meridional Truncation
! (in)  NESN0   = Index of first coefficient of JN in spectral array
! (in)  INSMAX  = Number of spectral coefs. of JN in spectral array
! (in)  TRIGS   = Tabulated sin and cos for FFTs
! (in)  ZFFT    = Real to be transformed
! (in)  ZWORK   = FFT work array
!     ------------------------------------------------------------------
! (out) ZOUT    = spectral and ordered array
!     ------------------------------------------------------------------

      IMPLICIT LOGICAL (L)
      
      INTEGER ISIGN, IINC, INDFFT
      INTEGER IDGL, IDLON, INSMAX
      INTEGER IFAX(20)
      INTEGER NESN0(0:INSMAX)
      INTEGER NCPL4N(0:INSMAX)
      
      REAL TRIGS(IDGL)            
      REAL ZOUT(NSEFRE),ZFFT(IDLON*IDGL)
      REAL ZWORK((IDGL+1)*IDLON)
      
! 1.  PREPARATION
!    
      ISIGN = -1
      IINC  = 1 
  
! 2.  FOURIER TRANSFORM
!     
      CALL FFT991(ZFFT,ZWORK,TRIGS,IFAX ,  &
                  IINC,IDGL,IDGL,IDLON,ISIGN)

! 3. REORDERING ACCORDING TO ALADIN FILES AND FILLING WITH
!    ZEROS THE IRRELEVANT COEFFICIENTS FOR A 2D FIELD
!
      INDFFT=1
      DO JN=0, INSMAX
        DO JROF=NESN0(JN),NESN0(JN)+1
          ZOUT(JROF)=ZFFT(INDFFT)
          INDFFT=INDFFT+1
        ENDDO
        DO JROF=NESN0(JN)+2,NESN0(JN)+NCPL4N(JN)-1
          ZOUT(JROF)=0.
        ENDDO
      ENDDO

     
      RETURN
      END
      
