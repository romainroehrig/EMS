subroutine real2spec(&
 &  ndgl,& 
 & nsefre,&
 & nsmax,&
  & ptrigs ,&! (I) - tabulated sin and cos for FFT
  & kfax   ,&! (I) - prime factors list of the Fourier vectors length
  & preal,   &! (I) - gridpoint field
  & pspec  &! (O) - spectral field
&)

! -----
! PURPOSE:
!   spectral ---> gridpoint transforms for 2D vertical plane 
!   model diagnostics
!
! METHOD:
!   Input array pspec contains spectral coefficients
!   ordered as in 2D vertical plane ALADIN file.
!   Output array preal contains gridpoint values.
!
! EXTERNALS:
!   fft991 or fft992  (xrd)
!         
! REFERENCE:
!   Meteo-France CNRM/GMAP
!
! ORIGINAL:
!   ??-10-1998, Pierre Benard
!
! MODIFICATIONS:
!   16-08-2002, J. Masek:
!     Thinned interface, cleaning, rewriting into free from.
!
!   02-01-2006, J. Masek:   
!     All modules merged into mod_aux, cpp macro PALADIN introdced for better 
!     portability.
!  10-05-2007, S.Malardel
!     Remove module and pass dimension with argument
!
! -----

implicit none

! -----
! I/O arguments
! -----
INTEGER, intent(in) :: ndgl, nsefre, nsmax
INTEGER, intent(in) :: kfax(20)

REAL, intent(in)  :: ptrigs(ndgl), preal (ndgl)
REAL, intent(out) :: pspec(nsefre)

! -----
! local variables
! -----
      
INTEGER :: ifft, jn
INTEGER :: ilot, inc, isign

! -----
! local arrays
! -----
REAL :: zreal(ndgl + 2)


! =====

! -----
! reorder spectral coefficients: 2D vertical plane ALADIN file ---> FFT
! -----

! initialize I/O array with zeroes

zreal(:)=0.
zreal(1:ndgl) = preal(1:ndgl) 

! -----
! real transformation--->spec 
! -----
      
! preparations
inc   = 1  ! increment within data vector
ilot  = 1  ! number of data vectors
isign = -1  ! spectral ---> gridpoint transformation

! Fourier transform (zreal is used for I/O, zwork is unused)
!ifdef PALADIN
!call fft991(zreal, zwork, ptrigs, kfax, inc, ndgl, ndgl, ilot, isign)
!else
call fft992(zreal, ptrigs, kfax, inc, ndgl, ndgl, ilot, isign)
!endif

! fill in spectral coefficients relevant for 2D model
ifft = 1
do jn = 0, nsmax
  pspec(4*jn + 1)=  zreal(ifft) 
  ifft        = ifft + 1
 pspec(4*jn + 2)=  zreal(ifft) 
  ifft        = ifft + 1
enddo


end subroutine real2spec

