
! Wrappers to LFA library for Python (epygram)

subroutine wlfaouv(kreturncode, cdnomf, cdtypo, kul)
! --------------------------------------------------------------------------
! **** *WLFAOUV* Open a LFA file.
! --------------------------------------------------------------------------
! Input:
! cdnomf      file name.
! cdtypo      opening type: 'R' READ, 'W' WRITE, 'A' APPEND, 'S' SCRATCH.
! Output:
! kreturncode code error code
! kul         logical unit of LFA file.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
character*(*), intent(in) :: cdnomf
character*(*), intent(in) :: cdtypo
integer(kind=8), intent(out) :: kreturncode
integer(kind=8), intent(out) :: kul

INTEGER(KIND=jpintusr), PARAMETER :: JPMAXLOGICALUNITNUMBER=5000
INTEGER(KIND=jpintusr) :: INUMER
LOGICAL :: LLEXISTS, LLOPEN

integer(kind=jpintusr) :: iul

! find a free logical unit
INUMER=10
LLEXISTS=.FALSE.
LLOPEN=.TRUE.
DO WHILE(INUMER.LT.JPMAXLOGICALUNITNUMBER .AND. (LLOPEN .OR. .NOT. LLEXISTS))
  INUMER=INUMER+1
  INQUIRE(UNIT=INUMER, EXIST=LLEXISTS, OPENED=LLOPEN)
ENDDO
IF(LLOPEN .OR. .NOT. LLEXISTS) THEN
  KRETURNCODE=-999
ENDIF

! (Re)-init of libgfortran to enable big_endian file reading
#ifdef __GFORTRAN__
CALL INIT_GFORTRAN_BIG_ENDIAN()
#endif

! call lfaouv
iul = inumer
call lfaouv(iul, cdnomf, cdtypo)
kul = int(iul, 8)
! (Re)-init of libgfortran to enable native endianess file reading
#ifdef __GFORTRAN__
CALL INIT_GFORTRAN_NATIVE_ENDIAN()
#endif

end subroutine wlfaouv



subroutine wlfafer(kul)
! --------------------------------------------------------------------------
! **** *WLFAFER* Close a LFA file.
! --------------------------------------------------------------------------
! Input:
! kul        logical unit of LFA file.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul

integer(kind=jpintusr) :: iul

iul = int(kul, jpintusr)
call lfafer(iul)

end subroutine wlfafer



subroutine wlfaecrr(kul, cdna, preel, klong)
! --------------------------------------------------------------------------
! **** *WLFAECRR* Write real data on LFA file.
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             name of article to write.
! preel(1,klong)   real data to write.
! klong            length of article to write.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul, klong
character*(*), intent(in)   :: cdna
real(kind=8), intent(out)   :: preel(klong)

integer(kind=jpintusr) :: iul, ilong
real(kind=jpreeusr)    :: zreel(klong)

zreel = real(preel, jpreeusr)
iul = int(kul, jpintusr)
ilong = int(klong, jpintusr)
call lfaecrr(iul, cdna, zreel, ilong)

end subroutine wlfaecrr



subroutine wlfaecri(kul, cdna, kentier, klong)
! --------------------------------------------------------------------------
! **** *WLFAECRI* Write integer data of LFA file.
! --------------------------------------------------------------------------
! Input:
! kul                  logical unit of LFA file.
! cdna                 name of article to write.
! kentier(1,klong)     integers to write.
! klong                length of article to write.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul, klong
character*(*), intent(in) :: cdna
integer(kind=8), intent(in) :: kentier(klong)

integer(kind=jpintusr) :: iul, ilong
real(kind=jpreeusr)    :: ientier(klong)

ientier = real(kentier, jpreeusr)
iul = int(kul, jpintusr)
ilong = int(klong, jpintusr)
call lfaecri(iul, cdna, ientier, ilong)

end subroutine wlfaecri



subroutine wlfaecrc(kul, cdna, cdcar, klong)
! --------------------------------------------------------------------------
! **** *WLFAECRC* Write character data on LFA file.
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             name of article to write.
! cdcar(1,klong)   characters to write.
! klong            length of article to write.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul, klong
character*(*), intent(in) :: cdna
character*(*), intent(in) :: cdcar(klong)

integer(kind=jpintusr) :: iul, ilong

iul = int(kul, jpintusr)
ilong = int(klong, jpintusr)
call lfaecrc(iul, cdna, cdcar, ilong)

end subroutine wlfaecrc



subroutine wlfalecr(kreturncode, kul, cdna, kdimb, preel, klong)
! --------------------------------------------------------------------------
! **** *WLFALECR* Read real data on LFA file.
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             article name.
! kdimb            physical dimension of array preel.
! Output:
! kreturncode error code
! preel(1,klong)   real elements read.
! klong            number of real elements read.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul, kdimb
character*(*), intent(in) :: cdna
integer(kind=8), intent(out) :: kreturncode
real(kind=8), intent(out) :: preel(kdimb)
integer(kind=8), intent(out) :: klong

real(kind=jpreeusr)    :: zreel(kdimb)
integer(kind=jpintusr) :: iul, idimb
integer(kind=jpintusr) :: ilong
integer(kind=jpintusr) :: ierr

iul = int(kul, jpintusr)
idimb = int(kdimb, jpintusr)
call lfalecr(iul, cdna, idimb, zreel, ilong, ierr)
preel = real(zreel, 8)
klong = int(ilong, 8)
kreturncode = int(ierr, 8)

end subroutine wlfalecr



subroutine wlfaleci(kreturncode, kul, cdna, kdimb, kentier, klong)
! --------------------------------------------------------------------------
! **** *WLFALECI* Read integer data on LFA file.
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             article name.
! kdimb            physical dimension of array kentier.
! Output:
! kreturncode      error code
! kentier(1,klong) integer elements read.
! klong            number of integer elements read.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul, kdimb
character*(*), intent(in) :: cdna
integer(kind=8), intent(out) :: kreturncode
integer(kind=8), intent(out) :: kentier(kdimb)
integer(kind=8), intent(out) :: klong

integer(kind=jpintusr) :: ientier(kdimb)
integer(kind=jpintusr) :: iul, idimb
integer(kind=jpintusr) :: ilong
integer(kind=jpintusr) :: ierr

iul = int(kul, jpintusr)
idimb = int(kdimb, jpintusr)
call lfaleci(iul, cdna, idimb, ientier, ilong, ierr)
kentier = int(ientier, 8)
kreturncode = int(ierr, 8)
klong = int(ilong, 8)

end subroutine wlfaleci



subroutine wlfalecc(kreturncode, kul, cdna, kdimb, klenc, clcar, klong)
! --------------------------------------------------------------------------
! **** *WLFALECC* Read character data on LFA file.
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             article name.
! kdimb            physical dimension of array clcar.
! klenc            max length of strings in clcar
! Output:
! kreturncode      error code
! clcar            array of elements read. 
! klong            number of character elements read.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul, kdimb, klenc
character*(*), intent(in) :: cdna
integer(kind=8), intent(out) :: kreturncode
character(len=klenc), intent(out) :: clcar(kdimb)
integer(kind=8), intent(out) :: klong

integer(kind=jpintusr) :: iul, idimb
integer(kind=jpintusr) :: ireturncode
integer(kind=jpintusr) :: ilong

iul = int(kul, jpintusr)
idimb = int(kdimb, jpintusr)
call lfalecc(iul, cdna, idimb, clcar, ilong, ireturncode)
klong = int(ilong, 8)
kreturncode = int(ireturncode, 8)

end subroutine wlfalecc



subroutine wlfacas(kreturncode, kul, cdna, cdtype, klong)
! --------------------------------------------------------------------------
! **** *WLFACAS* Get documentation about a LFA article.
! --------------------------------------------------------------------------
! Input:
! kul               file logical unit.
! cdna:             name of required article.
! Output:
! kreturncode       error code
! cdtype            article type: 'R4', 'I8', 'C '.
! klong             number of elements in this article.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul
character*(*), intent(in) :: cdna
integer(kind=8), intent(out) :: kreturncode
character(len=2), intent(out) :: cdtype
integer(kind=8), intent(out) :: klong

integer(kind=jpintusr) :: ierr
integer(kind=jpintusr) :: iul
integer(kind=jpintusr) :: ilong

iul = int(kul, jpintusr)
call lfacas(iul, cdna, cdtype, ilong, ierr)
klong = int(ilong, 8)
kreturncode=int(ierr, 8)

end subroutine wlfacas



subroutine wlfalaft(kul, kdlis, klenc, knlis, cclis)
! --------------------------------------------------------------------------
! **** *WLFALAFT* Article list of a LFA file, on an array.
! --------------------------------------------------------------------------
! Input:
! kul            logical unit of LFA file.
! kdlis          physical dimension of array cdlis.
! klenc            max length of strings in clcar
! Output:
! knlis          number of articles on the file. This number is also
!                the number of elements written on cclis.
! cclis          array of article names.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=8), intent(in) :: kul, kdlis, klenc
integer(kind=8), intent(out) :: knlis
character(len=klenc), intent(out) :: cclis(kdlis)

integer(kind=jpintusr) :: iul, idlis
integer(kind=jpintusr) :: inlis

iul = int(kul, jpintusr)
idlis = int(kdlis, jpintusr)
call lfalaft(iul, cclis, idlis, inlis)
knlis = int(inlis, 8)

end subroutine wlfalaft



subroutine wlfatest(kreturncode, cdnomf, ldlfa)
! --------------------------------------------------------------------------
! **** *WLFATEST* Test if a file is a LFA one.
! --------------------------------------------------------------------------
! Input:
! cdnomf      file name.
! Output:
! kreturncode     code error code
! ldlfa=.true. if the file is a LFA one, .false. else case.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
character*(*), intent(in) :: cdnomf
integer(kind=8), intent(out) :: kreturncode
logical, intent(out) :: ldlfa

INTEGER(KIND=jpintusr), PARAMETER :: JPMAXLOGICALUNITNUMBER=5000
INTEGER(KIND=jpintusr) :: INUMER
LOGICAL :: LLEXISTS, LLOPEN

! find a free logical unit
INUMER=10
LLEXISTS=.FALSE.
LLOPEN=.TRUE.
DO WHILE(INUMER.LT.JPMAXLOGICALUNITNUMBER .AND. (LLOPEN .OR. .NOT. LLEXISTS))
  INUMER=INUMER+1
  INQUIRE(UNIT=INUMER, EXIST=LLEXISTS, OPENED=LLOPEN)
ENDDO
IF(LLOPEN .OR. .NOT. LLEXISTS) THEN
  KRETURNCODE=-999
ENDIF

! (Re)-init of libgfortran to enable big_endian file reading
#ifdef __GFORTRAN__
CALL INIT_GFORTRAN_BIG_ENDIAN()
#endif

call lfatest(INUMER, cdnomf, ldlfa)

! (Re)-init of libgfortran to enable native endianess file reading
#ifdef __GFORTRAN__
CALL INIT_GFORTRAN_NATIVE_ENDIAN()
#endif

end subroutine wlfatest



