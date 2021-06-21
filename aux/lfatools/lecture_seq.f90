subroutine lfaminm(kul)
! --------------------------------------------------------------
! **** *LFAMINM* Extrema de tous les articles d'un fichier LFA.
! **** *LFAMINM* Extrema of all articles of a given LFA file.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kul unite logique du fichier LFA d'entree.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
! Input:
! kul logical unit of LFA file.
! Output:
! Extrema on standard output.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,ilong,ierr
integer(kind=jpintusr) iversion
integer(kind=jpintesb) iversionesb
character*200 clna
#include"lfayom.h"
character*2 cltype
if(lglang) then
	write(*,'(3a)') 'LFAMINM du fichier ' &
& 		,cgnomf(kul)(1:index(cgnomf(kul),' ')),':'
else
	write(*,'(3a)') 'LFAMINM from file ' &
& 		,cgnomf(kul)(1:index(cgnomf(kul),' ')),':'
endif
rewind(kul)
read(kul) iversionesb
iversion=iversionesb
!
! Position du pointeur.
!
lgpoint(kul)=.false.
  100 continue
!
! -------------------------------------------------
! Avancee d'un article dans le fichier LFA.
! -------------------------------------------------
!
clna=' '
call lfacas(kul,clna,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! On n'est pas en fin de fichier.
	!
	if(cltype(1:1) == 'R') then
		!
		! Article de type reel.
		!
		call lfaiminmr(kul,clna,cltype,ilong)
	elseif(cltype(1:1) == 'I') then
		!
		! Article de type entier.
		!
		call lfaiminmi(kul,clna,cltype,ilong)
	elseif(cltype(1:1) == 'C') then
		!
		! Article de type caractere.
		!
		call lfaiminmc(kul,clna,cltype,ilong)
	else
		print*,'LFAMINM/ATTENTION: type de donnee inconnu!...'
		print*,cltype
	endif
	goto 100
endif
end
