program lfadiffart
! --------------------------------------------------------------
! **** *lfadiffart* Différence de deux fichiers LFA.
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
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) iarg,iul1,iul2,ilong1,ierr1,ilong2 &
& ,ierr2,ilna1,ilna2,ilfe1,ilfe2,inarg
integer(kind=4) iarg4,iargc
character*200 clfe1,clfe2,clna1,clna2,cltype1,cltype2
character*3 cllang
logical llimpri
!
! -------------------------------------------------
! Initialisation par défaut.
! -------------------------------------------------
!
!
! Saisie de la ligne de commande.
!
iarg4=iargc() ! nombre d'arguments.
iarg=iarg4
if(iarg /= 2) then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Différence entre les listes d''articles de deux fichiers LFA.'
		print*,' '
		print*,'Utilisation: lfadiffart F1 F2 '
		print*,'avec'
		print*,'	F1 et F2 les deux fichiers d''entrée.'
		print*,' '
		stop
	else
		print*,' '
		print*,'Articles list difference between two LFA files.'
		print*,' '
		print*,'Usage: lfadiffart F1 F2 '
		print*,'with'
		print*,'	F1 and F2 the two input files.'
		print*,' '
		stop
	endif
endif
inarg=1
call getargp(inarg,clfe1)
inarg=2
call getargp(inarg,clfe2)
!
! Ouverture des fichiers.
!
iul1=72
call lfaouv(iul1,clfe1,'R')
call lfaerf(iul1,.false.)
iul2=73
call lfaouv(iul2,clfe2,'R')
call lfaerf(iul2,.false.)
!
! -------------------------------------------------
! On lit le fichier 1 séquentiellement.
! -------------------------------------------------
!
llimpri=.false.
  100 continue
!
! Renseignements sur l'article suivant du fichier.
!
clna1=' '
call lfacas(iul1,clna1,cltype1,ilong1,ierr1)
call lfacas(iul2,clna1,cltype2,ilong2,ierr2)
ilna1=len_trim(clna1)
if(ierr1 == 0) then
	!
	!-------------------------------------------------
	! On n'est pas en fin du fichier 1.
	!-------------------------------------------------
	!
	if(ierr2 /= 0) then
		!
		! L'article du fichier 1 n'existe pas
		! dans le fichier 2.
		!
		if(.not.llimpri) then
			ilfe1=len_trim(clfe1)
			if(cllang() == 'FRA') then
				print*,'Seulement dans ',clfe1(1:ilfe1),':'
			else
				print*,'Only in ',clfe1(1:ilfe1),':'
			endif
			llimpri=.true.
		endif
		ilna1=len_trim(clna1)
		print*,'	',clna1(1:ilna1)
	endif
	goto 100
endif
!
! -------------------------------------------------
! On lit le fichier 2 séquentiellement.
! -------------------------------------------------
!
llimpri=.false.
call lfarew(iul1)
call lfarew(iul2)
  200 continue
!
! Renseignements sur l'article suivant du fichier.
!
clna2=' '
call lfacas(iul2,clna2,cltype2,ilong2,ierr2)
call lfacas(iul1,clna2,cltype1,ilong1,ierr1)
ilna2=len_trim(clna2)
if(ierr2 == 0) then
	!
	!-------------------------------------------------
	! On n'est pas en fin du fichier 2.
	!-------------------------------------------------
	!
	if(ierr1 /= 0) then
		!
		! L'article du fichier 2 n'existe pas
		! dans le fichier 1.
		!
		if(.not.llimpri) then
			ilfe2=len_trim(clfe2)
			if(cllang() == 'FRA') then
				print*,'Seulement dans ',clfe2(1:ilfe2),':'
			else
				print*,'Only in ',clfe2(1:ilfe2),':'
			endif
			llimpri=.true.
		endif
		ilna2=len_trim(clna2)
		print*,'	',clna2(1:ilna2)
	endif
	goto 200
endif
!
! Fermeture des fichiers.
!
call lfafer(iul1)
call lfafer(iul2)
end
