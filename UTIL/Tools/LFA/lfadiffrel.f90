program lfadiffrel
! --------------------------------------------------------------
! **** *lfadiffrel* Différence de deux fichiers LFA.
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
integer(kind=jpintusr) iarg,iul1,iul2,iuls,ilong1,ierr1,ilong2 &
& ,ierr2,ilna1,inarg
integer(kind=4) iarg4,iargc
real(kind=jpreeusr) zindef
character*200 clfe1,clfe2,clfs,clna1,cltype1,cltype2
character*3 cllang
!
! -------------------------------------------------
! Initialisation par défaut.
! -------------------------------------------------
!
zindef=999.999
!
! Saisie de la ligne de commande.
!
iarg4=iargc() ! nombre d'arguments.
iarg=iarg4
if(iarg /= 3) then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Différence relative de deux fichiers LFA.'
		print*,' '
		print*,'Utilisation: lfadiffrel F1 F2 FDIFF'
		print*,'avec'
		print*,'	F1 et F2 les deux fichiers d''entrée.'
		print*,'	FDIFF le fichier de sortie, recevant' &
& 		,' (F2-F1)/mva(F1).'
		print*,' '
		print*,'mva(F1) est la moyenne de la valeur absolue de l''article de F1.'
		print*,'Remarque: la différence est opérée sur' &
& 		,' les articles'
		print*,'communs aux deux fichiers.'
		print*,' '
		print*,'Lorsque mva(F1)=0, le résultat est nul si F2=0,' &
& 		,' et égal à ',zindef,' sinon.'
		print*,' '
		stop
	else
		print*,' '
		print*,'Relative difference between two LFA files.'
		print*,' '
		print*,'Usage: lfadiffrel F1 F2 FDIFF'
		print*,'with'
		print*,'	F1 and F2 the two input files.'
		print*,'	FDIFF the output file, receiving' &
& 		,' (F2-F1)/mav(F1).'
		print*,' '
		print*,'mav(F1) is the mean absolute value of the F1 article.'
		print*,'Nota: the difference is calculated on' &
& 		,' articles'
		print*,' present in both files.'
		print*,' '
		print*,'If mav(F1)=0, result is 0 if F2=0,' &
& 		,' and equal to ',zindef,' else case.'
		print*,' '
		stop
	endif
endif
inarg=1
call getargp(inarg,clfe1)
inarg=2
call getargp(inarg,clfe2)
inarg=3
call getargp(inarg,clfs)
!
! Ouverture des fichiers.
!
iul1=72
call lfaouv(iul1,clfe1,'R')
iul2=73
call lfaouv(iul2,clfe2,'R')
call lfaerf(iul2,.false.)
iuls=74
call lfaouv(iuls,clfs,'W')
!
! -------------------------------------------------
! On lit le fichier 1 séquentiellement.
! -------------------------------------------------
!
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
	! On n'est pas en fin de fichier 1.
	!
	if(ierr2 == 0) then
		!
		! L'article du fichier 1 existe bien
		! dans le fichier 2.
		! Y a-t-il bien le même type?
		!
		if(cltype1(1:1) == cltype2(1:1)) then
			!
			! Les deux articles ont bien le même type.
			! Ont-ils également la même longueur?
			!
			if(ilong1 == ilong2) then
				if(cltype1(1:1) == 'R') then
					!
					! Article de type réel.
					!
					call lfadiffrelr(iul1,iul2,iuls,clna1,ilong1,zindef)
				elseif(cltype1(1:1) == 'I') then
					!
					! Article de type entier.
					!
					call lfadiffreli(iul1,iul2,iuls,clna1,ilong1,zindef)
				else
					!
					! Autres types d'article.
					! On ne fait pas la différence.
					! On avance à l'article suivant.
					!
					call lfaavan(iul1)
				endif
			else
				print*,'LFADIFFREL/ATTENTION: l''article ',clna1(1:ilna1) &
& 				,' n''a pas la meme longueur dans les deux fichiers.'
			endif
		else
			print*,'LFADIFFREL/ATTENTION: l''article ',clna1(1:ilna1) &
& 			,' n''a pas le meme type dans les deux fichiers.'
		endif
	elseif(ierr2 == -1) then
		print*,'LFADIFFREL/ATTENTION: l''article ',clna1(1:ilna1) &
& 		,' est absent du fichier 2.'
	else
		print*,'LFADIFFREL/ATTENTION: code reponse ',ierr2 &
& 		,' en recherche de l''article ',clna1(1:ilna1)
	endif
	goto 100
endif
!
! Fermeture des fichiers.
!
call lfafer(iul1)
call lfafer(iul2)
call lfafer(iuls)
end
subroutine lfadiffrelr(kul1,kul2,kuls,cdna,klong,pindef)
! --------------------------------------------------------------
! **** *lfadiffrelR* Différence de deux articles réels.
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
! kul1, kul2: unités logiques des deux fichiers d'entrée.
! kuls: unité logique de sortie.
! cdna: nom de l'article dont on veut calculer la différence.
! klong: dimension physique du tableau à différencier.
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul1,kul2,kuls,klong,ilong,ierr,jlong
real(kind=jpreeusr) pindef,zmva
character*(*) cdna
real(kind=jpreeusr) zreel1(klong)
real(kind=jpreeusr) zreel2(klong)
!
! Lecture de l'article cdna sur les deux fichiers.
!
call lfalecr(kul1,cdna,klong,zreel1,ilong,ierr)
call lfalecr(kul2,cdna,klong,zreel2,ilong,ierr)
!
! Calcul du mva.
!
zmva=0.
do jlong=1,klong
	zmva=zmva+abs(zreel1(jlong))
enddo
if(zmva <= 0.) then
	!
	! Le mva est nul.
	!
	do jlong=1,klong
		if(zreel2(jlong) == 0.) then
			zreel1(jlong)=0.
		else
			zreel1(jlong)=pindef
		endif
	enddo
else
	!
	! Le mva est non nul.
	! Calcul de la différence relative.
	!
	zmva=zmva/klong
	do jlong=1,klong
		zreel1(jlong)=(zreel2(jlong)-zreel1(jlong))/zmva
	enddo
endif
!
! Ecriture de la différence sur le fichier de sortie.
!
call lfaecrr(kuls,cdna,zreel1,ilong)
end
subroutine lfadiffreli(kul1,kul2,kuls,cdna,klong,pindef)
! --------------------------------------------------------------
! **** *lfadiffrelI* Différence de deux articles entiers.
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
! kul1, kul2: unités logiques des deux fichiers d'entrée.
! kuls: unité logique de sortie.
! cdna: nom de l'article dont on veut calculer la différence.
! klong: dimension physique du tableau à différencier.
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul1,kul2,kuls,klong,ilong,ierr,jlong
real(kind=jpreeusr) pindef,zmva
character*(*) cdna
integer(kind=jpintusr) ient1(klong)
integer(kind=jpintusr) ient2(klong)
real(kind=jpreeusr) zres(klong)
!
! Lecture de l'article cdna sur les deux fichiers.
!
call lfaleci(kul1,cdna,klong,ient1,ilong,ierr)
call lfaleci(kul2,cdna,klong,ient2,ilong,ierr)
!
! Calcul du mva.
!
zmva=0.
do jlong=1,klong
	zmva=zmva+abs(real(ient1(jlong)))
enddo
if(zmva <= 0.) then
	!
	! Le mva est nul.
	!
	do jlong=1,klong
		if(ient2(jlong) == 0) then
			zres(jlong)=0.
		else
			zres(jlong)=pindef
		endif
	enddo
else
	!
	! Le mva est non nul.
	! Calcul de la différence relative.
	!
	zmva=zmva/klong
	do jlong=1,klong
		zres(jlong)=real(ient2(jlong)-ient1(jlong))/zmva
	enddo
endif
!
! Ecriture de la différence sur le fichier de sortie.
!
call lfaecrr(kuls,cdna,zres,ilong)
end
