program lfadiff
! --------------------------------------------------------------
! **** *lfadiff* Différence de deux fichiers LFA.
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
character*200 clfe1,clfe2,clfs,clna1,cltype1,cltype2
character*3 cllang
!
! Saisie de la ligne de commande.
!
iarg4=iargc() ! nombre d'arguments.
iarg=iarg4
if(iarg /= 3) then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Différence de deux fichiers LFA.'
		print*,' '
		print*,'Utilisation: lfadiff F1 F2 FDIFF'
		print*,'avec'
		print*,'	F1 et F2 les deux fichiers d''entrée.'
		print*,'	FDIFF le fichier de sortie, recevant F2-F1.'
		print*,' '
		print*,'Remarque: la différence est opérée sur les articles'
		print*,'communs aux deux fichiers.'
		print*,' '
		stop
	else
		print*,' '
		print*,'Difference between two LFA files.'
		print*,' '
		print*,'Usage: lfadiff F1 F2 FDIFF'
		print*,'with'
		print*,'	F1 and F2 the two input files.'
		print*,'	FDIFF the output LFA file, receiving F2-F1.'
		print*,' '
		print*,'Nota: the difference is calculated on articles'
		print*,'present in both files.'
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
		if(cltype1 == cltype2) then
			!
			! Les deux articles ont bien le même type.
			! Ont-ils également la même longueur?
			!
			if(ilong1 == ilong2) then
				if(cltype1(1:1) == 'R') then
					!
					! Article de type réel.
					!
					call lfadiffr(iul1,iul2,iuls,clna1,ilong1)
				elseif(cltype1(1:1) == 'I') then
					!
					! Article de type entier.
					!
					call lfadiffi(iul1,iul2,iuls,clna1,ilong1)
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
subroutine lfadiffr(kul1,kul2,kuls,cdna,klong)
! --------------------------------------------------------------
! **** *lfaDIFFR* Différence de deux articles réels.
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
integer(kind=jpintusr) kul1,kul2,kuls,klong,ilong,ierr,jlong,ilna
character*(*) cdna
real(kind=jpreeusr) zreel1(klong)
real(kind=jpreeusr) zreel2(klong)
!
! Lecture de l'article cdna sur le fichier 1.
!
call lfalecr(kul1,cdna,klong,zreel1,ilong,ierr)
!
! Lecture de l'article cdna sur le fichier 2;
! on est sûr que cet article existe dans le fichier 1,
! puisqu'on a tiré le nom d'article précisément de ce fichier!...
! Mais il n'est pas certain qu'il soit dans le fichier 2,
! c'est ce qu'on va vérifier ci-après.
!
call lfaerf(kul2,.false.)
call lfalecr(kul2,cdna,klong,zreel2,ilong,ierr)
if(ierr == 0.and.ilong == klong) then
	!
	! Les longueurs sont égales.
	! On effectue la différence.
	!
	do jlong=1,klong
		zreel1(jlong)=zreel2(jlong)-zreel1(jlong)
	enddo
	!
	! Ecriture de la différence sur le fichier de sortie.
	!
	call lfaecrr(kuls,cdna,zreel1,ilong)
else
	!
	! L'article n'existe pas dans le fichier 2.
	! Rien à faire.
	!
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna),' absent du fichier de sortie.'
endif
end
subroutine lfadiffi(kul1,kul2,kuls,cdna,klong)
! --------------------------------------------------------------
! **** *lfaDIFFI* Différence de deux articles entiers.
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
integer(kind=jpintusr) kul1,kul2,kuls,klong,ilong,ierr,jlong,ilna
character*(*) cdna
integer(kind=jpintusr) ient1(klong)
integer(kind=jpintusr) ient2(klong)
!
! Lecture de l'article cdna sur le fichier 1.
!
call lfaleci(kul1,cdna,klong,ient1,ilong,ierr)
!
! Lecture de l'article cdna sur le fichier 2;
! on est sûr que cet article existe dans le fichier 1,
! puisqu'on a tiré le nom d'article précisément de ce fichier!...
! Mais il n'est pas certain qu'il soit dans le fichier 2,
! c'est ce qu'on va vérifier ci-après.
!
call lfaerf(kul2,.false.)
call lfaleci(kul2,cdna,klong,ient2,ilong,ierr)
if(ierr == 0.and.ilong == klong) then
	!
	! Les longueurs sont égales.
	! On effectue la différence.
	!
	do jlong=1,klong
		ient1(jlong)=ient2(jlong)-ient1(jlong)
	enddo
	!
	! Ecriture de la différence sur le fichier de sortie.
	!
	call lfaecri(kuls,cdna,ient1,ilong)
else
	!
	! L'article n'existe pas dans le fichier 2.
	! Rien à faire.
	!
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna),' absent du fichier de sortie.'
endif
end
