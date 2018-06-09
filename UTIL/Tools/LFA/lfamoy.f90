program lfamoy
! --------------------------------------------------------------
! **** *lfamoy* Moyenne de n fichiers LFA.
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
integer(kind=jpintusr) iarg,iul1,iul,iuls,ilong1,ierr1,ilong2 &
& 	,ierr2,ilna1,jlong,jarg,ilong,ierr,inarg
integer(kind=4) iargc
character*200 clfe,clfs,clna1,cltype1,cltype2
character*3 cllang
integer, parameter :: jpx=1000 ! entier majorant le plus grand article des LFA d'entrée.
real(kind=jpreeusr) zmoy(jpx)
real(kind=jpreeusr) zadd(jpx)
real(kind=jpreeusr) zc
integer(kind=jpintusr) imoy(jpx)
integer(kind=jpintusr) iadd(jpx)
!
! Saisie de la ligne de commande.
!
iarg=iargc() ! nombre d'arguments.
if(iarg < 3) then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Moyenne de n fichiers LFA.'
		print*,' '
		print*,'Utilisation: lfamoy FMOY F1 F2 [F3 ... Fn]'
		print*,'avec'
		print*,'	F1 F2 [F3 ... Fn] les n fichiers d''entrée.'
		print*,'	FMOY le fichier de sortie, recevant la moyenne.'
		print*,' '
		print*,'Remarque: la moyenne est opérée sur les articles'
		print*,'communs aux n fichiers.'
		print*,' '
		stop
	else
		print*,' '
		print*,'Mean of n LFA files.'
		print*,' '
		print*,'Usage: lfamoy FMEA F1 F2 [F3 ... Fn]'
		print*,'with'
		print*,'	F1 F2 [F3 ... Fn] the n input files.'
		print*,'	FMEA the output file, receiving mean value.'
		print*,' '
		print*,'Nota: the mean is performed on articles'
		print*,'present in all files.'
		print*,' '
		stop
	endif
endif
inarg=1
call getargp(inarg,clfs)
!
! Ouverture du fichier de sortie.
!
iuls=20
call lfaouv(iuls,clfs,'W')
!
! -------------------------------------------------
! Ouverture du premier fichier d'entrée.
! -------------------------------------------------
!
iul1=iuls+1 ! premier fichier d'entrée.
inarg=2
call getargp(inarg,clfe)
call lfaouv(iul1,clfe,'R')
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
ilna1=len_trim(clna1)
if(ierr1 == 0) then
	!
	! On n'est pas en fin de fichier 1.
	!
	if(ilong1 > jpx) then
		print*,'LFAMOY/ERREUR: relancer avec ' &
& 			,'une valeur plus grande de jpx!...'
		print*,'jpx=',jpx
		print*,'Or requis: ',ilong1
		stop 'call abort'
	endif
	if(cltype1(1:1) == 'I') then
		!
		! Article entier.
		!
		call lfaleci(iul1,clna1,ilong1,imoy,ilong,ierr)
	elseif(cltype1(1:1) == 'R') then
		!
		! Article réel.
		!
		call lfalecr(iul1,clna1,ilong1,zmoy,ilong,ierr)
	endif
	!
	! -------------------------------------------------
	! Boucle sur les n-1 autres fichiers d'entrée.
	! -------------------------------------------------
	!
	do jarg=3,iarg
		call getargp(jarg,clfe)
		iul=iul1+1
		call lfaouv(iul,clfe,'R')
		call lfacas(iul,clna1,cltype2,ilong2,ierr2)
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
						call lfalecr(iul,clna1,ilong1,zadd,ilong,ierr)
						do jlong=1,ilong1
							zmoy(jlong)=zmoy(jlong)+zadd(jlong)
						enddo
					elseif(cltype1(1:1) == 'I') then
						!
						! Article de type entier.
						!
						call lfaleci(iul,clna1,ilong1,iadd,ilong,ierr)
						do jlong=1,ilong1
							imoy(jlong)=imoy(jlong)+iadd(jlong)
                                                        !if (clna1 == 'NINDAT') then
                                                        !        write(*,*) imoy(jlong),iadd(jlong)
                                                        !endif
						enddo
					else
						!
						! Autres types d'article.
						! On ne fait pas la différence.
						! On avance à l'article suivant.
						!
						call lfaavan(iul1)
					endif
				else
					print*,'lfamoy/ERREUR: l''article ',clna1(1:ilna1) &
& 						,' n''a pas la meme longueur dans les deux fichiers.'
					stop 'call abort'
				endif
			else
				print*,'lfamoy/ERREUR: l''article ',clna1(1:ilna1) &
& 					,' n''a pas le meme type dans les deux fichiers.'
				stop 'call abort'
			endif
		elseif(ierr2 == -1) then
			print*,'lfamoy/ERREUR: l''article ',clna1(1:ilna1) &
& 				,' est absent du fichier 2.'
			stop 'call abort'
		else
			print*,'lfamoy/ERREUR: code reponse ',ierr2 &
& 				,' en recherche de l''article ',clna1(1:ilna1)
			stop 'call abort'
		endif
		call lfafer(iul)
	enddo
	zc=1./real(iarg-1)
        !if (clna1 == 'NINDAT') then
        !        write(*,*) iarg,zc
        !endif
	if(cltype1(1:1) == 'R') then
		!
		! Article de type réel.
		!
		do jlong=1,ilong1
			zmoy(jlong)=zmoy(jlong)*zc
		enddo
		call lfaecrr(iuls,clna1,zmoy,ilong1)
	elseif(cltype1(1:1) == 'I') then
		!
		! Article de type entier.
		!
		do jlong=1,ilong1
                        !if (clna1 == 'NINDAT') then
                        !        write(*,*) imoy(jlong),real(imoy(jlong))*zc,floor(real(imoy(jlong))*zc)
                        !endif
			imoy(jlong)=floor(real(imoy(jlong))*zc)
		enddo
		call lfaecri(iuls,clna1,imoy,ilong1)
	endif
	goto 100
endif
!
! Fermeture du fichier de sortie.
!
call lfafer(iuls)
end
