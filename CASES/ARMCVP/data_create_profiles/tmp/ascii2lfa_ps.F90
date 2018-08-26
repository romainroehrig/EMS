program lit
! --------------------------------------------------------------
! **** ** .
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
integer, parameter :: jpniv=18
integer, parameter :: jppdt=17
real zree_niv(jpniv)
real zree_pdt(jppdt)
real zree(jppdt,jpniv)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
read *,clin,clout
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
open(22,file=clin,form='formatted')
!
!-------------------------------------------------
! Ouverture du fichier LFA.
!-------------------------------------------------
!
iul1=23
call lfaouv(iul1,clout,'W')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Nb de périodes de temps.
!-------------------------------------------------
!
read(22,fmt='(a)') clc
read(22,fmt='(a)') clc
read(22,fmt='(a)') clc
read(22,fmt='(a)') clc
do j=1,35
	read(22,fmt='(a)') clc
	read(22,fmt='(5e15.7)') (zree_pdt(jree),jree=1,jppdt)
	if(clc(1:len_trim(clc)) == 'Area Mean Ps(mb)') then
		!
		!-------------------------------------------------
		! On va écrire sur fichier un tableau de ps
		! surdimensionné: (temps, niveaux)
		! alors qu'il n'y a qu'un seul niveau pour la ps,
		! ce à seule fin d'écrire un tableau de même taille
		! que tous les autres (u, v, T, ...)
		! et ainsi de bénéficier du même traitement 
		! quant à l'interpolation ultérieure.
		!-------------------------------------------------
		!
		do jlev=1,jpniv
			do jpdt=1,jppdt
				zree(jpdt,jlev)=zree_pdt(jpdt)
			enddo
		enddo
		iprod=jppdt*jpniv
		call lfaecrr(iul1,clc,zree,iprod)
	endif
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(22)
end
