subroutine interpol1d(klev1,pcoo1,pval1,klev2,pcoo2,pval2)
! --------------------------------------------------------------
! **** ** Interpolation d'un tableau 1D sur un autre.
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
!	klev1: nb de niveaux en entrée.
!	pcoo1:coordonnée irrégulière du tableau 1 d'entrée.
!	pval1: valeurs du tableau 1 d'entrée.
!	klev2: nb de niveaux en sortie.
!	pcoo2:coordonnée irrégulière du tableau 2 de sortie.
! En sortie:
!	pval2: valeurs du tableau 2 de sortie.
! --------------------------------------------------------------
! Les coordonnées peuvent être croissantes ou décroissantes.
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
real pcoo1(klev1)
real zcoo1(klev1)
real pval1(klev1)
real zval1(klev1)
real pcoo2(klev2)
real zcoo2(klev2)
real pval2(klev2)
!
!-------------------------------------------------
! Test de croissance ou décroissance de la coordonnée.
!-------------------------------------------------
!
if(pcoo1(1) > pcoo1(klev1)) then
	!
	!-------------------------------------------------
	! On permute tout.
	!-------------------------------------------------
	!
	do jlev=1,klev1
		zcoo1(jlev)=pcoo1(klev1-jlev+1) 
		zval1(jlev)=pval1(klev1-jlev+1)
	enddo
else
	!
	!-------------------------------------------------
	! Rien à permuter.
	!-------------------------------------------------
	!
	zcoo1=pcoo1
	zval1=pval1
endif
if(pcoo2(1) > pcoo2(klev2)) then
	!
	!-------------------------------------------------
	! On permute tout.
	!-------------------------------------------------
	!
	do jlev=1,klev2
		zcoo2(jlev)=pcoo2(klev2-jlev+1) 
	enddo
else
	!
	!-------------------------------------------------
	! Rien à permuter.
	!-------------------------------------------------
	!
	zcoo2=pcoo2
endif
!
!-------------------------------------------------
! Boucle sur les valeurs du tableau de sortie.
!-------------------------------------------------
!
do jlev=1,klev2
	zcoo=zcoo2(jlev)
	if(zcoo <= zcoo1(1)) then
		!
		!-------------------------------------------------
		! Echantillonnage.
		!-------------------------------------------------
		!
		!pval2(jlev)=zval1(1)
		!
		!-------------------------------------------------
		! Extrapolation.
		!-------------------------------------------------
		!
		pval2(jlev)=zval1(1)+(zval1(1+1)-zval1(1))*(zcoo-zcoo1(1))/(zcoo1(1+1)-zcoo1(1))
	elseif(zcoo >= zcoo1(klev1)) then
		!
		!-------------------------------------------------
		! Echantillonnage.
		!-------------------------------------------------
		!
		!pval2(jlev)=zval1(klev1)
		!
		!-------------------------------------------------
		! Extrapolation.
		!-------------------------------------------------
		!
		pval2(jlev)=zval1(klev1-1)+(zval1(klev1-1+1)-zval1(klev1-1))*(zcoo-zcoo1(klev1-1))/(zcoo1(klev1-1+1)-zcoo1(klev1-1))
	else
		llok=.false.
		do jlev1=1,klev1-1
			if(zcoo >= zcoo1(jlev1) .and. zcoo <= zcoo1(jlev1+1)) then
				llok=.true.
				pval2(jlev)=zval1(jlev1)+(zval1(jlev1+1)-zval1(jlev1))*(zcoo-zcoo1(jlev1))/(zcoo1(jlev1+1)-zcoo1(jlev1))
			endif
		enddo
		if(.not.llok) then
			write(*,fmt=*) 'interpol1d/ERREUR: lors de l''interpolation d''un tableau de taille ',klev1,' sur un tableau de taille ',klev2
			write(*,fmt=*) '	aucune solution n''a pu être trouvée!...'
			write(*,fmt=*) '	Erreur probabble de coordonnées d''entrée.'
			stop 'call abort'
		endif
	endif
enddo
!
!-------------------------------------------------
! Test de croissance ou décroissance de la coordonnée.
!-------------------------------------------------
!
if(pcoo2(1) > pcoo2(klev2)) then
	!
	!-------------------------------------------------
	! On permute tout.
	!-------------------------------------------------
	!
	do jlev=1,klev2/2
		call permute(pval2(jlev),pval2(klev2-jlev+1))
	enddo
endif
end
subroutine permute(px1,px2)
zx=px1
px1=px2
px2=zx
end
