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
real, allocatable :: zin(:,:)
real, allocatable :: zout(:,:)
real, allocatable :: zcoo1(:)
real, allocatable :: zval1(:)
real, allocatable :: zcoo2(:)
real, allocatable :: zval2(:)
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
! Lecture des coordonnées A et B.
!-------------------------------------------------
!
iullfa1=23
call lfaouv(iullfa1,clin,'R')
call lfaleci(iullfa1,'NLEV',1,ilev,ilong,ierr)
call lfaleci(iullfa1,'NTIME',1,itime1,ilong,ierr)
iprod=ilev*itime1
!
!-------------------------------------------------
! Allocation des tableaux de variables en entrée et sortie.
!-------------------------------------------------
!
allocate(zin(itime1,ilev))
allocate(zcoo1(itime1))
allocate(zval1(itime1))
itime2=97
allocate(zout(itime2,ilev))
allocate(zcoo2(itime2))
allocate(zval2(itime2))
!
!-------------------------------------------------
! Ouverture du fichier LFA de sortie.
!-------------------------------------------------
!
iullfa2=24
call lfaouv(iullfa2,clout,'W')
call lfaecri(iullfa2,'NLEV',ilev,1)
call lfaecri(iullfa2,'NTIME',itime2,1)
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
ierr=0
do while(ierr == 0)
	clna=' '
	call lfacas(iullfa1,clna,cltype,ilong,ierr)
	if(ierr == 0) then
		!
		!-------------------------------------------------
		! On n'est pas en fin de fichier.
		!-------------------------------------------------
		!
		if(ilong /= iprod) then
			!
			!-------------------------------------------------
			! L'article n'est pas de type champ 2D temps-verticale.
			!-------------------------------------------------
			!
			call lfaavan(iullfa1)
		else
			!
			!-------------------------------------------------
			! L'article est de type champ 2D temps-verticale.
			! On l'interpole.
			!-------------------------------------------------
			!
			call lfalecr(iullfa1,clna,iprod,zin,ilong,ierr)
			do jlev=1,ilev
				!
				!-------------------------------------------------
				! Initialisation de la coordonnée verticale
				! et de la valeur de la grille d'entrée.
				!-------------------------------------------------
				!
				do jtime=1,itime1
					zval1(jtime)=zin(jtime,jlev)
					zcoo1(jtime)=real(jtime-1)/real(itime1-1)
				enddo
				!
				!-------------------------------------------------
				! Initialisation de la coordonnée verticale de la grille de sortie:
				! pression en hPa.
				!-------------------------------------------------
				!
				do jtime=1,itime2
					zcoo2(jtime)=real(jtime-1)/real(itime2-1)
				enddo
				!
				!-------------------------------------------------
				! Interpolation.
				!-------------------------------------------------
				!
				if(jlev == 1) then
					write(*,fmt=*) 'Interpolate ',clna(1:len_trim(clna))
				endif
				call interpol1d(itime1,zcoo1,zval1,itime2,zcoo2,zval2)
				!
				!-------------------------------------------------
				! Ecriture du tableau 1D interpolé sur le tableau 2D (temps, verticale).
				!-------------------------------------------------
				!
				do jtime=1,itime2
					zout(jtime,jlev)=zval2(jtime)
				enddo
			enddo
			!
			!-------------------------------------------------
			! Ecriture d'un article (temps, verticale) sur le fichier LFA de sortie.
			!-------------------------------------------------
			!
			ilong=ilev*itime2
			call lfaecrr(iullfa2,clna,zout,ilong)
		endif
	endif
enddo
call lfacop(iullfa1,'VAH','VAH',iullfa2)
call lfacop(iullfa1,'VBH','VBH',iullfa2)
!
!-------------------------------------------------
! Fermeture du fichier LFA.
!-------------------------------------------------
!
call lfafer(iullfa2)
!
!-------------------------------------------------
! Fermeture du fichier LFA.
!-------------------------------------------------
!
call lfafer(iullfa1)
end
#include"interpol1d.F90"
