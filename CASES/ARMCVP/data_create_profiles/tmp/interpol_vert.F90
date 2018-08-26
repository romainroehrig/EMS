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
integer, parameter :: jppdt=17
integer, parameter :: jppdt_high_res=97
integer, parameter :: jpniv=18
integer, parameter :: jpprod=jppdt*jpniv
real zts(jppdt_high_res)
real zps_surdim(jppdt,jpniv) ! à jpdt donné, la valeur est la même quel que soit jniv.
real, allocatable :: za(:)
real, allocatable :: zb(:)
real, allocatable :: zpress2(:)
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
read *,clfniv,clin,clout
!
!-------------------------------------------------
! Lecture des coordonnées A et B.
!-------------------------------------------------
!
iul1=23
call lfaouv(iul1,clfniv,'R')
call lfaleci(iul1,'KLEV',1,ilev,ilong,ierr)
write(*,fmt=*) ilev,' niveaux verticaux dans le fichier ',clfniv(1:len_trim(clfniv))
ilev1=ilev+1
allocate(za(ilev1))
allocate(zb(ilev1))
allocate(zpress2(ilev1))
call lfalecr(iul1,'VAH',ilev1,za,ilong,ierr)
call lfalecr(iul1,'VBH',ilev1,zb,ilong,ierr)
call lfafer(iul1)
!
!-------------------------------------------------
! Allocation des tableaux de variables en entrée et sortie.
!-------------------------------------------------
!
allocate(zin(jppdt,jpniv))
allocate(zout(jppdt,ilev))
allocate(zcoo1(jpniv))
allocate(zval1(jpniv))
allocate(zcoo2(ilev))
allocate(zval2(ilev))
iprod=jpniv*jppdt
!
!-------------------------------------------------
! Ouverture du fichier LFA d'entrée.
!-------------------------------------------------
!
iullfa1=23
call lfaouv(iullfa1,clin,'R')
call lfalecr(iullfa1,'Pressure Levels p(np) mb :',jpniv,zcoo1,ilong,ierr)
do jlev=1,jpniv/2
	call swap(zcoo1(jlev),zcoo1(jpniv-jlev+1))
enddo
call lfalecr(iullfa1,'Area Mean Ps(mb)',jpprod,zps_surdim,ilong,ierr)
write(*,fmt='(a,/,100(5g16.7,/))') 'Pression de surface = ',(zps_surdim(jpdt,jpniv),jpdt=1,jppdt)
call lfalecr(iullfa1,'Ts',jppdt_high_res,zts,ilong,ierr)
!
!-------------------------------------------------
! Ouverture du fichier LFA de sortie.
!-------------------------------------------------
!
iullfa2=24
call lfaouv(iullfa2,clout,'W')
call lfaecri(iullfa2,'NLEV',ilev,1)
call lfaecri(iullfa2,'NTIME',jppdt,1)
call lfaecrr(iullfa2,'VAH',za,ilev1)
call lfaecrr(iullfa2,'VBH',zb,ilev1)
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
call lfarew(iullfa1)
ierr=0
do while(ierr == 0)
	clna=' '
	call lfacas(iullfa1,clna,cltype,ilong,ierr)
	write(*,fmt=*) clna(1:len_trim(clna))
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
			do jpdt=1,jppdt
				!
				!-------------------------------------------------
				! Initialisation de la coordonnée verticale
				! et de la valeur de la grille d'entrée.
				!-------------------------------------------------
				!
				do jlev=1,jpniv
					zval1(jlev)=zin(jpdt,jpniv-jlev+1)
				enddo
				!
				!-------------------------------------------------
				! Initialisation de la coordonnée verticale de la grille de sortie:
				! pression en hPa.
				!-------------------------------------------------
				!
				do jlev=1,ilev+1
					zpress2(jlev)=za(jlev)+zb(jlev)*zps_surdim(jpdt,jpniv)*100.
				enddo
				do jlev=1,ilev
					zcoo2(jlev)=0.5*(zpress2(jlev)+zpress2(jlev+1))/100.
				enddo
				!
				!-------------------------------------------------
				! Interpolation.
				!-------------------------------------------------
				!
				if(jpdt == 1) then
					write(*,fmt=*) 'Interpolate ',clna(1:len_trim(clna))
				endif
				call interpol1d(jpniv,zcoo1,zval1,ilev,zcoo2,zval2)
				!
				!-------------------------------------------------
				! Ecriture du tableau 1D interpolé sur le tableau 2D (temps, verticale).
				!-------------------------------------------------
				!
				do jlev=1,ilev
					zout(jpdt,jlev)=zval2(jlev)
				enddo
			enddo
			!
			!-------------------------------------------------
			! Ecriture d'un article (temps, verticale) sur le fichier LFA de sortie.
			!-------------------------------------------------
			!
			ilong=ilev*jppdt
			call lfaecrr(iullfa2,clna,zout,ilong)
		endif
	endif
enddo
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
#include"swap.F90"
