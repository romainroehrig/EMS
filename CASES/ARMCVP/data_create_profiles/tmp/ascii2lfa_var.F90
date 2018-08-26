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
read(22,fmt=*) zpdt
ipdt=nint(zpdt)
if(ipdt /= jppdt) then
	write(*,fmt=*) 'ascii2lfa_var/ERREUR: nb de pdt non attendu!...'
	stop 'call abort'
else
endif
call lfaecri(iul1,'N_TIME',ipdt,1)
!
!-------------------------------------------------
! Nb de niveaux.
!-------------------------------------------------
!
read(22,fmt='(a)') clc
read(22,fmt=*) zniv
iniv=nint(zniv)
if(iniv /= jpniv) then
	write(*,fmt=*) 'ascii2lfa_var/ERREUR: nb de niveaux non attendu!...'
	stop 'call abort'
else
endif
call lfaecri(iul1,'N_LEV',iniv,1)
!
!-------------------------------------------------
! Lecture de champs verticaux.
!-------------------------------------------------
!
do jnb=1,1
	read(22,fmt='(a)') clc
	do jlig=1,3
		read(22,fmt=*) (zree_niv(jree),jree=jlig*5-4,jlig*5-4+4)
	enddo
	read(22,fmt=*) (zree_niv(jree),jree=16,jpniv)
	write(*,fmt=*) clc(1:len_trim(clc)),' : ',zree_niv
	call lfaecrr(iul1,clc,zree_niv,jpniv)
enddo
!
!-------------------------------------------------
! Lecture de champs temporels.
!-------------------------------------------------
!
do jnb=1,6
	read(22,fmt='(a)') clc
	do jlig=1,3
		read(22,fmt=*) (zree_pdt(jree),jree=jlig*5-4,jlig*5-4+4)
	enddo
	read(22,fmt=*) (zree_pdt(jree),jree=16,jppdt)
	write(*,fmt=*) clc(1:len_trim(clc)),' : ',zree_pdt
	call lfaecrr(iul1,clc,zree_pdt,jppdt)
enddo
!
!-------------------------------------------------
! Nb de champs.
!-------------------------------------------------
!
read(22,fmt='(a)') clc
read(22,fmt=*) inbc
!
!-------------------------------------------------
! Boucke sur les champs: T, qv, etc...
!-------------------------------------------------
!
do jnbc=1,inbc
	read(22,fmt='(a)') clc
	!
	!-------------------------------------------------
	! Boucle sur les niveaux.
	!-------------------------------------------------
	!
	do jniv=1,jpniv
		!
		!-------------------------------------------------
		! Boucle sur les temps.
		!-------------------------------------------------
		!
		do jlig=1,3
			read(22,fmt=*) (zree(jree,jniv),jree=jlig*5-4,jlig*5-4+4)
		enddo
		read(22,fmt=*) (zree(jree,jniv),jree=16,jppdt)
		iprod=jpniv*jppdt
	enddo
	call lfaecrr(iul1,clc,zree,iprod)
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(22)
end
