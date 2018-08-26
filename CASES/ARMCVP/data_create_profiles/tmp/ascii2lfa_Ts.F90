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
integer, parameter :: jppdt=97
real zree_pdt(jppdt)
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
do jpdt=1,jppdt
	read(22,fmt=*) zgol,zgol,zgol,zree_pdt(jpdt)
enddo
clc='Ts'
call lfaecrr(iul1,clc,zree_pdt,jppdt)
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(22)
end
