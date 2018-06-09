program lec
! --------------------------------------------------------------
! **** *LEC* Programme lisant un fichier LFA.
! --------------------------------------------------------------
! Sujet:
! Lecture sequentielle d'un fichier LFA, avec affichage
! des extrema des articles reels et entiers rencontres.
!
! Le present programme permet de lire un fichier LFA
! sans avoir a installer la bibliotheque LFA.
! Mais l'utilisateur, en se reportant a la routine
! de demonstration lfappdemo de lfa.F, verra que la lecture/ecriture
! de fichiers non formattes est plus condensee et securisee
! en utilisant le logiciel LFA.
!
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
integer(kind=4) jpr,jpnoma
parameter(jpr=1000) ! taille majorant celle du plus grand article a lire dans le fichier LFA.
parameter(jpnoma=80) ! taille majorant celle du nom d'article le plus long.
real(kind=8) zreel8(jpr),zmin,zmax
real(kind=4) zreel4(jpr)
integer(kind=4) ient4(jpr),itype,ilong,jlong,imin,imax,iart &
& ,iascii(jpnoma),jna,ilna
integer(kind=8) ient8(jpr)
character*200 clfic,clna
!
! -------------------------------------------------
! Ouverture du fichier d'entree.
! -------------------------------------------------
!
clfic='LFA' ! nom du fichier d'entree.
open(1,file=clfic,form='unformatted')
!
! -------------------------------------------------
! On saute l'en-tete du fichier.
! -------------------------------------------------
!
read(1)
!
! -------------------------------------------------
! On va lire sequentiellement tout le fichier.
! -------------------------------------------------
!
iart=0 ! nombre d'articles lus sur le fichier.
  100 continue
!
! Lecture de l'autodocumentation du fichier:
! itype: type de donnee: reelle, entiere, caractere, etc...
! ilong: taille de l'article (nombre minimal d'elements du tableau le revceant).
!
read(1,end=200) itype,ilong,ilna,(iascii(jna),jna=1,ilna)
iart=iart+1
!
! -------------------------------------------------
! On determine le nom en clair de l'article.
! -------------------------------------------------
!
clna=' '
do jna=1,ilna
	clna(jna:jna)=char(iascii(jna))
enddo
!
! -------------------------------------------------
! Controle de la taille physique du tableau cible.
! -------------------------------------------------
!
if(ilong > jpr) then
	!
	! La dimension physique du tableau
	! ne lui permet pas de recevoir l'article
	! ecrit sur le fichier LFA.
	!
	print*,'LEC/ERREUR: recompiler avec une valeur' &
& 	,' plus grande de jpr!...'
	print*,'Il faut jpr => ',ilong,'!...'
	stop 'call abort'
endif
!
! -------------------------------------------------
! La taille est OK. On va lire sur le fichier.
! -------------------------------------------------
!
if(itype == 1) then
	!
	! -------------------------------------------------
	! Article de type reel 64 bits.
	! -------------------------------------------------
	!
	read(1) (zreel8(jlong),jlong=1,ilong)
	!
	! Mettre ici l'utilisation de l'article reel...
	! Par exemple, on calcule ci-dessous
	! les min et max du tableau de reels.
	!
	zmin=zreel8(1)
	zmax=zreel8(1)
	do jlong=1,ilong
		if(zreel8(jlong) < zmin) zmin=zreel8(jlong)
		if(zreel8(jlong) > zmin) zmax=zreel8(jlong)
	enddo
	print*,'Article ',iart &
& 	,', type REEL 64 bits, nom "',clna(1:ilna) &
& 	,'", longueur=',ilong &
& 	,' min=',zmin,' max=',zmax
elseif(itype == 3) then
	!
	! -------------------------------------------------
	! Article de type reel 32 bits.
	! -------------------------------------------------
	!
	read(1) (zreel4(jlong),jlong=1,ilong)
	!
	! Mettre ici l'utilisation de l'article reel...
	! Par exemple, on calcule ci-dessous
	! les min et max du tableau de reels.
	!
	zmin=zreel4(1)
	zmax=zreel4(1)
	do jlong=1,ilong
		if(zreel4(jlong) < zmin) zmin=zreel4(jlong)
		if(zreel4(jlong) > zmin) zmax=zreel4(jlong)
	enddo
	print*,'Article ',iart &
& 	,', type REEL 32 bits, nom "',clna(1:ilna) &
& 	,'", longueur=',ilong &
& 	,' min=',zmin,' max=',zmax
elseif(itype == 2) then
	!
	! -------------------------------------------------
	! Article de type entier 32 bits.
	! -------------------------------------------------
	!
	read(1) (ient4(jlong),jlong=1,ilong)
	!
	! Mettre ici l'utilisation de l'article entier...
	! Par exemple, on calcule ci-dessous
	! les min et max du tableau d'entiers.
	!
	imin=ient4(1)
	imax=ient4(1)
	do jlong=1,ilong
		if(ient4(jlong) < imin) imin=ient4(jlong)
		if(ient4(jlong) > imin) imax=ient4(jlong)
	enddo
	print*,'Article ',iart &
& 	,', type ENTIER 32 bits, nom "',clna(1:ilna) &
& 	,'", longueur=',ilong &
& 	,' min=',imin,' max=',imax
elseif(itype == 4) then
	!
	! -------------------------------------------------
	! Article de type entier 64 bits.
	! -------------------------------------------------
	!
	read(1) (ient8(jlong),jlong=1,ilong)
	!
	! Mettre ici l'utilisation de l'article entier...
	! Par exemple, on calcule ci-dessous
	! les min et max du tableau d'entiers.
	!
	imin=ient8(1)
	imax=ient8(1)
	do jlong=1,ilong
		if(ient8(jlong) < imin) imin=ient8(jlong)
		if(ient8(jlong) > imin) imax=ient8(jlong)
	enddo
	print*,'Article ',iart &
& 	,', type ENTIER 64 bits, nom "',clna(1:ilna) &
& 	,'", longueur=',ilong &
& 	,' min=',imin,' max=',imax
elseif(itype < 0) then
	!
	! -------------------------------------------------
	! Article de type caractere.
	! On saute au suivant.
	! -------------------------------------------------
	!
	read(1)
	print*,'Article ',iart &
& 	,', type CARACTERE, nom "',clna(1:ilna) &
& 	,'", longueur=',ilong
else
	print*,'lec/ERREUR: type de donnee inconnu!...'
	print*,itype
	stop 'call abort'
endif
goto 100
  200 continue
!
! -------------------------------------------------
! Fermeture du fichier d'entree.
! -------------------------------------------------
!
close(1)
end
