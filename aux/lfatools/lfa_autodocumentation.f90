program autodoc
! --------------------------------------------------------------
! **** *AUTODOC* Autodocumentation d'un fichier LFA.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) iarg
integer(kind=jpintesb) iversion,jpnoma
parameter(jpnoma=80) ! taille majorant celle du nom d'article le plus long.
integer(kind=jpintesb) itype,ilong,jlong,imin,imax,iart &
& ,iascii(jpnoma),jna,ilna
character*200 clfic,clna
character*200 classign
character*3 cllang
! call lfa
!
!-------------------------------------------------
! Ligne de commande.
!-------------------------------------------------
!
iarg=1
call getargp(iarg,clfic)
if(clfic == ' ') then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Lecture brute de l''autodocumentation d''un fichier LFA.'
		print*,' '
		print*,'Utilisation: lfa_autodocumentation FIC_LFA'
		print*,' '
	else
		print*,' '
		print*,'Prints out LFA file autodocumentation, in rough form.'
		print*,' '
		print*,'Usage: lfa_autodocumentation LFA_FILE'
		print*,' '
	endif
	stop
endif
#ifdef cray
!
! Dans le cas du Cray, il faut executer un ordre assign:
! en fortran, tous les read transductent alors du format IEEE
! vers CRAY, et les write dans l'autre sens.
!
write(classign,fmt='(2a)') 'assign -N ieee -F f77 f:',clfic(1:len_trim(clfic))
call assign(classign)
#endif
!
!-------------------------------------------------
! Ouverture du LFA.
!-------------------------------------------------
!
open(1,file=clfic,form='unformatted')
!
!-------------------------------------------------
! Version du logiciel ayant écrit le fichier.
!-------------------------------------------------
!
read(1) iversion
print*,'--------------------------------------------'
print*,'Version=',iversion
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
read(1)
!
! -------------------------------------------------
! On determine le nom en clair de l'article.
! -------------------------------------------------
!
clna=' '
do jna=1,ilna
	clna(jna:jna)=char(iascii(jna))
enddo
print*,'--------------------------------------------'
if(cllang() == 'FRA') then
	print*,'type       =',itype
	print*,'longueur   =',ilong
	print*,'nom        =',clna(1:ilna)
else
	print*,'type       =',itype
	print*,'length     =',ilong
	print*,'name       =',clna(1:ilna)
endif
goto 100
  200 continue
print*,'--------------------------------------------'
!
! -------------------------------------------------
! Fermeture du fichier d'entree.
! -------------------------------------------------
!
close(1)
#ifdef cray
call lfaiunassign(1)
#endif
end
