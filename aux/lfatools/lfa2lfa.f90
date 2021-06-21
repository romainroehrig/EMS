program pp
call lfapplfa2lfa
end
subroutine lfapplfa2lfa
! --------------------------------------------------------------
! **** *lfa2lfa* Conversion LFA > lfa.
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
integer(kind=jpintusr) iprecr,ipreci,iarg,jarg,ilarg,iule,iuls &
& ,ilong,ierr
integer(kind=4) iargc
character*200 clfe,clfs,clarg,clna
character*2 cltype
character*3 cllang
!
! -------------------------------------------------
! Initialisation par d�faut.
! -------------------------------------------------
!
iprecr=4 ! pr�cision des r�els � �crire sur le lfa.
ipreci=4 ! pr�cision des entiers � �crire sur le lfa.
clfe=' '
clfs=' '
!
! -------------------------------------------------
! Saisie de la ligne de commande.
! -------------------------------------------------
!
iarg=iargc() ! nombre d'arguments.
if(iarg < 2.or.iarg > 4) then
	!
	! -------------------------------------------------
	! Le nombre d'arguments n'est pas celui requis
	! pour ex�cuter. On fournit la documentation.
	! -------------------------------------------------
	!
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Conversion d''un fichier LFA en un autre fichier LFA,'
		print*,'en for�ant la pr�cision des r�els et des entiers.'
		print*,' '
		print*,'Utilisation: lfa2lfa [-i] [-r] nom_fic_entree' &
& 		,' nom_fic_sortie'
		print*,' '
		print*,'avec'
		print*, &
& 		'	-i8 si on veut en sortie des entiers sur 8 octets.'
		print*, &
& 		'	-i4 si on veut en sortie des entiers sur 4 octets.'
		print*,'	    d�faut: ',ipreci
		print*, &
& 		'	-r8 si on veut en sortie des r�els   sur 8 octets.'
		print*, &
& 		'	-r4 si on veut en sortie des r�els   sur 4 octets.'
		print*,'	    d�faut: ',iprecr
		print*,'	'
		print*,'Exemple:'
		print*,'	lfa2lfa -r8 -i4 LFA LFARES'
		print*,' '
		stop
	else
		print*,' '
		print*,'Convert a LFA file into another LFA file,'
		print*,'while forcing real and integer precision.'
		print*,' '
		print*,'Usage: lfa2lfa [-i] [-r] FILE_IN FILE_OUT'
		print*,' '
		print*,'with'
		print*, &
& 		'	-i8 for 8 bytes integers in output.'
		print*, &
& 		'	-i4 for 4 bytes integers in output.'
		print*,'	    default: ',ipreci
		print*, &
& 		'	-r8 for 8 bytes real in output.'
		print*, &
& 		'	-r4 si on veut en sortie des r�els   sur 4 octets.'
		print*,'	    default: ',iprecr
		print*,'	'
		print*,'Example:'
		print*,'	lfa2lfa -r8 -i4 LFA LFARES'
		print*,' '
		stop
	endif
else
	!
	! -------------------------------------------------
	! Nombre d'arguments OK.
	! -------------------------------------------------
	!
	do jarg=1,iarg
		call getargp(jarg,clarg)
		ilarg=len_trim(clarg)
		if(clarg(1:3) == '-r8') then
			!
			! -------------------------------------------------
			! Fourniture de la pr�cision.
			! -------------------------------------------------
			!
			iprecr=8
		elseif(clarg(1:3) == '-r4') then
			!
			! -------------------------------------------------
			! Fourniture de la pr�cision.
			! -------------------------------------------------
			!
			iprecr=4
		elseif(clarg(1:3) == '-i8') then
			!
			! -------------------------------------------------
			! Fourniture de la pr�cision.
			! -------------------------------------------------
			!
			ipreci=8
		elseif(clarg(1:3) == '-i4') then
			!
			! -------------------------------------------------
			! Fourniture de la pr�cision.
			! -------------------------------------------------
			!
			ipreci=4
		elseif(clfe == ' ') then
			!
			! -------------------------------------------------
			! Saisie du nom du fichier d'entr�e.
			! -------------------------------------------------
			!
			clfe=clarg
		else
			!
			! -------------------------------------------------
			! Saisie du nom du fichier de sortie.
			! -------------------------------------------------
			!
			clfs=clarg
		endif
	enddo
endif
!
! -------------------------------------------------
! Ouverture des fichiers.
! -------------------------------------------------
!
iule=72
iuls=73
call lfaouv(iule,clfe,'R')
call lfaouv(iuls,clfs,'W')
!
! -------------------------------------------------
! Pr�cision.
! -------------------------------------------------
!
call lfaprecr(iuls,iprecr)
call lfapreci(iuls,ipreci)
  100 continue
!
! Renseignements sur l'article suivant du fichier.
!
clna=' '
call lfacas(iule,clna,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! Copie de l'article clnom du fichier iule
	! au fichier iuls.
	!
	if(cltype(1:1) == 'R') then
		!
		! Article de type r�el.
		!
		call lfa2lfacopr(iule,clna,clna,ilong,iuls)
	elseif(cltype(1:1) == 'I') then
		!
		! Article de type entier.
		!
		call lfa2lfacopi(iule,clna,clna,ilong,iuls)
	elseif(cltype(1:1) == 'C') then
		!
		! Article de type caract�re.
		!
		call lfa2lfacopc(iule,clna,clna,ilong,iuls)
	else
		print*,'lfa2lfacop/ATTENTION: type de donn�e inconnu!...'
		print*,cltype
	endif
	goto 100
endif
!
! Fermeture des fichiers.
!
call lfafer(iule)
call lfafer(iuls)
end
subroutine lfa2lfacopc(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfa2lfacopC* Copie d'un article caract�res d'un fichier lfa � un autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kule unit� logique du fichier lfa d'entr�e.
! cdnae nom de l'article � lire.
! cdnas nom sous lequel l'article est recopi�.
! klong longeur de l'article � copier.
! kuls unit� logique du fichier lfa de sortie.
! En sortie:
! Le fichier d'unit� logique kuls est augment� d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*200 clnas
character*400 clbouc(klong)
!
! Lecture de l'article d'entr�e.
!
call lfalecc(kule,cdnae,klong,clbouc,ilong,ierr)
!
! Nom de l'article de sortie.
!
if(cdnas == ' ') then
	clnas=cdnae
else
	clnas=cdnas
endif
!
! Ecriture sur le fichier de sortie.
!
call lfaecrc(kuls,clnas,clbouc,ilong)
end
subroutine lfa2lfacopi(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfa2lfacopI* Copie d'un article entier d'un fichier lfa � un autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kule unit� logique du fichier lfa d'entr�e.
! cdnae nom de l'article � lire.
! cdnas nom sous lequel l'article est recopi�.
! klong longeur de l'article � copier.
! kuls unit� logique du fichier lfa de sortie.
! En sortie:
! Le fichier d'unit� logique kuls est augment� d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*200 clnas
integer(kind=jpintusr) ibouc(klong)
!
! Lecture de l'article d'entr�e.
!
call lfaleci(kule,cdnae,klong,ibouc,ilong,ierr)
!
! Nom de l'article de sortie.
!
if(cdnas == ' ') then
	clnas=cdnae
else
	clnas=cdnas
endif
!
! Ecriture sur le fichier de sortie.
!
call lfaecri(kuls,clnas,ibouc,ilong)
end
subroutine lfa2lfacopr(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfa2lfacopR* Copie d'un article r�el d'un fichier lfa � un autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kule unit� logique du fichier lfa d'entr�e.
! cdnae nom de l'article � lire.
! cdnas nom sous lequel l'article est recopi�.
! klong longeur de l'article � copier.
! kuls unit� logique du fichier lfa de sortie.
! En sortie:
! Le fichier d'unit� logique kuls est augment� d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*200 clnas
real(kind=jpreeusr) zbouc(klong)
!
! Lecture de l'article d'entr�e.
!
call lfalecr(kule,cdnae,klong,zbouc,ilong,ierr)
!
! Nom de l'article de sortie.
!
if(cdnas == ' ') then
	clnas=cdnae
else
	clnas=cdnas
endif
!
! Ecriture sur le fichier de sortie.
!
call lfaecrr(kuls,clnas,zbouc,ilong)
end
!
