program pp
call lfapplfp2lfa
end
subroutine lfapplfp2lfa
! --------------------------------------------------------------
! **** *lfapplfp2lfa* Conversion LFP > LFA.
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
integer(kind=jpintusr) iule,iuls,ilong,ierr,igol1,inarg
character*200 clfe,clfs,clna
character*2 cltype
character*3 cllang
!
! Saisie de la ligne de commande.
!
inarg=1
call getargp(inarg,clfe)
inarg=2
call getargp(inarg,clfs)
if(clfs == ' ') then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Conversion d''un fichier LFP en un fichier LFA.'
		print*,' '
		print*,'Utilisation: lfp2lfa nom_fic_entree nom_fic_sortie'
		print*,' '
		stop
	else
		print*,' '
		print*,'Convert a LFP file into a LFA one.'
		print*,' '
		print*,'Usage: lfp2lfa FILE_IN FILE_OUT'
		print*,' '
		stop
	endif
endif
!
! Ouverture des fichiers.
!
iule=72
iuls=73
igol1=1
call lfpouv(iule,clfe,igol1)
call lfaouv(iuls,clfs,'W')
  100 continue
!
! Renseignements sur l'article suivant du fichier.
!
clna=' '
call lfpcas(iule,clna,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! Copie de l'article clnom du fichier iule
	! au fichier iuls.
	!
	if(cltype(1:1) == 'R') then
		!
		! Article de type r�el.
		!
		call lfacopr(iule,clna,clna,ilong,iuls)
	elseif(cltype(1:1) == 'I') then
		!
		! Article de type entier.
		!
		call lfacopi(iule,clna,clna,ilong,iuls)
	elseif(cltype(1:1) == 'C') then
		!
		! Article de type caract�re.
		!
		call lfacopc(iule,clna,clna,ilong,iuls)
	else
		print*,'cop/ATTENTION: type de donn�e inconnu!...'
		print*,cltype
	endif
	goto 100
endif
!
! Fermeture des fichiers.
!
call lfpfer(iule)
call lfafer(iuls)
end
subroutine lfacopc(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfacopc* Copie d'un article caract�res d'un fichier LFP � un autre.
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
! kule unit� logique du fichier LFP d'entr�e.
! cdnae nom de l'article � lire.
! cdnas nom sous lequel l'article est recopi�.
! klong longeur de l'article � copier.
! kuls unit� logique du fichier LFP de sortie.
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
call lfplecc(kule,cdnae,klong,clbouc,ilong,ierr)
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
subroutine lfacopi(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfacopi* Copie d'un article entier d'un fichier LFP � un autre.
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
! kule unit� logique du fichier LFP d'entr�e.
! cdnae nom de l'article � lire.
! cdnas nom sous lequel l'article est recopi�.
! klong longeur de l'article � copier.
! kuls unit� logique du fichier LFP de sortie.
! En sortie:
! Le fichier d'unit� logique kuls est augment� d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
#include"lfadoc.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr,iprec
character*(*) cdnae, cdnas
character*200 clnas
integer(kind=jpintusr) ibouc(klong)
!
! Lecture de l'article d'entr�e.
!
call lfpleci(kule,cdnae,klong,ibouc,ilong,ierr)
!
! Nom de l'article de sortie.
!
if(cdnas == ' ') then
	clnas=cdnae
else
	clnas=cdnas
endif
!
! Choix de la precision de sortie, en fonction
! de celle lue sur le LFP d'entree.
!
if(cgdoc == 'I4-') then
	iprec=4
elseif(cgdoc == 'I8-') then
	iprec=8
else
	iprec=jpintdef
endif
call lfapreci(kuls,iprec)
!
! Ecriture sur le fichier de sortie.
!
call lfaecri(kuls,clnas,ibouc,ilong)
end
subroutine lfacopr(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfacopr* Copie d'un article r�el d'un fichier LFP � un autre.
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
! kule unit� logique du fichier LFP d'entr�e.
! cdnae nom de l'article � lire.
! cdnas nom sous lequel l'article est recopi�.
! klong longeur de l'article � copier.
! kuls unit� logique du fichier LFP de sortie.
! En sortie:
! Le fichier d'unit� logique kuls est augment� d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
#include"lfadoc.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr,iprec
character*(*) cdnae, cdnas
character*200 clnas
real(kind=jpreeusr) zbouc(klong)
!
! Lecture de l'article d'entr�e.
!
call lfplecr(kule,cdnae,klong,zbouc,ilong,ierr)
!
! Nom de l'article de sortie.
!
if(cdnas == ' ') then
	clnas=cdnae
else
	clnas=cdnas
endif
!
! Choix de la precision de sortie, en fonction
! de celle lue sur le LFP d'entree.
!
if(cgdoc == 'R4-') then
	iprec=4
elseif(cgdoc == 'R8-') then
	iprec=8
elseif(cgdoc == 'RG-') then
	iprec=16
else
	iprec=4
endif
call lfaprecr(kuls,iprec)
!
! Ecriture sur le fichier de sortie.
!
call lfaecrr(kuls,clnas,zbouc,ilong)
end
