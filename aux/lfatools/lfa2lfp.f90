program pp
call lfapplfa2lfp
end
subroutine lfapplfa2lfp
! --------------------------------------------------------------
! **** *lfa2lfp* Conversion LFA > LFP.
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
integer(kind=jpintusr) iarg,jarg,ilarg,iule,iuls &
& ,ilong,ierr,igol2
integer(kind=4) iargc
character*200 clfe,clfs,clarg,clna
character*2 cltype
character*3 cllang
!
! -------------------------------------------------
! Initialisation par d�faut.
! -------------------------------------------------
!
clfe=' '
clfs=' '
!
! -------------------------------------------------
! Saisie de la ligne de commande.
! -------------------------------------------------
!
iarg=iargc() ! nombre d'arguments.
if(iarg == 0) then
	!
	! -------------------------------------------------
	! Le nombre d'arguments n'est pas celui requis
	! pour ex�cuter. On fournit la documentation.
	! -------------------------------------------------
	!
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Conversion d''un fichier LFA en un fichier LFP.'
		print*,' '
		print*,'Sujet:'
		print*,' '
		print*,'Les fichiers binaires ne sont' &
& 		,' lisibles que par un logiciel.'
		print*,'Or il serait souvent utile de' &
& 		,' naviguer directement dans les'
		print*,'donn�es � l''aide d''un simple �diteur, pour voir'
		print*,'les valeurs individuelles,' &
& 		,' pouvoir les imprimer, etc...'
		print*,'Le pr�sent convertisseur transforme' &
& 		,' un fichier LFA (binaire IEEE)'
		print*,'en un fichier ASCII texte comportant' &
& 		,' in extenso les noms d''articles,'
		print*,'leur taille, type, et tous leurs �l�ments.' &
& 		,' Un tel fichier'
		print*,'peut egalement transiter par le courrier' &
& 		,' �lectronique.'
		print*,' '
		print*,'Utilisation: lfa2lfp nom_fic_entree' &
& 		,' nom_fic_sortie'
		print*,' '
		print*,'La transformation inverse est r�alis�e par lfp2lfa.'
		print*,' '
		stop
	else
		print*,' '
		print*,'Convert a LFA file into a LFP one.'
		print*,' '
		print*,'Subject:'
		print*,' '
		print*,'Binaries are readable only by a software.'
		print*,'It would be however often useful to navigate' &
& 		,' directly in the data'
		print*,'with a simple text editor, to look at' &
& 		,' individual values,'
		print*,'redirect them to printer, etc... The' &
& 		,' present procedure'
		print*,'converts a LFA file (IEEE binary)'
		print*,'into an ASCII text one, containig all data' &
& 		,' with article names,'
		print*,'length and type. This resulting file can also'
		print*,'be sent by email.'
		print*,' '
		print*,'Usage: lfa2lfp FILE_IN FILE_OUT'
		print*,' '
		print*,'The inverse operation can be performed by lfp2lfa.'
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
		if(clfe == ' ') then
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
igol2=2
call lfpouv(iuls,clfs,igol2)
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
		call lfa2lfpcopr(iule,clna,clna,ilong,iuls)
	elseif(cltype(1:1) == 'I') then
		!
		! Article de type entier.
		!
		call lfa2lfpcopi(iule,clna,clna,ilong,iuls)
	elseif(cltype(1:1) == 'C') then
		!
		! Article de type caract�re.
		!
		call lfa2lfpcopc(iule,clna,clna,ilong,iuls)
	else
		print*,'lfa2lfpcop/ATTENTION: type de donn�e inconnu!...'
		print*,cltype
	endif
	goto 100
endif
!
! Fermeture des fichiers.
!
call lfafer(iule)
call lfpfer(iuls)
end
subroutine lfa2lfpcopc(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfa2lfpcopC* Copie d'un article caract�res d'un fichier LFP � un autre.
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
call lfpecrc(kuls,clnas,clbouc,ilong)
end
subroutine lfa2lfpcopi(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfa2lfpcopI* Copie d'un article entier d'un fichier LFP � un autre.
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
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr,itype &
& ,ilnas,jbouc
character*(*) cdnae, cdnas
character*200 clnas
character*2 cltype
integer(kind=jpintusr) ibouc(klong)
!
! Lecture de l'article d'entr�e.
!
call lfaipos(kule,cdnae,ierr,itype,ilong)
call lfaitype(itype,cltype)
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
ilnas=len_trim(clnas)
write(kuls,fmt='(3A,i9,a)') cltype,'-%%!!','I',ilong &
& ,clnas(1:ilnas)
!
! Ecriture des entiers dans le LFP.
!
do jbouc=1,ilong
	write(kuls,fmt=*) ibouc(jbouc)
enddo
end
subroutine lfa2lfpcopr(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfa2lfpcopR* Copie d'un article r�el d'un fichier LFP � un autre.
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
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr,itype &
& ,ilnas,jbouc
character*(*) cdnae, cdnas
character*200 clnas,clfor
character*2 cltype
real(kind=jpreeusr) zbouc(klong)
!
! Lecture de l'article d'entr�e.
!
call lfaipos(kule,cdnae,ierr,itype,ilong)
call lfaitype(itype,cltype)
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
ilnas=len_trim(clnas)
write(kuls,fmt='(3A,i9,a)') cltype,'-%%!!','R',ilong &
& ,clnas(1:ilnas)
!
! Ecriture des reels dans le LFP.
!
!if(cltype == 'R4') then
!	clfor='(ES15.7)'
!elseif(cltype == 'R8') then
!	clfor='(ES23.15)'
!elseif(cltype == 'RG') then
!	clfor='(ES38.30)'
!else
!	print*,'lfa2lfpcopr/ERREUR: type de donnee inconnu!...'
!	print*,cltype
!	stop 'call abort'
!endif
do jbouc=1,ilong
	!write(kuls,fmt=clfor) zbouc(jbouc)
	write(kuls,fmt=*) zbouc(jbouc)
enddo
end
