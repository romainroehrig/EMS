MODULE yomlfa
! --------------------------------------------------------------
! **** Valeurs globales du logiciel LFA.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Auteur/author:   2002-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
save
!
!-------------------------------------------------
! Nombre maximal d'unités logiques possibles.
! Ne sert qu'à dimensionner le tableau recevant
! les noms en clair des fichiers des unités logiques ouvertes par LFA,
! aux seules fins d'impressions pour l'utilisateur.
!-------------------------------------------------
!
integer(kind=jpintusr), parameter :: jpmaxul=300
!
!-------------------------------------------------
! Nom en clair de chaque fichier ouvert.
!-------------------------------------------------
!
character*200, parameter :: cgindef='jhfjkefzref%6'
character*200 :: cgfnom(jpmaxul)=cgindef
end
subroutine getargp(karg,cdarg)
! --------------------------------------------------------------
! **** *getargp* GET Command Line Arguments.
! --------------------------------------------------------------
! Sujet:
! Cette routine est creee pour contourner une bug du f90 HP,
! qui decale de un par rapport au standard l'indice de l'argument
! a getarg!...
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) karg
integer iarg
character*(*) cdarg
#ifdef HPPA
iarg=karg+1
#else
iarg=karg
#endif
call getarg(iarg,cdarg)
end
subroutine lfaaffc(klbouc,kul,cdna)
! --------------------------------------------------------------
! **** *lfaaffc* Affichage de caracteres.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) klbouc,kul,ilong,ierr,jlong,ildta
character*(*) cdna
character*400 cldta(klbouc)
call lfalecc(kul,cdna,klbouc,cldta,ilong,ierr)
do jlong=1,ilong
	ildta=len_trim(cldta(jlong))
	write(*,'(a)') cldta(jlong)(1:ildta)
enddo
end
subroutine lfaaffi(klbouc,kul,cdna)
! --------------------------------------------------------------
! **** *lfaaffi* Affichage d'entiers.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) klbouc,kul,ilong,ierr,jlong
character*(*) cdna
integer(kind=jpintusr) idta(klbouc)
call lfaleci(kul,cdna,klbouc,idta,ilong,ierr)
do jlong=1,ilong
	print*,idta(jlong)
enddo
end
subroutine lfaaffr(klbouc,kul,cdna)
! --------------------------------------------------------------
! **** *lfaaffr* Affichage de reels.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) klbouc,kul,ilong,ierr,jlong
character*(*) cdna
real(kind=jpreeusr) zreel(klbouc)
call lfalecr(kul,cdna,klbouc,zreel,ilong,ierr)
do jlong=1,ilong
	print*,zreel(jlong)
enddo
end
subroutine lfaavan(kul)
! --------------------------------------------------------------------------
! **** *LFAAVAN* Saute l'article courant dans un fichier LFA.
! **** *LFAAVAN* Step over current article in an LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul               unite logique du fichier.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul               logical unit of the LFA file.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
#include"lfayom.h"
if(lgpoint(kul)) then
	!
	! Le pointeur est avant des donnees.
	! On les saute.
	!
	read(kul)
	!
	! Position du pointeur.
	!
	lgpoint(kul)=.false.
endif
end
subroutine lfacas(kul,cdna,cdtype,klong,kerr)
! --------------------------------------------------------------------------
! **** *LFACAS* Renseignements sur un article de fichier LFA.
! **** *LFACAS* Get documentation about a LFA article.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul               unite logique du fichier.
! cdna: si cdna=' ' on recherche l'article suivant.
! .         cdna est alors en entree/sortie,
! .         et en sortie il vaudra le nom de l'article suivant
! .         (si cet article existe).
! .         kerr...retour de recherche: 0 si OK,
! .                1 si fin de fichier.
! .     si cdna<>' ' cdna est le nom de l'article cherche.
! .          Il est alors en entree seulement.
! .         kerr...retour de recherche: 0 si OK,
! .                1 si article inexistant.
! En sortie:
! cdtype            type d'article: 'R4', 'I8', 'C '.
! klong             nombre d'elements de cet article.
! --------------------------------------------------------------------------
! Input:
! kul               file logical unit.
! cdna: if cdna=' ' on looks for nbext article.
! .         cdna is then in input/output
! .         and in output it will receive next article name
! .         (if this article exists).
! .         kerr...return from search: 0 if OK,
! .                1 if end of file.
! .     if cdna<>' ' cdna is the name from required article.
! .          It is then in input olny.
! .         kerr...return from search: 0 if OK,
! .                1 if non-existant article.
! Output:
! cdtype            article type: 'R4', 'I8', 'C '.
! klong             numbre of elements in this article.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,kerr,imes,itype
logical llerf
#include"lfayom.h"
character*(*) cdna
character*(*) cdtype
!
! On rend temporairement silencieux et tolerant le logiciel.
!
imes=nmes(kul)
nmes(kul)=0
llerf=lgerf(kul)
lgerf(kul)=.false.
!
! Recherche de l'article suivant.
!
call lfaipos(kul,cdna,kerr,itype,klong)
if(kerr == 0) then
	!
	! On n'etait pas en fin de fichier.
	! On lit le contenu de la documentation
	! pour fournir
	! le type de d'article (reel, entier, etc...)
	!
	call lfaitype(itype,cdtype)
endif
!
! On remet le niveau de messagerie et de tolerance a l'initial.
!
nmes(kul)=imes
lgerf(kul)=llerf
end
subroutine lfacop(kule,cdnae,cdnas,kuls)
! --------------------------------------------------------------
! **** *LFACOP* Copie d'un article d'un fichier LFA a un autre.
! **** *LFACOP* Copy one article from a LFA file to another.
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
! kule unite logique du fichier LFA d'entree.
! cdnae nom de l'article a lire.
! cdnas nom sous lequel l'article est recopie.
! kuls unite logique du fichier LFA de sortie.
! En sortie:
! Le fichier d'unite logique kuls est augmente d'un article.
! --------------------------------------------------------------
! Input:
! kule logical unit of input LFA file.
! cdnae article name to be read.
! cdnas article name to be written out.
! kuls logical unit of output LFA file.
! Output:
! The file which logical unit is kuls receives one more article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*2 cltype
!
! Renseignements sur l'article cdnae.
!
call lfacas(kule,cdnae,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! L'article existe.
	!
	if(cltype(1:1) == 'R') then
		!
		! Article de type reel.
		!
		call lfaicopr(kule,cdnae,cdnas,ilong,kuls)
	elseif(cltype(1:1) == 'I') then
		!
		! Article de type entier.
		!
		call lfaicopi(kule,cdnae,cdnas,ilong,kuls)
	elseif(cltype(1:1) == 'C') then
		!
		! Article de type caractere.
		!
		call lfaicopc(kule,cdnae,cdnas,ilong,kuls)
	else
		print*,'LFACOP/ATTENTION: type de donnee inconnu!...'
		print*,cltype
	endif
else
	print*,'LFACOP/ATTENTION: article ',cdnae,' inexistant!...'
endif
end
subroutine lfaecrc(kul,cdna,cdcar,klong)
! --------------------------------------------------------------------------
! **** *LFAECRC* Ecriture de caracteres sur fichier LFA.
! **** *LFAECRC* Write character data on LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul              unite logique du fichier.
! cdna             nom de l'article a ecrire.
! cdcar(1,klong)   caracteres a ecrire.
! klong            longueur de l'article a ecrire.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             name of article to write.
! cdcar(1,klong)   characters to write.
! klong            length of article to write.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilnomf,ilna,inbcare &
& 	,jlong,illoc,itype,iprod
character*(*) cdna
character*(*) cdcar(klong)
character*3 cllang
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfaecrc: ecriture de l''article ',cdna
endif
if(cgtypo(kul) == 'R') then
	if(cllang() == 'FRA') then
		print* &
& 			,'LFAECRC/ERREUR: ecriture sur fichier ouvert en lecture!...'
		print*,'Unite logique: ',kul
		ilnomf=len_trim(cgnomf(kul))
		print*,'Fichier ',cgnomf(kul)(1:ilnomf)
		ilna=len_trim(cdna)
		print*,'Article ',cdna(1:ilna)
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print* &
& 			,'LFAECRC/ERROR: write on file opened in read!...'
		print*,'Logical unit: ',kul
		ilnomf=len_trim(cgnomf(kul))
		print*,'File ',cgnomf(kul)(1:ilnomf)
		ilna=len_trim(cdna)
		print*,'Article ',cdna(1:ilna)
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
!
! On determine le nombre maximal de caracteres par element
! du tableau d'entree.
!
inbcare=0
do jlong=1,klong
	illoc=len_trim(cdcar(jlong))
	inbcare=max(inbcare,illoc)
enddo
!
! Ecriture de l'autodocumentation de l'article.
!
itype=-inbcare ! type de donnee (< 0 pour les donnees caracteres).
call lfaidoc(kul,itype,klong,cdna)
!
! Ecriture de l'article caractere
! sous forme d'entiers.
!
iprod=inbcare*klong ! nombre total de caracteres sur l'ensemble du tableau.
call lfaiecrcloc(kul,cdcar,inbcare,klong,iprod)
end
subroutine lfaecri(kul,cdna,kentier,klong)
! --------------------------------------------------------------------------
! **** *LFAECRI* Ecriture d'entiers sur fichier LFA.
! **** *LFAECRI* Write integer data of LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul                  unite logique du fichier.
! cdna                  nom de l'article a ecrire.
! kentier(1,klong)      entiers a ecrire.
! klong            longueur de l'article a ecrire.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul                  logical unit of LFA file.
! cdna                 name of article to write.
! kentier(1,klong)     integers to write.
! klong                length of article to write.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilnomf,ilna,itype,iprec,jlong
character*(*) cdna
integer(kind=jpintusr) kentier(klong)
character*3 cllang
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfaecri: ecriture de ',cdna
endif
if(cgtypo(kul) == 'R') then
	if(cllang() == 'FRA') then
		print* &
& 			,'LFAECRI/ERREUR: ecriture sur fichier ouvert en lecture!...'
		print*,'Unite logique: ',kul
		ilnomf=len_trim(cgnomf(kul))
		print*,'Fichier ',cgnomf(kul)(1:ilnomf)
		ilna=len_trim(cdna)
		print*,'Article ',cdna(1:ilna)
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print* &
& 			,'LFAECRI/ERROR: write on file opened in read!...'
		print*,'Logical unit: ',kul
		ilnomf=len_trim(cgnomf(kul))
		print*,'File ',cgnomf(kul)(1:ilnomf)
		ilna=len_trim(cdna)
		print*,'Article ',cdna(1:ilna)
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
!
! Ecriture de l'autodocumentation de l'article.
!
if(npreci(kul) == 8) then
	itype=4
elseif(npreci(kul) == 4) then
	itype=2
else
	if(cllang() == 'FRA') then
		print*,'LFAECRI/ERREUR: type non prevu!...'
		print*,npreci(kul)
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print*,'LFAECRI/ERROR: type unexpected!...'
		print*,npreci(kul)
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
call lfaidoc(kul,itype,klong,cdna)
!
! Ecriture des entiers dans le LFA.
!
!
#ifdef dblieee
!
! Sur certains Cray, la precision de sortie
! en IEEE est celle du tableau divisee
! par 2; il faut donc ici multiplier
! par 2 pour garantir le resultat.
!
iprec=2*npreci(kul)
#else
iprec=npreci(kul)
#endif
if(iprec == jpintusr) then
	!
	! La precision des entiers a ecrire
	! est celle des entiers passes en argument.
	! Ecriture directe donc.
	!
	write(kul) (kentier(jlong),jlong=1,klong)
elseif(iprec == 4) then
	!
	! La precision des entiers a ecrire est de 4 octets.
	!
	call lfaiecri4(kul,klong,kentier)
elseif(iprec == 8) then
	!
	! La precision des entiers a ecrire est de 8 octets.
	!
	call lfaiecri8(kul,klong,kentier)
else
	if(cllang() == 'FRA') then
		print*,'LFAECRI/ERREUR: precision de sortie impossible: ',iprec
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print*,'LFAECRI/ERROR: ouput precision: ',iprec
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
end
subroutine lfaecrr(kul,cdna,preel,klong)
! --------------------------------------------------------------------------
! **** *LFAECRR* Ecriture de reels sur fichier LFA.
! **** *LFAECRR* Write real data on LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul              unite logique du fichier.
! cdna             nom de l'article a ecrire.
! preel(1,klong)   reels a ecrire.
! klong            longueur de l'article a ecrire.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             name of article to write.
! preel(1,klong)   real data to write.
! klong            length of article to write.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilnomf,ilna,itype,jlong,iprec
character*(*) cdna
real(kind=jpreeusr) preel(klong)
character*3 cllang
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfaecrr: ecriture de l''article ',cdna
endif
if(cgtypo(kul) == 'R') then
	if(cllang() == 'FRA') then
		print* &
& 			,'LFAECRR/ERREUR: ecriture sur fichier ouvert en lecture!...'
		print*,'Unite logique: ',kul
		ilnomf=len_trim(cgnomf(kul))
		print*,'Fichier ',cgnomf(kul)(1:ilnomf)
		ilna=len_trim(cdna)
		print*,'Article ',cdna(1:ilna)
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print* &
& 			,'LFAECRR/ERROR: write on file opened in read!...'
		print*,'Logical unit: ',kul
		ilnomf=len_trim(cgnomf(kul))
		print*,'File ',cgnomf(kul)(1:ilnomf)
		ilna=len_trim(cdna)
		print*,'Article ',cdna(1:ilna)
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
!
! Ecriture de l'autodocumentation de l'article.
!
if(nprecr(kul) == 8) then
	itype=1
elseif(nprecr(kul) == 4) then
	itype=3
else
	if(cllang() == 'FRA') then
		print*,'LFAECRR/ERREUR: type non prevu!...'
		print*,nprecr(kul)
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print*,'LFAECRR/ERROR: type unexpected!...'
		print*,nprecr(kul)
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
call lfaidoc(kul,itype,klong,cdna)
!
! Ecriture de l'article reel.
!
#ifdef dblieee
!
! Sur certains Cray, la precision de sortie
! en IEEE est celle du tableau divisee
! par 2; il faut donc ici multiplier
! par 2 pour garantir le resultat.
!
iprec=2*nprecr(kul)
#else
iprec=nprecr(kul)
#endif
if(iprec == jpreeusr) then
	!
	! La precision des reels a ecrire
	! est celle des reels passes en argument.
	! Ecriture directe donc.
	!
	write(kul) (preel(jlong),jlong=1,klong)
elseif(iprec == 4) then
	!
	! La precision des reels a ecrire est de 4 octets.
	!
	call lfaiecrr4(kul,klong,preel)
elseif(iprec == 8) then
	!
	! La precision des reels a ecrire est de 8 octets.
	!
	call lfaiecrr8(kul,klong,preel)
else
	if(cllang() == 'FRA') then
		print*,'LFAECRR/ERREUR: precision de sortie impossible: ',iprec
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print*,'LFAECRR/ERROR: output precision: ',iprec
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
end
subroutine lfaerf(kul,lderf)
! --------------------------------------------------------------------------
! **** *LFAERF* Niveau d'erreur tolere par le logiciel LFA.
! **** *LFAERF* Choose error level of LFA software.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul               unite logique du fichier.
! lderf             .true. si toute erreur doit etre fatale,
! .false. si aucune ne doit l'etre.
! En sortie:
! lgerf             .true. si toute erreur est fatale,
! .false. si aucune ne l'est.
! --------------------------------------------------------------------------
! Input:
! kul               logical unit of LFA file.
! lderf             .true. if any error has to be fatal.
! .false. si none has to be.
! Output:
! lgerf             .true. if any error has to be fatal.
! .false. si none has to be.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
logical lderf
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfaerf: lgerf(',kul,' mis a ',lderf
endif
lgerf(kul)=lderf
end
subroutine lfafer(kul)
! --------------------------------------------------------------------------
! **** *LFAFER* Fermeture de fichier LFA.
! **** *LFAFER* Close a LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul        unite logique du fichier.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul        logical unit of LFA file.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfafer: fermeture de l''unite logique ',kul
endif
close(kul)
#ifdef cray
call lfaiunassign(kul)
#endif
end
subroutine lfaforvlc(kule,kuls,cdnoma,knomal)
! --------------------------------------------------------------
! **** *lfaforvlc* Formatte vers article LFA de caracteres.
! **** *lfaforvlc* Formatted data to LFA character article.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
! Input:
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,kuls,knomal,jlong
character*400 clval(knomal)
character*(*) cdnoma
do jlong=1,knomal
	read(kule,fmt='(a)') clval(jlong)
enddo
call lfaecrc(kuls,cdnoma,clval,knomal)
end
subroutine lfaforvli(kule,kuls,cdnoma,knomal)
! --------------------------------------------------------------
! **** *lfaforvli* Formatte vers article LFA d'entiers.
! **** *lfaforvli* Formatted data to LFA integer article.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
! Input:
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,kuls,knomal,jlong
integer(kind=jpintusr) ival(knomal)
character*(*) cdnoma
do jlong=1,knomal
	read(kule,fmt=*) ival(jlong)
enddo
call lfaecri(kuls,cdnoma,ival,knomal)
end
subroutine lfaforvlr(kule,kuls,cdnoma,knomal)
! --------------------------------------------------------------
! **** *lfaforvlr* Formatte vers article LFA de reels.
! **** *lfaforvlr* Formatted data to LFA real article.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
! Input:
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,kuls,knomal,jlong
real(kind=jpreeusr) zval(knomal)
character*(*) cdnoma
do jlong=1,knomal
	read(kule,fmt=*) zval(jlong)
enddo
call lfaecrr(kuls,cdnoma,zval,knomal)
end
subroutine lfaicopc(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfaicopc* Copie d'un article caracteres d'un fichier LFA a un autre.
! **** *lfaicopc* Copy a character LFA article from one file to another.
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
! kule unite logique du fichier LFA d'entree.
! cdnae nom de l'article a lire.
! cdnas nom sous lequel l'article est recopie.
! klong longeur de l'article a copier.
! kuls unite logique du fichier LFA de sortie.
! En sortie:
! --------------------------------------------------------------
! Input:
! kule logical unit of LFA input file.
! cdnae name of article to read.
! cdnas name of article to write.
! klong length of article to copy.
! kuls logical unit of LFA output file.
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*400 clnas
character*400 clbouc(klong)
!
! Lecture de l'article d'entree.
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
subroutine lfaicopi(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfaicopi* Copie d'un article entier d'un fichier LFA a un autre.
! **** *lfaicopi* Copy an integer LFA article from one file to another.
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
! kule unite logique du fichier LFA d'entree.
! cdnae nom de l'article a lire.
! cdnas nom sous lequel l'article est recopie.
! klong longeur de l'article a copier.
! kuls unite logique du fichier LFA de sortie.
! En sortie:
! --------------------------------------------------------------
! Input:
! kule logical unit of LFA input file.
! cdnae name of article to read.
! cdnas name of article to write.
! klong length of article to copy.
! kuls logical unit of LFA output file.
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr,iprec
character*(*) cdnae, cdnas
character*200 clnas
integer(kind=jpintusr) ibouc(klong)
character*3 cllang
#include"lfadoc.h"
!
! Lecture de l'article d'entree.
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
! Choix de la precision de sortie, en fonction
! de celle lue en entree.
!
if(cgdoc == 'I4-') then
	iprec=4
elseif(cgdoc == 'I8-') then
	iprec=8
else
	if(cllang() == 'FRA') then
		print*,'LFAICOPI/ERREUR: precision inconnue!...'
		print*,cgdoc
		stop 'call abort'
	else
		print*,'LFAICOPI/ERROR: unknown precision!...'
		print*,cgdoc
		stop 'call abort'
	endif
endif
call lfapreci(kuls,iprec)
!
! Ecriture sur le fichier de sortie.
!
call lfaecri(kuls,clnas,ibouc,ilong)
end
subroutine lfaicopr(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *lfaicopr* Copie d'un article reel d'un fichier LFA a un autre.
! **** *lfaicopr* Copy a real LFA article from one file to another.
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
! kule unite logique du fichier LFA d'entree.
! cdnae nom de l'article a lire.
! cdnas nom sous lequel l'article est recopie.
! klong longeur de l'article a copier.
! kuls unite logique du fichier LFA de sortie.
! En sortie:
! --------------------------------------------------------------
! Input:
! kule logical unit of LFA input file.
! cdnae name of article to read.
! cdnas name of article to write.
! klong length of article to copy.
! kuls logical unit of LFA output file.
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr,iprec
character*(*) cdnae, cdnas
character*200 clnas
real(kind=jpreeusr) zbouc(klong)
character*3 cllang
#include"lfadoc.h"
!
! Lecture de l'article d'entree.
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
! Choix de la precision de sortie, en fonction
! de celle lue en entree.
!
if(cgdoc == 'R4-') then
	iprec=4
elseif(cgdoc == 'R8-') then
	iprec=8
else
	if(cllang() == 'FRA') then
		print*,'LFAICOPR/ERREUR: precision inconnue!...'
		print*,cgdoc
		stop 'call abort'
	else
		print*,'LFAICOPR/ERROR: unknown precision!...'
		print*,cgdoc
		stop 'call abort'
	endif
endif
call lfaprecr(kuls,iprec)
!
! Ecriture sur le fichier de sortie.
!
call lfaecrr(kuls,clnas,zbouc,ilong)
end
subroutine lfaidoc(kul,ktype,klong,cdna)
! --------------------------------------------------------------------------
! **** *lfaidoc* Ecriture de l'autodocumentation de l'article: type d'article, longueur et nom.
! **** *lfaidoc* Write autodocumentation of a LFA article.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul: unite logique
! ktype: type d'article.
! klong: longueur de l'article.
! cdna: nom de l'article.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul: logical unit of LFA file.
! ktype: article type.
! klong: article length.
! cdna: article name.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jna
integer(kind=jpintusr) ktype,klong,ilna
integer(kind=jpintesb) itypeesb,ilongesb,ilnaesb,ichar
character*(*) cdna
!
! Conversion vers entiers 4 octets.
!
itypeesb=ktype
ilongesb=klong
!
! Longueur de la chaine de caracteres.
!
ilna=len_trim(cdna)
ilnaesb=ilna
write(kul) itypeesb,ilongesb,ilnaesb &
& 	,(ichar(cdna(jna:jna)),jna=1,ilnaesb)
end
subroutine lfaiecrcloc(kul,cdcar,knbcare,klong,kprod)
! --------------------------------------------------------------
! **** *lfaiecrcloc* Ecriture de caracteres un LFA.
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
integer(kind=jpintusr) kul,knbcare,klong,kprod,jlong
character*(*) cdcar(klong)
character*(knbcare) clcar(klong)
integer(kind=jpintusr) iascii(kprod)
!
! -------------------------------------------------
! On porte le tableau d'entree sur un tableau
! dont les elements sont plus courts (de longueur knbcare).
! -------------------------------------------------
!
do jlong=1,klong
	clcar(jlong)=cdcar(jlong)
enddo
!
! -------------------------------------------------
! Ecriture des caracteres sur le fichier LFA.
! -------------------------------------------------
!
write(kul) clcar
end
subroutine lfaiecri4(kul,klong,kentier)
! --------------------------------------------------------------
! **** *lfaiecrr4* Ecriture d'entiers la precision imposee 4 octets.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
integer(kind=4) ientier(klong)
integer(kind=jpintusr) kentier(klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	ientier(jlong)=kentier(jlong)
enddo
!
! Ecriture du tableau affecte.
!
write(kul) (ientier(jlong),jlong=1,klong)
end
subroutine lfaiecri8(kul,klong,kentier)
! --------------------------------------------------------------
! **** *lfaiecrr8* Ecriture d'entiers la precision imposee 8 octets.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
integer(kind=8) ientier(klong)
integer(kind=jpintusr) kentier(klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	ientier(jlong)=kentier(jlong)
enddo
!
! Ecriture du tableau affecte.
!
write(kul) (ientier(jlong),jlong=1,klong)
end
subroutine lfaiecrr4(kul,klong,preel)
! --------------------------------------------------------------
! **** *lfaiecrr4* Ecriture de reels a la precision imposee 4 octets.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
real(kind=4) zreel(klong)
real(kind=jpreeusr) preel(klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	zreel(jlong)=preel(jlong)
enddo
!
! Ecriture du tableau affecte.
!
write(kul) (zreel(jlong),jlong=1,klong)
end
subroutine lfaiecrr8(kul,klong,preel)
! --------------------------------------------------------------
! **** *lfaiecrr8* Ecriture de reels a la precision imposee 8 octets.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
real(kind=8) zreel(klong)
real(kind=jpreeusr) preel(klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	zreel(jlong)=preel(jlong)
enddo
!
! Ecriture du tableau affecte.
!
write(kul) (zreel(jlong),jlong=1,klong)
end
subroutine lfaileccloc(kul,cdcar,kdimb,knbcare,klong)
! --------------------------------------------------------------
! **** *lfaileccloc* Lecture d'entiers sur un LFA, convertis en caracteres.
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
integer(kind=jpintusr) kul,klong,jlong,kdimb,knbcare
#include"lfayom.h"
character*(*) cdcar(kdimb)
character*(knbcare) clcar(klong)
character*3 cllang
!
! Test de la position du pointeur.
!
if(.not.lgpoint(kul)) then
	!
	! Le pointeur est avant une autodocumentation.
	! C'est qu'il y a eu un probleme en amont.
	!
	if(cllang() == 'FRA') then
		print*,'LFAILECCLOC/ERREUR: pointeur non positionne' &
& 			,' avant des donnees!...'
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print*,'LFAILECCLOC/ERROR: pointer location not before' &
& 			,' data!...'
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
!
! -------------------------------------------------
! Lecture des entiers sur le fichier LFA.
! -------------------------------------------------
!
read(kul) clcar
do jlong=1,klong
	cdcar(jlong)=clcar(jlong)
enddo
!
! Position du pointeur.
!
lgpoint(kul)=.false.
end
subroutine lfaileccloc8(kul,cdcar,kdimb,knbcare,klong,kprod)
! --------------------------------------------------------------
! **** *lfaileccloc8* Lecture d'entiers sur un LFA, convertis en caracteres.
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
integer(kind=jpintusr) kul,kdimb,knbcare,klong,jlong &
& 	,kprod,jprod,jna,ipos
#include"lfayom.h"
character*(*) cdcar(kdimb)
character*(knbcare) clcar(klong)
integer(kind=jpintesb) iasciiesb(kprod)
character*3 cllang
!
! Test de la position du pointeur.
!
if(.not.lgpoint(kul)) then
	!
	! Le pointeur est avant une autodocumentation.
	! C'est qu'il y a eu un probleme en amont.
	!
	if(cllang() == 'FRA') then
		print*,'LFAILECCLOC8/ERREUR: pointeur non positionne' &
& 			,' avant des donnees!...'
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print*,'LFAILECCLOC8/ERROR: pointer location not before' &
& 			,' data!...'
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
!
! -------------------------------------------------
! Lecture des entiers sur le fichier LFA.
! -------------------------------------------------
!
read(kul) (iasciiesb(jprod),jprod=1,kprod)
!
! Position du pointeur.
!
lgpoint(kul)=.false.
!
! -------------------------------------------------
! Conversion entiers > caracteres.
! -------------------------------------------------
!
do jlong=1,klong
	!
	! On initialise l'element a une chaine blanche.
	!
	cdcar(jlong)=' '
	do jna=1,knbcare
		!
		! On convertit le code ASCII en un caractere.
		!
		ipos=(jlong-1)*knbcare+jna
		cdcar(jlong)(jna:jna)=char(iasciiesb(ipos))
	enddo
enddo
end
subroutine lfaileci4(kul,klong,kentier)
! --------------------------------------------------------------
! **** *lfaileci4* Lecture d'entiers a 4 octets sur fichier.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
integer(kind=4) ientier(klong)
integer(kind=jpintusr) kentier(klong)
!
! Ecriture du tableau affecte.
!
read(kul) (ientier(jlong),jlong=1,klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	kentier(jlong)=ientier(jlong)
enddo
end
subroutine lfaileci8(kul,klong,kentier)
! --------------------------------------------------------------
! **** *lfaileci8* Lecture d'entiers a 8 octets sur fichier.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
integer(kind=8) ientier(klong)
integer(kind=jpintusr) kentier(klong)
!
! Ecriture du tableau affecte.
!
read(kul) (ientier(jlong),jlong=1,klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	kentier(jlong)=ientier(jlong)
enddo
end
subroutine lfailecr4(kul,klong,preel)
! --------------------------------------------------------------
! **** *lfailecr4* Lecture de reels 4 octets sur fichier.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
real(kind=4) zreel(klong)
real(kind=jpreeusr) preel(klong)
!
! Ecriture du tableau affecte.
!
read(kul) (zreel(jlong),jlong=1,klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	preel(jlong)=zreel(jlong)
enddo
end
subroutine lfailecr8(kul,klong,preel)
! --------------------------------------------------------------
! **** *lfailecr8* Lecture de reels 8 octets sur fichier.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,jlong,klong
real(kind=8) zreel(klong)
real(kind=jpreeusr) preel(klong)
!
! Ecriture du tableau affecte.
!
read(kul) (zreel(jlong),jlong=1,klong)
!
! On affecte un tableau dans l'autre
! afin que le changement de precision s'opere.
!
do jlong=1,klong
	preel(jlong)=zreel(jlong)
enddo
end
subroutine lfaiminmc(kul,cdna,cdtype,klong)
! --------------------------------------------------------------
! **** *lfaiminmc* Extrema d'un article de caracteres de fichier LFA.
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
! kul unite logique du fichier LFA d'entree.
! cdna nom de l'article a lire.
! cdtype type d'article (reel, entier, etc...).
! klong longueur de cet article.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilong,ierr,jlong &
& 	,ilnom,ilmin,ilmax,igol18
character*400 cldonmin,cldonmax
character*400 cldon(klong)
character*200 cdna
character*(*) cdtype
call lfalecc(kul,cdna,klong,cldon,ilong,ierr)
cldonmin=cldon(1)
cldonmax=cldon(1)
do jlong=1,ilong
	if(cldon(jlong) < cldonmin) cldonmin=cldon(jlong)
	if(cldon(jlong) > cldonmax) cldonmax=cldon(jlong)
enddo
ilnom=len_trim(cdna)
igol18=18
ilnom=max(igol18,ilnom)
ilmin=12
ilmax=ilmin
!write(*,'(4a,i8,4a)') cdna(1:ilnom),'|',cdtype,'| l=',ilong,', min=',cldonmin(1:ilmin),' max=',cldonmax(1:ilmax)
write(*,'(a,i8,4a,34x,4a)') 'l=',ilong,', min=',cldonmin(1:ilmin),' max=',cldonmax(1:ilmax) &
& ,'|',cdtype,'| ',cdna(1:ilnom)
end
subroutine lfaiminmi(kul,cdna,cdtype,klong)
! --------------------------------------------------------------
! **** *lfaiminmi* Extrema d'un article d'entiers de fichier LFA.
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
! kul unite logique du fichier LFA d'entree.
! cdna nom de l'article a lire.
! cdtype type d'article (reel, entier, etc...).
! klong longueur de cet article.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
#include"lfayom.h"
integer(kind=jpintusr) kul,klong,ilong,ierr,imin,imax &
& 	,jlong,ilnom,ilmoy,ilrcm,ilmin,ilmax,iopt,inc
real(kind=jpreeusr) zmoy,zrcm,zvaltmp
character*200 clmin,clmax,clmoy,clrcm,cllong
character*200 cdna
character*(*) cdtype
integer(kind=jpintusr) idon(klong),igol18
call lfaleci(kul,cdna,klong,idon,ilong,ierr)
imin=idon(1)
imax=idon(1)
zmoy=0.
zrcm=0.
do jlong=1,ilong
	if(idon(jlong) < imin) imin=idon(jlong)
	if(idon(jlong) > imax) imax=idon(jlong)
	zvaltmp=real(idon(jlong))
	zmoy=zmoy+zvaltmp
	zrcm=zrcm+zvaltmp*zvaltmp
enddo
zmoy=zmoy/real(ilong)
zrcm=sqrt(zrcm/real(ilong))
ilnom=len_trim(cdna)
igol18=18
ilnom=max(igol18,ilnom)
write(clmin,fmt='(4x,i8)') imin
write(clmax,fmt='(4x,i8)') imax
ilmin=12
ilmax=ilmin
if(lglang) then
	write(*,'(a,i8,2(a,i11),2(a,g11.4),4a)') 'l=',ilong,', min= ',imin,' max= ',imax,' moy= ',zmoy,' rcm= ',zrcm &
	& ,'|',cdtype,'| ',cdna(1:ilnom)
else
	write(*,'(a,i8,2(a,i11),2(a,g11.4),4a)') 'l=',ilong,', min= ',imin,' max= ',imax,' mea= ',zmoy,' rms= ',zrcm &
	& ,'|',cdtype,'| ',cdna(1:ilnom)
endif
end
subroutine lfaiminmr(kul,cdna,cdtype,klong)
! --------------------------------------------------------------
! **** *lfaiminmr* Extrema d'un article de reels de fichier LFA.
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
! kul unite logique du fichier LFA d'entree.
! cdna nom de l'article a lire.
! cdtype type d'article (reel, entier, etc...).
! klong longueur de cet article.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
#include"lfayom.h"
integer(kind=jpintusr) kul,klong,ilong,ierr,jlong,ilnom &
& 	,ilmoy,ilrcm,ilmin,ilmax,igol18,iopt,inc
real(kind=jpreeusr) zmin,zmax,zmoy,zrcm
character*200 clmin,clmax,clmoy,clrcm
character*200 cdna
character*(*) cdtype
real(kind=jpreeusr) zdon(klong)
call lfalecr(kul,cdna,klong,zdon,ilong,ierr)
zmin=zdon(1)
zmax=zdon(1)
zmoy=0.
zrcm=0.
do jlong=1,ilong
	if(zdon(jlong) < zmin) zmin=zdon(jlong)
	if(zdon(jlong) > zmax) zmax=zdon(jlong)
	zmoy=zmoy+zdon(jlong)
	zrcm=zrcm+zdon(jlong)*zdon(jlong)
enddo
zmoy=zmoy/real(ilong)
zrcm=sqrt(zrcm/real(ilong))
ilnom=len_trim(cdna)
igol18=18
ilnom=max(igol18,ilnom)
if(lglang) then
	write(*,'(a,i8,4(a,g11.4),4a)') 'l=',ilong,', min= ',zmin,' max= ',zmax,' moy= ',zmoy,' rcm= ',zrcm &
	& ,'|',cdtype,'| ',cdna(1:ilnom)
else
	write(*,'(a,i8,4(a,g11.4),4a)') 'l=',ilong,', min= ',zmin,' max= ',zmax,' mea= ',zmoy,' rms= ',zrcm &
	& ,'|',cdtype,'| ',cdna(1:ilnom)
endif
end
subroutine lfainoma(kascii,knoma,kna,cdna)
! --------------------------------------------------------------
! **** *LFAINOMA* Passage suite d'entiers ASCII > chaine de caracteres.
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
! kascii: tableau contenant la suite des entiers ASCII.
! knoma: dimension physique du tableau kascii.
! kna: nombre d'entiers reellement ecrits sur kascii.
! En sortie:
! cdna: nom de l'article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) knoma,kna,jna
integer(kind=jpintusr) kascii(knoma)
character*(*) cdna
!
! -------------------------------------------------
! On cree le nom de l'article a partir
! du tableau d'entiers kascii.
! -------------------------------------------------
!
cdna=' '
do jna=1,kna
	cdna(jna:jna)=char(kascii(jna))
enddo
end
subroutine lfaipos(kul,cdna,kerr,ktype,klong)
! --------------------------------------------------------------------------
! **** *lfaipos* Recherche de la position d'un article dans un fichier LFA.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! 98-03, Luc Gerard: protection de la boucle sur des indefs,
! intervenant en fin de fichier sur certains logiciels (f90 HP!...).
! --------------------------------------------------------------------------
! En entree:
! kul          unite logique du fichier.
! cdna         nom de l'article.
! - si cdna chaine non blanche:
! On recherche l'article de nom cdna.
! - si cdna chaine blanche:
! On recherche l'article suivant.
! --------------------------------------------------------------------------
! En sortie:
! kerr indicateur d'erreur:
! +----------+--------------------------------------------------+
! | Valeur   |             Signification                        |
! +----------+--------------------------------------------------+
! | kerr=  0 | Tout est OK                                      |
! |          | Le pointeur sequentiel est alors positionne      |
! |          | sur l'article demande.                           |
! | kerr= -1 | Article inexistant                               |
! | kerr=-10 | Fin de fichier en recherche de l'article suivant |
! +----------+--------------------------------------------------+
! ktype type d'article (entier, reel, etc...)
! klong longueur de l'article.
! kerr         indicateur: 0 si article rencontre avant la fin du fichier.
! Le pointeur du fichier sequentiel est
! alors positionne sur l'article demande.
! ktype type d'article (entier, reel, etc...)
! klong longueur de l'article.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
#include"lfayom.h"
integer(kind=jpintusr) kul,kerr,ildna,ilgna &
& 	,ktype,klong,ilna,iversion,iascii(jpnoma),jnausr,inoma
integer(kind=jpintesb) itypeesb,ilongesb,ilnaesb &
& 	,iasciiesb(jpnoma),iversionesb,jna,ijpnoma
logical llrew
character*(*) cdna
character*(jpnoma) clna
character*3 cllang
ildna=len_trim(cdna)
!
! -------------------------------------------------
! Impressions preliminaires.
! -------------------------------------------------
!
if(nmes(kul) == 2) then
	print*,'++ lfaipos: recherche de ',cdna(1:ildna),'...'
endif
!
! -------------------------------------------------
! Initialisations.
! -------------------------------------------------
!
kerr=-1
llrew=.false. ! vrai si fichier rembobine, faux sinon.
!
! -------------------------------------------------
! Test de la position du pointeur.
! -------------------------------------------------
!
if(lgpoint(kul)) then
	!
	! Le pointeur est avant un article de donnees.
	! Ces donnees ne seraient-elles pas justement
	! celles qu'il faut trouver?
	!
	! Ce cas est peu probable dans le cadre
	! d'une lecture des articles du fichier
	! dans le desordre, mais est le cas general
	! lors d'une lecture sequentielle.
	!
	ilgna=len_trim(cgna(kul))
	if(cgna(kul)(1:ilgna) == cdna(1:ildna)) then
		!
		! Le pointeur est positionne avant l'article
		! de donnees que l'utilisateur
		! souhaite lire. Il n'y a donc
		! aucune action de lecture du fichier
		! a effectuer.
		!
		kerr=0
		ktype=ntype(kul)
#ifdef r4pre2
		if(ktype == 1.and.nversion(kul) < 21) ktype=3
#endif
		klong=nlong(kul)
		return
	else
		!
		! On est positionne sur un article de donnees,
		! or il fauit aller chercher l'article suivant.
		! Il faut donc sauter les donnees pour permettre
		! de lire l'autodocumentation suivante.
		!
		read(kul)
		!
		! Position du pointeur.
		!
		lgpoint(kul)=.false.
	endif
else
	!
	! Le pointeur est avant un article d'autodocumentation.
	! Il faut lire cette autodocumentation, ce qui est fait ci-apres!...
	!
endif
!
! -------------------------------------------------
! Lecture de l'autodocumentation.
! -------------------------------------------------
!
ijpnoma=jpnoma
  100 read(kul,end=200) itypeesb,ilongesb,ilnaesb &
& 	,(iasciiesb(jna),jna=1,min(ilnaesb,ijpnoma))
!
! -------------------------------------------------
! Le min ci-dessus sert a proteger le logiciel
! du traitement errone de la fin de fichier par certains
! codes fortran (f90 HP pour ne pas le nommer!...),
! lesquels bien qu'en situation de fin de fichier
! affectent des indefs a ilnaesb avant de sortir
! via l'etiquette "end=", et donc tournent sur une
! boucle jna=1,ilnaesb parfois gigantesque avant de sortir!...
! -------------------------------------------------
!
if(ilnaesb > jpnoma) then
	!
	! -------------------------------------------------
	! Le present cas peut arriver si le code servant
	! a lire un fichier a ete compile avec une valeur
	! de jpnoma plus faible que celle du code
	! ayant servit a l'ecrire, et que l'article
	! ecrit avait une taille physique plus grande
	! que le premier jpnoma.
	! Ce cas est plus que rarissime: il n'est carrement encore
	! jamais survenu!...
	! -------------------------------------------------
	!
	if(cllang() == 'FRA') then
		print*,'LFAIPOS/ERREUR: ilnaesb > jpnoma!...'
		print*,ilnaesb,jpnoma
		call lfa_print_file(kul)
		stop 'call abort'
	else
		print*,'LFAIPOS/ERROR: ilnaesb > jpnoma!...'
		print*,ilnaesb,jpnoma
		call lfa_print_file(kul)
		stop 'call abort'
	endif
endif
ktype=itypeesb
#ifdef r4pre2
if(ktype == 1.and.nversion(kul) < 21) ktype=3
#endif
klong=ilongesb
ilna=ilnaesb
do jnausr=1,ilna
	iascii(jnausr)=iasciiesb(jnausr)
enddo
if(nmes(kul) == 2) print*,'lfaipos: Article de type ',ktype &
& 	,' de longueur ',klong
!
! -------------------------------------------------
! On cree le nom clna de l'article a partir
! du tableau d'entiers iascii.
! -------------------------------------------------
!
inoma=jpnoma
call lfainoma(iascii,inoma,ilna,clna)
!
! Position du pointeur.
!
lgpoint(kul)=.true.
ntype(kul)=ktype
nlong(kul)=klong
cgna(kul)=clna
!
! Sortie de renseignements.
!
if(nmes(kul) == 2) print*,'lfaipos: Article teste: ',clna(1:ilna)
if(clna(1:ilna) == cdna(1:ildna)) then
	!
	! -------------------------------------------------
	! Le nom de l'article courant est bien celui
	! demande.
	! -------------------------------------------------
	!
	if(nmes(kul) == 2) print*,'lfaipos: Article trouve.'
	kerr=0
	return
elseif(cdna == ' ') then
	!
	! -------------------------------------------------
	! On cherchait l'article suivant.
	! -------------------------------------------------
	!
	if(nmes(kul) == 2) print*,'lfaipos: Article suivant trouve.'
	cdna=clna
	kerr=0
	return
else
	!
	! -------------------------------------------------
	! Le nom de l'article courant n'est pas celui
	! demande. On saute les donnees associees.
	! -------------------------------------------------
	!
	read(kul)
	!
	! Position du pointeur.
	!
	lgpoint(kul)=.false.
endif
goto 100
  200 continue
!
! On est en fin de fichier.
!
rewind(kul)
read(kul) iversionesb
iversion=iversionesb
!
! Position du pointeur.
!
lgpoint(kul)=.false.
if(cdna == ' ') then
	!
	! On recherchait l'article suivant
	! mais on est en fait en fin de fichier.
	!
	kerr=-10
	return
endif
if(.not.llrew) then
	!
	! Si on est ici, c'est qu'a la fois on n'a pas rencontre
	! l'article demande et que le fichier n'etait pas
	! rembobine. Il est desormais rembobine, et on tente
	! a nouveau la recherche de l'article demande.
	!
	llrew=.true.
	if(nmes(kul) >= 2) &
& 		print*,'LFAIPOS/RECHERCHE de ',cdna(1:ildna) &
& 		,': fin du fichier lu et rebobinage...'
	goto 100
endif
!
! L'article demande n'existe pas dans le fichier.
! Testons si le niveau d'erreur en fin de recherche
! est compatible avec la poursuite du programme.
!
if(kerr /= 0.and.lgerf(kul)) then
	!
	! Le niveau d'erreur doit provoquer un ABORT.
	!
	if(cllang() == 'FRA') then
		print*,'LFAIPOS/ERREUR: article ',cdna(1:ildna) &
& 			,' inexistant!...'
	else
		print*,'LFAIPOS/ERROR: article ',cdna(1:ildna) &
& 			,' not found!...'
	endif
	call lfa_print_file(kul)
	stop 'call abort'
elseif(kerr /= 0.and.nmes(kul) >= 1) then
	!
	! Le niveau d'erreur doit provoquer un message d'alerte.
	!
	if(kerr == -1.and.nmes(kul) >= 1) then
		ildna=len_trim(cdna)
		print*,'LFAIPOS/ATTENTION: article ' &
& 			,cdna(1:ildna),' inexistant!...'
	endif
endif
end
subroutine lfaitype(ktype,cdtype)
! --------------------------------------------------------------
! **** *LFAITYPE* Type en clair de donnee LFA.
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
! ktype: entier definissant le type.
! En sortie:
! cdtype: chaine definissant le type (plus clair pour l'utilisateur).
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) ktype
character*(*) cdtype
character*3 cllang
if(ktype == 1) then
	! type=1: reel  8 octets.
	cdtype='R8'
elseif(ktype == 3) then
	! type=3: reel  4 octets.
	cdtype='R4'
elseif(ktype == 4) then
	! type=4: entier 8 octets.
	cdtype='I8'
elseif(ktype == 2) then
	! type=2: entier 4 octets.
	cdtype='I4'
elseif(ktype < 0) then
	cdtype='C '
else
	if(cllang() == 'FRA') then
		print*,'LFAITYPE/ERREUR: type de donnee inconnu: ',ktype,'!...'
		stop 'call abort'
	else
		print*,'LFAITYPE/ERROR: unknown data type: ',ktype,'!...'
		stop 'call abort'
	endif
endif
end
subroutine lfaiunassign(kul)
#ifdef cray
! --------------------------------------------------------------
! **** *lfaiunassign* Desassignation IEEE d'un fichier Cray.
! **** *lfaiunassign* Unassign IEEE of a Cray file.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
!
! La presente routine ne sert que dans le cas
! de la machine Cray.
! Il faut supprimer
! l'ordre assign, a telle fin que les read/write
! sur le present fichier, effectues par les futurs programmes fortran,
! ne transductent plus.
!
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,ilnomf
character*200 clnomf,classign
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfaiunassign: desassignation de l''UL ',kul
endif
!
! Nom du fichier.
!
clnomf=cgnomf(kul)
ilnomf=len_trim(clnomf)
!
! Desassignation.
!
write(classign,fmt='(2a)') 'assign -R f:',clnomf(1:ilnomf)
call assign(classign)
#endif
end
subroutine lfalaf(kul,kulout)
! --------------------------------------------------------------------------
! **** *LFALAF* Liste des articles d'un fichier LFA.
! **** *LFALAF* Article list of a LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul             unite logique du fichier.
! kulout          unite logique sur laquelle sortir la liste.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul             logical unit of LFA file.
! kulout          logical unit on which print out the list.
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kulout,iart,ilong,ierr,ilna
integer(kind=jpintusr) iversion
integer(kind=jpintesb) iversionesb
character*200 clna,cltype
#include"lfayom.h"
if(lglang) then
	write(kulout,'(3a)') 'LFALAF du fichier de nom ' &
& 		,cgnomf(kul)(1:index(cgnomf(kul),' ')),':'
else
	write(kulout,'(3a)') 'LFALAF from file ' &
& 		,cgnomf(kul)(1:index(cgnomf(kul),' ')),':'
endif
!
! -------------------------------------------------
! On rembobine le fichier.
! -------------------------------------------------
!
rewind(kul)
read(kul) iversionesb
iversion=iversionesb
!
! Position du pointeur.
!
lgpoint(kul)=.false.
!
! -------------------------------------------------
! On lit le fichier LFA sequentiellement jusqu'a la fin.
! -------------------------------------------------
!
iart=0
  100 continue
clna=' '
!
! -------------------------------------------------
! Avancee d'un article dans le fichier LFA.
! -------------------------------------------------
!
call lfacas(kul,clna,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! On n'est pas en fin de fichier.
	! On avance d'un article.
	!
	call lfaavan(kul)
	iart=iart+1
	!
	! Sortie des renseignements sur l'article.
	!
	ilna=len_trim(clna)
	if(lglang) then
		write(kulout,'(3a,i9,2a)') &
& 			'Type |',cltype(1:2) &
& 			,'| Longueur ',ilong &
& 			,' | ',clna(1:ilna)
	else
		write(kulout,'(3a,i9,2a)') &
& 			'Type |',cltype(1:2) &
& 			,'| Length   ',ilong &
& 			,' | ',clna(1:ilna)
	endif
	!
	! Lecture de la suite du fichier.
	!
	goto 100
endif
!
! -------------------------------------------------
! On remet le fichier au debut.
! -------------------------------------------------
!
rewind(kul)
read(kul) iversionesb
iversion=iversionesb
!
! Position du pointeur.
!
lgpoint(kul)=.false.
end
subroutine lfalaft(kul,cdlis,kdlis,knlis)
! --------------------------------------------------------------------------
! **** *LFALAFT* Liste des articles d'un fichier LFA sur tableau de caracteres.
! **** *LFALAFT* Article list of a LFA file, on an array.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul             unite logique du fichier.
! kdlis           dimension physique du tableau cdlis.
! En sortie:
! knlis           nombre d'articles du fichier.
! Ce nombre est egalement le nombre d'elements ecrits sur cdlis.
! cdlis(1, ..., knlis) nom des articles du fichier.
! --------------------------------------------------------------------------
! Input:
! kul            logical unit of LFA file.
! kdlis          physical dimension of array cdlis.
! Output:
! knlis          number of articles on the file.
! This number is also the number of elements written on cdlis.
! cdlis(1, ..., knlis) article names.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdlis,knlis,ilong,ierr
integer(kind=jpintusr) iversion
integer(kind=jpintesb) iversionesb
character*200 clna
character*2 cltype
character*(*) cdlis(kdlis)
character*3 cllang
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfalaft: entree.'
endif
!
! -------------------------------------------------
! On rembobine le fichier.
! -------------------------------------------------
!
rewind(kul)
read(kul) iversionesb
iversion=iversionesb
!
! Position du pointeur.
!
lgpoint(kul)=.false.
!
! -------------------------------------------------
! On lit le fichier LFA sequentiellement jusqu'a la fin.
! -------------------------------------------------
!
knlis=0
  100 continue
clna=' '
!
! -------------------------------------------------
! Avancee d'un article dans le fichier LFA.
! -------------------------------------------------
!
call lfacas(kul,clna,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! On n'est pas en fin de fichier.
	! On avance d'un article.
	!
	call lfaavan(kul)
	knlis=knlis+1
	if(knlis > kdlis) then
		if(cllang() == 'FRA') then
			print*,'LFALAFT/ERREUR: trop d''articles dans le fichier!...'
			print*,'Recompiler!...'
			print*,knlis,kdlis
			call lfa_print_file(kul)
			stop 'call abort'
		else
			print*,'LFALAFT/ERROR: too many articles in file!...'
			print*,'Recompile!...'
			print*,knlis,kdlis
			call lfa_print_file(kul)
			stop 'call abort'
		endif
	endif
	cdlis(knlis)=clna
	!
	! Lecture de la suite du fichier.
	!
	goto 100
endif
end
subroutine lfalecc(kul,cdna,kdimb,cdcar,klong,kerr)
! --------------------------------------------------------------------------
! **** *LFALECC* Lecture de caracteres sur fichier LFA.
! **** *LFALECC* Read character data on LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul              unite logique du fichier.
! cdna             nom de l'article.
! kdimb            dimension du tableau cdcar.
! En sortie:
! klong            nombre de chaines de caracteres lues.
! cdcar(1,klong)   chaines lues.
! kerr             indicateur d'erreur:
! +----------+-----------------------------------------------------+
! | Valeur   |             Signification                           |
! +----------+-----------------------------------------------------+
! | kerr=  0 | Tout est OK!                                        |
! | kerr= -1 | Article inexistant                                  |
! | kerr= -6 | Article plus long que le tableau devant le recevoir |
! | kerr= -8 | Mauvais type de donnees (reelles, entieres, car.)   |
! +----------+-----------------------------------------------------+
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             article name.
! kdimb            physical dimension of array cdcar.
! Output:
! klong            number of character elements read.
! cdcar(1,klong)   character elements read.
! kerr             error indicator:
! +----------+-----------------------------------------------------+
! | Value    |             Meaning                                 |
! +----------+-----------------------------------------------------+
! | kerr=  0 | Everything is OK!                                   |
! | kerr= -1 | Article inexistant                                  |
! | kerr= -6 | Article bigger than array supposed to receive it    |
! | kerr= -8 | Wrong data type (real, integer, char.)              |
! +----------+-----------------------------------------------------+
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdimb,klong,kerr,ilna,itype &
& 	,itailtab,itailfic,iprod
character*(*) cdna
character*(*) cdcar(kdimb)
character*3 cllang
#include"lfayom.h"
ilna=len_trim(cdna)
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfalecc: lecture de ',cdna
endif
klong=0
!
! Position du pointeur fichier sur l'article dans le LFA.
!
call lfaipos(kul,cdna,kerr,itype,klong)
if(kerr /= 0) then
	!
	! Erreur retournee par lfaipos.
	! On la transmet a l'appelant.
	!
	return
elseif(itype >= 0) then
	!
	! L'article cherche n'est pas de type caractere!...
	!
	if(lgerf(kul)) then
		if(cllang() == 'FRA') then
			print*,'LFALECC/ERREUR: l''article ',cdna(1:ilna) &
& 				,' n''est pas de type caractere!...'
			call lfa_print_file(kul)
			stop 'call abort'
		else
			print*,'LFALECC/ERROR: the article ',cdna(1:ilna) &
& 				,' is not character type!...'
			call lfa_print_file(kul)
			stop 'call abort'
		endif
	else
		if(cllang() == 'FRA') then
			print*,'LFALECC/ATTENTION: l''article ',cdna(1:ilna) &
& 				,' n''est pas de type caractere!...'
			kerr=-8
			return
		else
			print*,'LFALECC/WARNING: the article ',cdna(1:ilna) &
& 				,' is not character type!...'
			kerr=-8
			return
		endif
	endif
else
	!
	! L'article existe et a le bon type.
	! On verifie que la dimension du tableau
	! de caracteres permet de recevoir les donnees
	! du fichier LFA.
	!
	if(klong > kdimb) then
		!
		! L'article est plus long que la dimension
		! du tableau de caracteres.
		!
		if(lgerf(kul)) then
			if(cllang() == 'FRA') then
				print*,'LFALECC/ERREUR: article ',cdna(1:ilna) &
& 					,' plus long que le tableau devant le recevoir!...'
				print*,'En effet article de ',klong &
& 					,' elements, or tableau de ',kdimb,' elements.'
			call lfa_print_file(kul)
				stop 'call abort'
			else
				print*,'LFALECC/ERROR: article ',cdna(1:ilna) &
& 					,' longer than the array supposed to receive it!...'
				print*,'Article with ',klong &
& 					,' elements, array with ',kdimb,' elements.'
			call lfa_print_file(kul)
				stop 'call abort'
			endif
		else
			if(cllang() == 'FRA') then
				print*,'LFALECC/ATTENTION: article ',cdna(1:ilna) &
& 					,' plus long que le tableau devant le recevoir!...'
				kerr=-6
				return
			else
				print*,'LFALECC/WARNING: article ',cdna(1:ilna) &
& 					,' longer than the array supposed to receive it!...'
				kerr=-6
				return
			endif
		endif
	endif
	!
	! On verifie que la taille en caracteres
	! de chaque element du tableau cdcar
	! permet de recevoir les donnees du fichier LFA.
	!
	itailtab=len(cdcar(1)) ! taille de chaque element du tableau cdcar.
	itailfic=-itype ! taille de chaque element dans le fichier.
	if(itailfic > itailtab) then
		!
		! L'article est plus long que la taille
		! du tableau de caracteres.
		!
		if(lgerf(kul)) then
			if(cllang() == 'FRA') then
				print*,'LFALECC/ERREUR: l''article ',cdna(1:ilna) &
& 					,' est constitue d''elements de taille' &
& 					,' plus longue que celle du tableau devant les recevoir!...'
				print*,'En effet, fichier: elements de taille ' &
& 					,itailfic,', alors que tableau: ',itailtab
				call lfa_print_file(kul)
				stop 'call abort'
			else
				print*,'LFALECC/ERROR: the article elements',cdna(1:ilna) &
& 					,' are longer' &
& 					,' than those from array supposed to receive them!...'
				print*,'File: length of elements ' &
& 					,itailfic,', array: ',itailtab
				call lfa_print_file(kul)
				stop 'call abort'
			endif
		else
			if(cllang() == 'FRA') then
				print*,'LFALECC/ATTENTION: l''article ',cdna(1:ilna) &
& 					,' est constitue d''elements de taille' &
& 					,' plus longue que celle du tableau' &
& 					,' devant les recevoir!...'
				kerr=-6
				return
			else
				print*,'LFALECC/WARNING: the article elements ',cdna(1:ilna) &
& 					,' are longer' &
& 					,' than those from array supposed to receive them!...'
				kerr=-6
				return
			endif
		endif
	endif
	!
	! Test de la position du pointeur.
	!
	if(.not.lgpoint(kul)) then
		!
		! Le pointeur est avant une autodocumentation.
		! C'est qu'il y a eu un probleme en amont.
		!
		if(cllang() == 'FRA') then
			print*,'LFAILECC/ERREUR: pointeur non positionne ' &
& 				,'avant des donnees!...'
			call lfa_print_file(kul)
			stop 'call abort'
		else
			print*,'LFAILECC/ERROR: pointer location not before ' &
& 				,'data!...'
			call lfa_print_file(kul)
			stop 'call abort'
		endif
	endif
	!
	! Les tailles sont OK. On va lire sur le fichier LFA.
	!
	if(nversion(kul) == 8) then
		!
		! Version du logiciel dans laquelle
		! les caracteres etaient ecrits
		! sous forme leur suite ASCII d'entiers.
		!
		iprod=itailfic*klong ! nombre total de caracteres sur l'ensemble du tableau.
		call lfaileccloc8(kul,cdcar,kdimb,itailfic,klong,iprod)
	else
		!
		! Version du logiciel dans laquelle
		! les caracteres sont ecrits
		! sous forme d'un write implicite
		! du tableau de caracteres.
		!
		call lfaileccloc(kul,cdcar,kdimb,itailfic,klong)
	endif
endif
end
subroutine lfaleci(kul,cdna,kdimb,kentier,klong,kerr)
! --------------------------------------------------------------------------
! **** *LFALECI* Lecture d'entiers sur fichier LFA.
! **** *LFALECI* Read integer data on LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul              unite logique du fichier.
! cdna             nom de l'article.
! kdimb            dimension du tableau kentier.
! En sortie:
! klong            nombre d'entiers lus.
! kentier(1,klong) entiers lus.
! kerr             indicateur d'erreur:
! +----------+-----------------------------------------------------+
! | Valeur   |             Signification                           |
! +----------+-----------------------------------------------------+
! | kerr=  0 | Tout est OK!                                        |
! | kerr= -1 | Article inexistant                                  |
! | kerr= -6 | Article plus long que le tableau devant le recevoir |
! | kerr= -8 | Mauvais type de donnees (reelles, entieres, car.)   |
! +----------+-----------------------------------------------------+
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             article name.
! kdimb            physical dimension of array kentier.
! Output:
! klong            number of integer elements read.
! kentier(1,klong) integer elements read.
! kerr             error indicator:
! +----------+-----------------------------------------------------+
! | Value    |             Meaning                                 |
! +----------+-----------------------------------------------------+
! | kerr=  0 | Everything is OK!                                   |
! | kerr= -1 | Article inexistant                                  |
! | kerr= -6 | Article bigger than array supposed to receive it    |
! | kerr= -8 | Wrong data type (real, integer, char.)              |
! +----------+-----------------------------------------------------+
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdimb,klong,kerr,ilna,itype &
& 	,jlong,iprec
character*2 cltype
character*(*) cdna
integer(kind=jpintusr) kentier(kdimb)
character*3 cllang
#include"lfayom.h"
#include"lfadoc.h"
ilna=len_trim(cdna)
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfaleci: lecture de ',cdna
endif
klong=0
!
! Position du pointeur fichier sur l'article dans le LFA.
!
call lfaipos(kul,cdna,kerr,itype,klong)
call lfaitype(itype,cltype)
cgdoc=cltype//'-'
if(kerr /= 0) then
	!
	! Erreur retournee par lfaipos.
	! On la transmet a l'appelant.
	!
	return
elseif(cltype(1:1) /= 'I') then
	!
	! L'article cherche n'est pas de type entier!...
	!
	if(lgerf(kul)) then
		if(cllang() == 'FRA') then
			print*,'LFALECI/ERREUR: l''article ',cdna(1:ilna) &
& 				,' n''est pas de type entier!...'
			call lfa_print_file(kul)
			stop 'call abort'
		else
			print*,'LFALECI/ERROR: the article ',cdna(1:ilna) &
& 				,' is not integer type!...'
			call lfa_print_file(kul)
			stop 'call abort'
		endif
	else
		if(cllang() == 'FRA') then
			print*,'LFALECI/ATTENTION: l''article ',cdna(1:ilna) &
& 				,' n''est pas de type entier!...'
			kerr=-8
			return
		else
			print*,'LFALECI/WARNING: the article ',cdna(1:ilna) &
& 				,' is not integer type!...'
			kerr=-8
			return
		endif
	endif
else
	!
	! L'article existe et a le bon type.
	! On verifie que la dimension du tableau
	! permet de recevoir les donnees
	! du fichier LFA.
	!
	if(klong > kdimb) then
		!
		! L'article est plus long que la dimension
		! du tableau de caracteres.
		!
		if(lgerf(kul)) then
			if(cllang() == 'FRA') then
				print*,'LFALECI/ERREUR: article ',cdna(1:ilna) &
& 					,' plus long que le tableau devant le recevoir!...'
				print*,'En effet article de ',klong &
& 					,' elements, or tableau de ' &
& 					,kdimb,' elements.'
				call lfa_print_file(kul)
				stop 'call abort'
			else
				print*,'LFALECI/ERROR: article ',cdna(1:ilna) &
& 					,' longer than array supposed to receive it!...'
				print*,'Article ',klong &
& 					,' elements, array ' &
& 					,kdimb,' elements.'
				call lfa_print_file(kul)
				stop 'call abort'
			endif
		else
			print*,'LFALECI/ATTENTION: article ',cdna(1:ilna) &
& 				,' plus long que le tableau devant le recevoir!...'
			kerr=-6
			return
		endif
	endif
	!
	! Test de la position du pointeur.
	!
	if(.not.lgpoint(kul)) then
		!
		! Le pointeur est avant une autodocumentation.
		! C'est qu'il y a eu un probleme en amont.
		!
		if(cllang() == 'FRA') then
			print*,'LFAILECI/ERREUR: pointeur non positionne' &
& 				,' avant des donnees!...'
			call lfa_print_file(kul)
			stop 'call abort'
		else
			print*,'LFAILECI/ERROR: pointer location not before' &
& 				,' data!...'
			call lfa_print_file(kul)
			stop 'call abort'
		endif
	endif
	!
	! La taille est OK. Lecture sur le LFA.
	!
	!
	! iprec: taille en octets des entiers du fichier.
	!
	if(itype == 4) then
		iprec=8
	elseif(itype == 2) then
		iprec=4
	else
		print*,'LFALECI/ERREUR: type de donnee non attendu!...'
		print*,itype
		call lfa_print_file(kul)
		stop 'call abort'
	endif
#ifdef dblieee
	!
	! Sur certains Cray, la precision de sortie d'une lecture
	! en IEEE est celle du fichier multipliee
	! par 2; il faut donc ici multiplier
	! par 2 pour garantir le resultat.
	!
	iprec=2*iprec
#endif
	if(iprec == jpintusr) then
		!
		! La precision des entiers a lire
		! est celle des entiers passes en argument.
		! Lecture directe donc.
		!
		read(kul) (kentier(jlong),jlong=1,klong)
	elseif(iprec == 4) then
		!
		! La precision des entiers a lire est de 4 octets.
		!
		call lfaileci4(kul,klong,kentier)
	elseif(iprec == 8) then
		!
		! La precision des entiers a lire est de 8 octets.
		!
		call lfaileci8(kul,klong,kentier)
	else
		print*,'LFALECI/ERREUR: precision d''entree impossible: ',iprec
		call lfa_print_file(kul)
		stop 'call abort'
	endif
	!
	! Position du pointeur.
	!
	lgpoint(kul)=.false.
endif
end
subroutine lfalecr(kul,cdna,kdimb,preel,klong,kerr)
! --------------------------------------------------------------------------
! **** *LFALECR* Lecture de reels sur fichier LFA.
! **** *LFALECR* Read real data on LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul              unite logique du fichier.
! cdna             nom de l'article.
! kdimb            dimension du tableau preel.
! En sortie:
! klong            nombre de reels lus.
! preel(1,klong)   reels lus.
! kerr             indicateur d'erreur:
! +----------+-----------------------------------------------------+
! | Valeur   |             Signification                           |
! +----------+-----------------------------------------------------+
! | kerr=  0 | Tout est OK!                                        |
! | kerr= -1 | Article inexistant                                  |
! | kerr= -6 | Article plus long que le tableau devant le recevoir |
! | kerr= -8 | Mauvais type de donnees (reelles, entieres, car.)   |
! +----------+-----------------------------------------------------+
! --------------------------------------------------------------------------
! Input:
! kul              logical unit of LFA file.
! cdna             article name.
! kdimb            physical dimension of array preel.
! Output:
! klong            number of real elements read.
! preel(1,klong)   real elements read.
! kerr             error indicator:
! +----------+-----------------------------------------------------+
! | Value    |             Meaning                                 |
! +----------+-----------------------------------------------------+
! | kerr=  0 | Everything is OK!                                   |
! | kerr= -1 | Article inexistant                                  |
! | kerr= -6 | Article bigger than array supposed to receive it    |
! | kerr= -8 | Wrong data type (real, integer, char.)              |
! +----------+-----------------------------------------------------+
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdimb,klong,kerr,ilna,itype &
& 	,itypenouv,jlong,iprec
character*2 cltype
character*(*) cdna
real(kind=jpreeusr) preel(kdimb)
character*3 cllang
#include"lfayom.h"
#include"lfadoc.h"
ilna=len_trim(cdna)
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfalecr: lecture de ',cdna
endif
klong=0
!
! Position du pointeur fichier sur l'article dans le LFA.
!
call lfaipos(kul,cdna,kerr,itype,klong)
!
! Type en clair de l'article.
!
call lfaitype(itype,cltype)
cgdoc=cltype//'-'
if(kerr /= 0) then
	!
	! Erreur retournee par lfaipos.
	! On la transmet a l'appelant.
	!
	return
elseif(cltype(1:1) /= 'R') then
	!
	! L'article cherche n'est pas de type reel!...
	!
	if(lgerf(kul)) then
		if(cllang() == 'FRA') then
			print*,'LFALECR/ERREUR: l''article ',cdna(1:ilna) &
& 				,' n''est pas de type reel!...'
			call lfa_print_file(kul)
			stop 'call abort'
		else
			print*,'LFALECR/ERROR: the article ',cdna(1:ilna) &
& 				,' is not real type!...'
			call lfa_print_file(kul)
			stop 'call abort'
		endif
	else
		if(cllang() == 'FRA') then
			print*,'LFALECR/ATTENTION: l''article ',cdna(1:ilna) &
& 				,' n''est pas de type reel!...'
			kerr=-8
			return
		else
			print*,'LFALECR/WARNING: the article ',cdna(1:ilna) &
& 				,' is not real type!...'
			kerr=-8
			return
		endif
	endif
else
	!
	! L'article existe et a le bon type.
	! On verifie que la dimension du tableau
	! permet de recevoir les donnees
	! du fichier LFA.
	!
	if(klong > kdimb) then
		!
		! L'article est plus long que la dimension
		! du tableau de caracteres.
		!
		if(lgerf(kul)) then
			if(cllang() == 'FRA') then
				print*,'LFALECR/ERREUR: article ',cdna(1:ilna) &
& 					,' plus long que le tableau devant le recevoir!...'
				print*,'En effet article de ',klong &
& 					,' elements, or tableau de ' &
& 					,kdimb,' elements.'
				call lfa_print_file(kul)
				stop 'call abort'
			else
				print*,'LFALECR/ERROR: article ',cdna(1:ilna) &
& 					,' longer than array supposed to receive it!...'
				print*,'Article ',klong &
& 					,' elements, array ' &
& 					,kdimb,' elements.'
				call lfa_print_file(kul)
				stop 'call abort'
			endif
		else
			print*,'LFALECR/ATTENTION: article ' &
& 				,cdna(1:ilna),' plus long que le tableau' &
& 				,' devant le recevoir!...'
			kerr=-6
			return
		endif
	endif
	!
	! Test de la position du pointeur.
	!
	if(.not.lgpoint(kul)) then
		!
		! Le pointeur est avant une autodocumentation.
		! C'est qu'il y a eu un probleme en amont.
		!
		print*,'LFAILECR/ERREUR: pointeur non positionne ' &
& 			,'avant des donnees!...'
		call lfa_print_file(kul)
		stop 'call abort'
	endif
	!
	! La taille est OK. Lecture sur le LFA.
	!
	!
	! iprec: taille en octets des reels du fichier.
	!
	if(itype == 1) then
		iprec=8
	elseif(itype == 3) then
		iprec=4
	else
		print*,'LFALECR/ERREUR: type de donnee non attendu!...'
		print*,itype
		call lfa_print_file(kul)
		stop 'call abort'
	endif
#ifdef dblieee
	!
	! Sur certains Cray, la precision de sortie d'une lecture
	! en IEEE est celle du fichier multipliee
	! par 2; il faut donc ici multiplier
	! par 2 pour garantir le resultat.
	!
	iprec=2*iprec
#endif
	if(iprec == jpreeusr) then
		!
		! La precision des reels a lire
		! est celle des reels passes en argument.
		! Lecture directe donc.
		!
		read(kul) (preel(jlong),jlong=1,klong)
	elseif(iprec == 4) then
		!
		! La precision des reels a lire est de 4 octets.
		!
		call lfailecr4(kul,klong,preel)
	elseif(iprec == 8) then
		!
		! La precision des reels a lire est de 8 octets.
		!
		call lfailecr8(kul,klong,preel)
	else
		print*,'LFALECR/ERREUR: precision d''entree impossible: ',iprec
		call lfa_print_file(kul)
		stop 'call abort'
	endif
	!
	! Position du pointeur.
	!
	lgpoint(kul)=.false.
endif
end
subroutine lfames(kul,kmes)
! --------------------------------------------------------------------------
! **** *LFAMES* Niveau de messagerie du logiciel LFA.
! **** *LFAMES* Print out level of LFA software.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul         unite logique du fichier.
! kmes        niveau de messagerie:
! si 0 aucun message sorti par le logiciel LFA.
! si 1 messages d'ATTENTION et d'ERREUR sorties.
! si 2 LFA est bavard (a reserver au debug de LFA...).
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul         logical unit of LFA file.
! kmes        print out level:
! if 0 no message print out.
! if 1 WARNING or ERROR messages print out.
! if 2 many comments print out (LFA debug mode only...).
! Output:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kmes
#include"lfayom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfames: niveau de messagerie de l''unite logique ' &
& 		,kul,' porte a ',kmes
endif
nmes(kul)=kmes
end
subroutine lfaminm(kul)
! --------------------------------------------------------------
! **** *LFAMINM* Extrema de tous les articles d'un fichier LFA.
! **** *LFAMINM* Extrema of all articles of a given LFA file.
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
! kul unite logique du fichier LFA d'entree.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
! Input:
! kul logical unit of LFA file.
! Output:
! Extrema on standard output.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,ilong,ierr
integer(kind=jpintusr) iversion
integer(kind=jpintesb) iversionesb
character*200 clna
#include"lfayom.h"
character*2 cltype
if(lglang) then
	write(*,'(3a)') 'LFAMINM du fichier ' &
& 		,cgnomf(kul)(1:index(cgnomf(kul),' ')),':'
else
	write(*,'(3a)') 'LFAMINM from file ' &
& 		,cgnomf(kul)(1:index(cgnomf(kul),' ')),':'
endif
rewind(kul)
read(kul) iversionesb
iversion=iversionesb
!
! Position du pointeur.
!
lgpoint(kul)=.false.
  100 continue
!
! -------------------------------------------------
! Avancee d'un article dans le fichier LFA.
! -------------------------------------------------
!
clna=' '
call lfacas(kul,clna,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! On n'est pas en fin de fichier.
	!
	if(cltype(1:1) == 'R') then
		!
		! Article de type reel.
		!
		call lfaiminmr(kul,clna,cltype,ilong)
	elseif(cltype(1:1) == 'I') then
		!
		! Article de type entier.
		!
		call lfaiminmi(kul,clna,cltype,ilong)
	elseif(cltype(1:1) == 'C') then
		!
		! Article de type caractere.
		!
		call lfaiminmc(kul,clna,cltype,ilong)
	else
		print*,'LFAMINM/ATTENTION: type de donnee inconnu!...'
		print*,cltype
	endif
	goto 100
endif
end
subroutine lfaouv(kul,cdnomf,cdtypo)
! --------------------------------------------------------------------------
! **** *LFAOUV* Ouverture de fichier LFA.
! **** *LFAOUV* Open a LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul         unite logique du fichier.
! cdnomf      nom du fichier.
! cdtypo      type d'ouverture: 'R' READ, 'W' WRITE, 'A' APPEND, 'S' SCRATCH.
! En sortie:
! --------------------------------------------------------------------------
! Input:
! kul         logical unit of LFA file.
! cdnomf      file name.
! cdtypo      opening type: 'R' READ, 'W' WRITE, 'A' APPEND, 'S' SCRATCH.
! Output:
! --------------------------------------------------------------------------
use yomlfa, only: cgfnom, jpmaxul
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,ilnomf,iltypo
integer(kind=jpintusr) iversion
integer(kind=jpintesb) iversionesb
logical llex,lllfa
character*(*) cdnomf
character*(*) cdtypo
#ifdef cray
character*200 classign
#endif
character*3 cllang
#include"lfayom.h"
ilnomf=len_trim(cdnomf)
iltypo=len_trim(cdtypo)
iversion=21
!
! Type d'ouverture du fichier.
!
cgtypo(kul)=cdtypo
!
! Niveau de messagerie
! (valeur par defaut).
!
nmes(kul)=1
!
! Niveau d'erreur fatale par defaut.
!
call lfaerf(kul,.true.)
!
! Sauvegarde du nom du fichier.
!
cgnomf(kul)=cdnomf
!
! Precision d'ecriture par defaut.
!
nprecr(kul)=jpreedef
npreci(kul)=jpintdef
if(cllang() == 'FRA') then
	lglang=.true.
else
	lglang=.false.
endif
!
! Position du pointeur.
!
lgpoint(kul)=.false.
if(cdtypo(1:iltypo) == 'R') then
	!
	! READ.
	! Le fichier existe-t-il?
	!
	inquire(file=cdnomf,exist=llex)
	if(.not.llex) then
		if(cllang() == 'FRA') then
			print*,'LFAOUV/ERREUR: fichier d''entree inexistant!...'
		else
			print*,'LFAOUV/ERROR: input file not found!...'
		endif
		print*,cdnomf(1:ilnomf)
		stop 'call abort'
	endif
	!
	! Le fichier est-il bien LFA?
	!
	call lfatest(kul,cdnomf,lllfa)
	if(.not.lllfa) then
		if(cllang() == 'FRA') then
			print*,'LFAOUV/ERREUR: incompatibilite fichier/logiciel.'
		else
			print*,'LFAOUV/ERROR: file/software inconsistency!...'
		endif
		print*,cdnomf(1:ilnomf)
		stop 'call abort'
	endif
	!
	! Le fichier existe et est bien LFA.
	! On l'ouvre.
	!
#ifdef cray
	!
	! Dans le cas du Cray, il faut executer un ordre assign:
	! en fortran, tous les read transductent alors du format IEEE
	! vers CRAY, et les write dans l'autre sens.
	!
	write(classign,fmt='(2a)') 'assign -N ieee -F f77 f:',cdnomf(1:ilnomf)
	call assign(classign)
#endif
	open(kul,file=cdnomf,form='unformatted',status='old')
	read(kul) iversionesb
	iversion=iversionesb
	!
	! On sauvegarde la version qui a produit le fichier lu.
	!
	nversion(kul)=iversion
	!
	! Position du pointeur.
	!
	lgpoint(kul)=.false.
elseif(cdtypo(1:iltypo) == 'W') then
	!
	! WRITE.
	!
#ifdef cray
	!
	! Dans le cas du Cray, il faut executer un ordre assign:
	! en fortran, tous les read transductent alors du format IEEE
	! vers CRAY, et les write dans l'autre sens.
	!
	write(classign,fmt='(2a)') 'assign -N ieee -F f77 f:',cdnomf(1:ilnomf)
	call assign(classign)
#endif
	open(kul,file=cdnomf,form='unformatted',status='replace')
	!
	! Ecriture de l'en-tete du fichier LFA.
	!
	iversionesb=iversion
	write(kul) iversionesb
	!
	! On sauvegarde la version qui a produit le fichier lu.
	!
	nversion(kul)=iversion
elseif(cdtypo(1:iltypo) == 'S') then
	!
	! SCRATCH.
	!
#ifdef cray
	!
	! Dans le cas du Cray, il faut executer un ordre assign:
	! en fortran, tous les read transductent alors du format IEEE
	! vers CRAY, et les write dans l'autre sens.
	!
	write(classign,fmt='(2a)') 'assign -N ieee -F f77 f:',cdnomf(1:ilnomf)
	call assign(classign)
#endif
	open(kul,form='unformatted',status='scratch')
	!
	! Ecriture de l'en-tete du fichier LFA.
	!
	iversionesb=iversion
	write(kul) iversionesb
	!
	! On sauvegarde la version qui a produit le fichier lu.
	!
	nversion(kul)=iversion
elseif(cdtypo(1:iltypo) == 'A') then
	!
	! APPEND.
	!
#ifdef cray
	!
	! Dans le cas du Cray, il faut executer un ordre assign:
	! en fortran, tous les read transductent alors du format IEEE
	! vers CRAY, et les write dans l'autre sens.
	!
	write(classign,fmt='(2a)') 'assign -N ieee -F f77 f:',cdnomf(1:ilnomf)
	call assign(classign)
#endif
	open(kul,file=cdnomf,form='unformatted',status='old',position='append')
	!open(kul,file=cdnomf,form='unformatted',status='old',access='append') ! f77 seulement.
else
	if(cllang() == 'FRA') then
		print*,'LFAOUV/ERREUR: type d''ouverture inconnu!...'
	else
		print*,'LFAOUV/ERROR: unknown open type!...'
	endif
	print*,cdtypo(1:iltypo)
	!
	! On sauvegarde la version qui a produit le fichier lu.
	!
	nversion(kul)=iversion
	call lfa_print_file(kul)
	stop 'call abort'
endif
!
!-------------------------------------------------
! Sauvegarde dans un module du nom en clair du fichier,
! aux seules fins d'impressions en clair pour l'utilisateur.
!-------------------------------------------------
!
if(kul > jpmaxul) then
	if(cllang() == 'FRA') then
		print*,'LFAOUV/ERREUR: recompiler le logiciel LFA avec jpmaxul plus grand!...'
	else
		print*,'LFAOUV/ERROR: recompile LFA software with a greater value of jpmaxul!...'
	endif
	call lfa_print_file(kul)
	stop 'call abort'
else
	cgfnom(kul)=cdnomf(1:len_trim(cdnomf))
endif
end
subroutine lfappdemo
! --------------------------------------------------------------
! **** *LFAPPDEMO* Demonstation du logiciel LFA.
! **** *LFAPPDEMO* LFA software demonstration.
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
#include"lfayom.h"
integer(kind=jpintusr) jpr,ir,jr,iul,ilong,ierr,ilc,iprec &
& 	,iulout
character*200 clnomf,clnarc,clnarl,clnaec,clnael,clnac,clnara &
& 	,clnard,clnaed
logical lllfa,llent,llree,llcar
parameter(jpr=8) ! dimension physique.
real(kind=jpreeusr) zreel(jpr),zinterm
integer(kind=jpintusr) ient(jpr)
character*200 clc(jpr)
character*3 cllang
ir=jpr/2 ! dimension logique.
llcar=.true.
llent=.true.
llree=.true.
if(cllang() == 'FRA') then
	clnard='Reels par defaut'
	clnarc='Reels courts'
	clnarl='Reels.Longs'
	clnaed='Entiers defaut'
	clnaec='Entiers courts'
	clnael='Entiers LONGS'
	clnac='Caracteres'
	clnara='Reels_ajoutes'
else
	clnard='Default REAL'
	clnarc='Short REAL'
	clnarl='Long REAL'
	clnaed='Int. by default'
	clnaec='Short.integers'
	clnael='Long_integers'
	clnac='Some char'
	clnara='Appended real'
endif
!
! -------------------------------------------------
! Initialisation des tableaux.
! -------------------------------------------------
!
do jr=1,ir
	zinterm=real(jr)
	zreel(jr)=sqrt(zinterm)
	ient(jr)=jr*jr
	write(clc(jr),fmt='(a,i8)') 'Car ',jr*jr
enddo
!
! -------------------------------------------------
! Ouverture du fichier en mode ecriture.
! -------------------------------------------------
!
iul=23
clnomf='LFA'
call lfaouv(iul,clnomf,'W')
!
! -------------------------------------------------
! Niveau de messagerie.
! -------------------------------------------------
!
! call lfames(iul,2)
if(llree) then
	!
	! -------------------------------------------------
	! Ecriture de reels.
	! -------------------------------------------------
	!
	call lfaecrr(iul,clnard,zreel,ir)
	!
	! -------------------------------------------------
	! Ecriture de reels courts.
	! -------------------------------------------------
	!
	iprec=4
	call lfaprecr(iul,iprec)
	call lfaecrr(iul,clnarc,zreel,ir)
	!
	! -------------------------------------------------
	! Ecriture de reels longs.
	! -------------------------------------------------
	!
	iprec=8
	call lfaprecr(iul,iprec)
	call lfaecrr(iul,clnarl,zreel,ir)
endif
if(llcar) then
	!
	! -------------------------------------------------
	! Ecriture de caracteres.
	! -------------------------------------------------
	!
	call lfaecrc(iul,clnac,clc,ir)
endif
if(llent) then
	!
	! -------------------------------------------------
	! Ecriture d'entiers.
	! -------------------------------------------------
	!
	call lfaecri(iul,clnaed,ient,ir)
	!
	! -------------------------------------------------
	! Ecriture d'entiers courts.
	! -------------------------------------------------
	!
	iprec=4
	call lfapreci(iul,iprec)
	call lfaecri(iul,clnaec,ient,ir)
	!
	! -------------------------------------------------
	! Ecriture d'entiers longs.
	! -------------------------------------------------
	!
	iprec=8
	call lfapreci(iul,iprec)
	call lfaecri(iul,clnael,ient,ir)
endif
!
! -------------------------------------------------
! Liste des articles.
! -------------------------------------------------
!
iulout=6
call lfalaf(iul,iulout)
call lfaminm(iul)
!
! -------------------------------------------------
! Fermeture du fichier.
! -------------------------------------------------
!
call lfafer(iul)
!
! -------------------------------------------------
! Ouverture du fichier en mode ajout.
! -------------------------------------------------
!
iul=23
clnomf='LFA'
call lfaouv(iul,clnomf,'A')
if(llree) then
	!
	! -------------------------------------------------
	! Ecriture de reels.
	! -------------------------------------------------
	!
	call lfaecrr(iul,clnara,zreel,ir)
endif
!
! -------------------------------------------------
! Liste des articles.
! -------------------------------------------------
!
! call lfaminm(iul)
!
! -------------------------------------------------
! Fermeture du fichier.
! -------------------------------------------------
!
call lfafer(iul)
!
! -------------------------------------------------
! Initialisation des tableaux.
! -------------------------------------------------
!
do jr=1,jpr
	zreel(jr)=0.
	ient(jr)=0
	clc(jr)=' '
enddo
!
! -------------------------------------------------
! Ouverture du fichier en mode lecture.
! -------------------------------------------------
!
iul=23
clnomf='LFA'
call lfatest(iul,clnomf,lllfa)
if(lllfa) then
	if(lglang) then
		print*,'Le fichier est de type LFA.'
	else
		print*,'The file is a LFA one.'
	endif
else
	if(lglang) then
		print*,'Le fichier n''est pas de type LFA.'
	else
		print*,'The file is not a LFA one.'
	endif
endif
call lfaouv(iul,clnomf,'R')
!
! -------------------------------------------------
! Niveau de messagerie.
! -------------------------------------------------
!
! call lfames(iul,2)
if(llent) then
	!
	! -------------------------------------------------
	! Lecture d'entiers.
	! -------------------------------------------------
	!
	call lfaleci(iul,clnaed,ir,ient,ilong,ierr)
	do jr=1,ir
		print*,'ient(',jr,')=',ient(jr)
	enddo
endif
if(llcar) then
	!
	! -------------------------------------------------
	! Lecture de caracteres.
	! -------------------------------------------------
	!
	call lfalecc(iul,clnac,ir,clc,ilong,ierr)
	do jr=1,ir
		ilc=len_trim(clc(jr))
		print*,clc(jr)(1:ilc)
	enddo
endif
if(llree) then
	!
	! -------------------------------------------------
	! Lecture de reels.
	! -------------------------------------------------
	!
	call lfalecr(iul,clnard,ir,zreel,ilong,ierr)
	do jr=1,ir
		print*,'zreel(',jr,')=',zreel(jr)
	enddo
endif
!
! -------------------------------------------------
! Test de la gestion d'erreur.
! -------------------------------------------------
!
!
! Mise en mode "permissif".
!
! call lfaerf(iul,.false.)
!
! Lecture d'article inexistant.
!
! call lfalecr(iul,'ACHSO',ir,zreel,ilong,ierr)
! print*,'Demande d''article inexistant: ierr=',ierr
!
! -------------------------------------------------
! Fermeture du fichier.
! -------------------------------------------------
!
call lfafer(iul)
end
subroutine lfapplfac
! --------------------------------------------------------------------------
! **** *LFAPPLFAC* Extraction sur output standard d'un article de fichier LFA.
! **** *LFAPPLFAC* Extract on standard output one LFA article.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   95-03, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
character*200 clnomf,clnomc
integer(kind=jpintusr) iul,iuls,ilong,ierr,ilnomf,inarg
character*2 cltype
character*3 cllang
!
! Saisie de la ligne de commande.
!
inarg=1
call getargp(inarg,clnomf)
inarg=2
call getargp(inarg,clnomc)
if(clnomf == ' '.or.clnomc == ' ') then
	if(cllang() == 'FRA') then
		write(*,*) ' '
		write(*,*) &
& 			'Extraction sur output standard d''un article de fichier LFA.'
		write(*,*) ' '
		write(*,*) 'Utilisation: lfac nomf nomc '
		write(*,*) 'avec'
		write(*,*) '    nomf nom du fichier LFA d''entree.'
		write(*,*) '    nomc nom du champ a extraire.'
		write(*,*) ' '
		stop
	else
		write(*,*) ' '
		write(*,*) &
& 			'Extract on standard output one LFA article.'
		write(*,*) ' '
		write(*,*) 'Usage: lfac FILE ARTICLE'
		write(*,*) 'with'
		write(*,*) '    FILE: LFA file name.'
		write(*,*) '    ARTICLE: article name in the file.'
		write(*,*) ' '
		stop
	endif
endif
!
! Ouverture du fichier.
!
ilnomf=200
iul=7
iuls=17
call lfaouv(iul,clnomf,'R')
!
! Recherche de l'article souhaite.
!
call lfacas(iul,clnomc,cltype,ilong,ierr)
if(ierr == 0) then
	!
	! L'article existe dans le fichier.
	!
	if(cltype(1:1) == 'R') then
		!
		! Article de type reel.
		!
		call lfaaffr(ilong,iul,clnomc)
	elseif(cltype(1:1) == 'I') then
		!
		! Article de type entier.
		!
		call lfaaffi(ilong,iul,clnomc)
	elseif(cltype(1:1) == 'C') then
		!
		! Article de type caractere.
		!
		call lfaaffc(ilong,iul,clnomc)
	else
		print*,'lfac: ERREUR interne: type de champ non prevu!...'
		stop
	endif
	close(iuls)
endif
!
! Fermeture du fichier.
!
call lfafer(iul)
end
subroutine lfapplfacre
! --------------------------------------------------------------
! **** *LFAPPLFACRE* Creation d'un fichier LFA a partir de la ligne de commande.
! **** *LFAPPLFACRE* Create a LFA file from command line.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) jparg,iuls,jarg,iltype,iule &
& 	,inomal,ilvalfic,ientier(1),ientierloc,igol1,iprec,inarg
real(kind=jpreeusr) zreel(1),zreelloc
logical llex
character*200 clfs,clnoma,clvalfic
character*2 cltype
parameter(jparg=20)
character*3 cllang
!
! Saisie de la ligne de commande.
!
inarg=1
call getargp(inarg,clfs)
if(clfs == ' ') then
	if(cllang() == 'FRA') then
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') &
& 			'Creation d''un fichier LFA a partir de la ligne de commande'
		write(*,fmt='(9a)') 'et(ou) de fichier(s) texte.'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') 'Utilisation:'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') &
& 			'lfacre LFA [nom_1 type_1 valfic_1] ... [nom_n type_n valfic_n]'
		write(*,fmt='(a,i3)') 'n doit valoir au plus ',jparg
		write(*,fmt='(9a)') &
& 			'En sortie, le fichier LFA contiendra les n articles'
		write(*,fmt='(9a)') &
& 			'nom_1 a nom_n, dont le type sera type_1 a type_n' &
& 			,' (type: R4, R8, I4, I8 ou C),'
		write(*,fmt='(9a)') &
& 			'et dont le contenu sera valfic_1 a valfic_n:'
		write(*,fmt='(9a)') &
& 			'	- Si valfic_i est un fichier, alors le contenu'
		write(*,fmt='(9a)') &
& 			'	  de ce fichier sera le contenu de l''article nom_i.'
		write(*,fmt='(9a)') &
& 			'	- Si valfic_i n''est pas un fichier, alors c''est la valeur'
		write(*,fmt='(9a)') &
& 			'	  du contenu de l''article de longueur 1 nom_i.'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') 'Exemple:'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') 'cat <<EOF > gol'
		write(*,fmt='(9a)') 'gol1'
		write(*,fmt='(9a)') 'gol2'
		write(*,fmt='(9a)') 'EOF'
		write(*,fmt='(9a)') &
& 			'lfacre LFA RII0 R8 1370. indice C gol annee I4 2006'
		write(*,fmt='(9a)') &
& 			'creera le fichier LFA, contenant trois articles' &
& 			,', l''article reel RII0'
		write(*,fmt='(9a)') &
& 			'(longueur 1), l''article caractere indice (longueur 2),'
		write(*,fmt='(9a)') &
& 			'et l''article entier annee (longueur 1).'
		write(*,fmt='(9a)') ' '
		stop
	else
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') &
& 			'Create a LFA file from command line'
		write(*,fmt='(9a)') 'and(or) from ASCII text file(s).'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') 'Usage:'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') &
& 			'lfacre LFA [article_name_1 type_1 fil_name_1] ' &
& 			,'... [article_name_n type_n fil_name_n]'
		write(*,fmt='(a,i3)') 'n has to be less than ',jparg
		write(*,fmt='(9a)') &
& 			'In output, the LFA file will contain the n articles'
		write(*,fmt='(9a)') &
& 			'article_name_1 to article_name_n,' &
& 			,' which type will be type_1 to type_n' &
& 			,' (type: R4, R8, I4, I8 or C),'
		write(*,fmt='(9a)') &
& 			'and contents of these articles will be ' &
& 			,'fil_name_1 to fil_name_n:'
		write(*,fmt='(9a)') &
& 			'	- If fil_name_i is a file, then its contents'
		write(*,fmt='(9a)') &
& 			'	  will be put in article_name_i article.'
		write(*,fmt='(9a)') &
& 			'	- If fil_name_i is not a file, then it is the value'
		write(*,fmt='(9a)') &
& 			'	  of the one-value article article_name_i.'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') 'Example:'
		write(*,fmt='(9a)') ' '
		write(*,fmt='(9a)') 'cat <<EOF > gol'
		write(*,fmt='(9a)') 'gol1'
		write(*,fmt='(9a)') 'gol2'
		write(*,fmt='(9a)') 'EOF'
		write(*,fmt='(9a)') &
& 			'lfacre LFA RII0 R8 1370. indice C gol year I4 2006'
		write(*,fmt='(9a)') &
& 			'will create the file LFA, containing tree articles' &
& 			,', the real data article RII0'
		write(*,fmt='(9a)') &
& 			'(length 1), the character data article indice (length 2),'
		write(*,fmt='(9a)') &
& 			'and the integer data article year (length 1).'
		write(*,fmt='(9a)') ' '
		stop
	endif
endif
!
! Ouverture du fichier de sortie.
!
iuls=73
call lfaouv(iuls,clfs,'W')
!
! Boucle sur la ligne de commande.
!
do jarg=1,jparg
	inarg=2+3*(jarg-1)
	call getargp(inarg,clnoma)
	inarg=3+3*(jarg-1)
	call getargp(inarg,cltype)
	inarg=4+3*(jarg-1)
	call getargp(inarg,clvalfic)
	if(clnoma == ' ') then
		!
		! Fin de la ligne de commande.
		! Fermeture des fichiers.
		!
		call lfafer(iuls)
		stop
	else
		!
		! Ligne de commande non vide.
		!
		iltype=len_trim(cltype)
		!
		! -------------------------------------------------
		! On force la précision d'écriture des entiers et réels.
		! -------------------------------------------------
		!
		if(cltype(1:1) == 'R') then
			!
			! Article réel.
			!
			read(cltype(2:2),fmt='(i1)') iprec
			call lfaprecr(iuls,iprec)
		elseif(cltype(1:1) == 'I') then
			!
			! Article entier.
			!
			read(cltype(2:2),fmt='(i1)') iprec
			call lfapreci(iuls,iprec)
		endif
		!
		! -------------------------------------------------
		! L'utilisateur fournit-il un fichier d'entrée?
		! -------------------------------------------------
		!
		inquire(file=clvalfic,exist=llex)
		if(llex) then
			!
			! Le fichier existe.
			! On va le lire sur un tableau.
			!
			! Combien d'articles ce fichier comporte-t-il?
			!
			iule=1
			open(iule,file=clvalfic,form='formatted')
			inomal=0
  100 			read(iule,fmt='(a)',end=200)
			inomal=inomal+1
			goto 100
  200 			continue
			rewind(iule)
			!
			! Ce fichier comporte inomal articles.
			!
			if(cltype(1:1) == 'R') then
				!
				! Article de reel.
				!
				call lfaforvlr(iule,iuls,clnoma,inomal)
			elseif(cltype(1:1) == 'I') then
				!
				! Article d'entier.
				!
				call lfaforvli(iule,iuls,clnoma,inomal)
			elseif(cltype(1:1) == 'C') then
				!
				! Article caractere.
				!
				call lfaforvlc(iule,iuls,clnoma,inomal)
			else
				print*,'LFACRE/ERREUR: type d''article inconnu!...'
				print*,cltype
				stop 'call abort'
			endif
			close(iule)
		else
			!
			! Le fichier n'existe pas.
			! On ecrit un article de longueur 1.
			!
			igol1=1
			if(cltype(1:1) == 'R') then
				!
				! Article de reel.
				!
				ilvalfic=len_trim(clvalfic)
				call carree(clvalfic,ilvalfic,zreelloc)
				zreel(1)=zreelloc
				call lfaecrr(iuls,clnoma,zreel,igol1)
			elseif(cltype(1:1) == 'I') then
				!
				! Article d'entier.
				!
				ilvalfic=len_trim(clvalfic)
				igol1=1
				call carint(clvalfic,igol1,ilvalfic,ientierloc)
				ientier(1)=ientierloc
				call lfaecri(iuls,clnoma,ientier,igol1)
			elseif(cltype(1:1) == 'C') then
				!
				! Article caractere.
				!
				call lfaecrc(iuls,clnoma,clvalfic,igol1)
			else
				print*,'LFACRE/ERREUR: type d''article inconnu!...'
				print*,cltype
				stop 'call abort'
			endif
		endif
	endif
enddo
end
subroutine lfapplfalaf
! --------------------------------------------------------------------------
! **** *LFAPPLFALAF* Programme listant les articles de fichiers ASCII autodocumentes.
! **** *LFAPPLFALAF* Get the articles list of a LFA file.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   94-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
character*200 clnomf
integer(kind=jpintusr) iul,iulout,inarg
character*3 cllang
!
! Ouverture du fichier.
!
inarg=1
call getargp(inarg,clnomf)
if(clnomf == ' ') then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Sortie sur output standard de la liste des articles' &
& 			,' d''un fichier LFA.'
		print*,' '
		print*,'Utilisation: lfalaf nomf'
		print*,' '
		stop
	else
		print*,' '
		print*,'Get the articles list of a LFA file.'
		print*,' '
		print*,'Usage: lfalaf FILE'
		print*,' '
		stop
	endif
endif
iul=7
call lfaouv(iul,clnomf,'R')
!
! Contenu du fichier.
!
iulout=6
call lfalaf(iul,iulout)
!
! Fermeture du fichier.
!
call lfafer(iul)
end
subroutine lfapplfaminm
! --------------------------------------------------------------
! **** *LFAPPLFAMINM* Sortie des extrema des articles de fichiers LFA.
! **** *LFAPPLFAMINM* Print out extrema of all articles from some LFA files.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) iarg,iul,ilf,jarg
integer(kind=4) iarg4,iargc
character*200 clf
character*3 cllang
!
! Saisie de la ligne de commande.
!
iarg4=iargc()
iarg=iarg4
if(iarg == 0) then
	if(cllang() == 'FRA') then
		write(*,'(a)') ' '
		write(*,'(2a)') 'Sortie des extrema et moyenne des articles' &
& 			,' d''un (plusieurs) fichier(s) LFA.'
		write(*,'(a)') ' '
		write(*,'(a)') 'Utilisation: lfaminm LFA1 [LFA2 ... LFAn]'
		write(*,'(a)') ' '
		stop
	else
		write(*,'(a)') ' '
		write(*,'(2a)') 'Prints out extrema, mean and rms ' &
& 			,'of all articles from one (or more) LFA file(s).'
		write(*,'(a)') ' '
		write(*,'(a)') 'Usage: lfaminm LFA1 [LFA2 ... LFAn]'
		write(*,'(a)') ' '
		stop
	endif
endif
do jarg=1,iarg
	call getargp(jarg,clf)
	!
	! Ouverture du fichier.
	!
	iul=72
	call lfaouv(iul,clf,'R')
	ilf=len_trim(clf)
	!
	! Determination des extrema, avec sortie sur output standard.
	!
	call lfaminm(iul)
	!
	! Fermeture du fichier.
	!
	call lfafer(iul)
enddo
end
subroutine lfapplfareu
! --------------------------------------------------------------
! **** *LFAPPLFAREU* Reunion de deux fichiers de LFA.
! **** *LFAPPLFAREU* Fuse two LFA files.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) jplis,iule1,iule2,iuls,ilis1 &
& 	,ilis2,jlis1,illis1,jlis2,illis2,inarg
logical llinter
character*200 clfe1,clfe2,clfs
parameter(jplis=600) ! nombre maxi d'articles dans les fichiers LFA d'entree.
character*200 cllis1(jplis) ! liste des articles du fichier d'entree 1.
character*200 cllis2(jplis) ! liste des articles du fichier d'entree 2.
character*3 cllang
!
! Saisie de la ligne de commande.
!
inarg=1
call getargp(inarg,clfe1)
inarg=2
call getargp(inarg,clfe2)
inarg=3
call getargp(inarg,clfs)
if(clfs == ' ') then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Reunion de deux fichiers LFA.'
		print*,' '
		print*,'Utilisation: lfareu F1 F2 Fres'
		print*,'	En entree: F1 et F2, en sortie: Fres.'
		print*,'	F2 est prioritaire sur F1, i.e. si un article'
		print*,'	est present dans F1 et F2,' &
& 			,' c''est celui de F2 qui sera copie.'
		print*,' '
		stop
	else
		print*,' '
		print*,'Fuse two LFA files.'
		print*,' '
		print*,'Usage: lfareu F1 F2 Fres'
		print*,'	In input: F1 and F2, in output: Fres.'
		print*,'	F2 has higher priority than F1, i.e. if an article'
		print*,'	is present in both F1 and F2,' &
& 			,' the article from F2 will be copied.'
		print*,' '
		stop
	endif
endif
!
! Ouverture des fichiers.
!
iule1=72
iule2=74
iuls=73
call lfaouv(iule1,clfe1,'R')
call lfaouv(iule2,clfe2,'R')
call lfaouv(iuls,clfs,'W')
!
! Liste des articles.
!
call lfalaft(iule1,cllis1,jplis,ilis1)
call lfalaft(iule2,cllis2,jplis,ilis2)
!
! On boucle sur les articles du fichier 1.
! On copie en sortie chaque article,
! en le lisant sur 1 s'il n'est que dans 1,
! et en le lisant sur 2 s'il est dans les deux.
!
do jlis1=1,ilis1
	llinter=.false. ! vrai si l'article est present dans l'intersection des deux fichiers.
	illis1=len_trim(cllis1(jlis1))
	do jlis2=1,ilis2
		illis2=len_trim(cllis2(jlis2))
		if(cllis2(jlis2)(1:illis2) == cllis1(jlis1)(1:illis1)) &
& 			llinter=.true.
	enddo
	if(llinter) then
		!
		! On copie l'article du fichier 2 au fichier de sortie.
		!
		call lfacop(iule2,cllis1(jlis1),' ',iuls)
	else
		!
		! On copie l'article du fichier 1 au fichier de sortie.
		!
		call lfacop(iule1,cllis1(jlis1),' ',iuls)
	endif
enddo
!
! On boucle sur les articles du fichier 2.
! On copie en sortie les articles,
! de 2 qui ne sont pas dans 1.
!
do jlis2=1,ilis2
	llinter=.false. ! vrai si l'article est present dans l'intersection des deux fichiers.
	illis2=len_trim(cllis2(jlis2))
	do jlis1=1,ilis1
		illis1=len_trim(cllis1(jlis1))
		if(cllis2(jlis2)(1:illis2) == cllis1(jlis1)(1:illis1)) &
& 			llinter=.true.
	enddo
	if(.not.llinter) then
		!
		! L'article est seulement dans le 2.
		!
		call lfacop(iule2,cllis2(jlis2),' ',iuls)
	endif
enddo
!
! Fermeture des fichiers.
!
call lfafer(iule1)
call lfafer(iule2)
call lfafer(iuls)
end
subroutine lfapplfatest
! --------------------------------------------------------------------------
! **** *LFAPPLFATEST* Programme testant si un fichier est LFA.
! **** *LFAPPLFATEST* Prints out if a given file is a LFA one.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   98-01, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
character*200 clnomf
integer(kind=jpintusr) iul,inarg
logical lllfa
character*3 cllang
!
! Ouverture du fichier.
!
inarg=1
call getargp(inarg,clnomf)
if(clnomf == ' ') then
	if(cllang() == 'FRA') then
		print*,' '
		print*,'Programme testant si un fichier est de type LFA.'
		print*,' '
		print*,'Utilisation: lfatest FICHIER'
		print*,' '
		print*,'La reponse, sur output standard, sera'
		print*,'	- "lfa"     si le fichier est LFA,'
		print*,'	- "non-lfa" sinon.'
		print*,' '
		stop
	else
		print*,' '
		print*,'Prints out if a given file is a LFA one.'
		print*,' '
		print*,'Usage: lfatest FILE'
		print*,' '
		print*,'The result, on standard output, will be'
		print*,'	- "lfa"     if the file is a LFA one,'
		print*,'	- "non-lfa" else case.'
		print*,' '
		stop
	endif
endif
iul=7
call lfatest(iul,clnomf,lllfa)
if(lllfa) then
	write(*,'(a)') 'lfa'
else
	write(*,'(a)') 'non-lfa'
endif
end
subroutine lfapreci(kul,kprec)
! --------------------------------------------------------------
! **** *LFAPRECI* Forcage de la precision d'ecriture des entiers.
! **** *LFAPRECI* Force integer data writing precision.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kul      unite logique du fichier LFA.
! kprec    precision des entiers a ecrire ulterieurement, en octets.
! En sortie:
! --------------------------------------------------------------
! Input:
! kul      logical unit of LFA file.
! kprec    precision of integer data to write, in bytes.
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
#include"lfayom.h"
integer(kind=jpintusr) kul,kprec
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfapreci: forcage de la precision ',kprec
endif
if(kprec == 4.or.kprec == 8) then
	npreci(kul)=kprec
else
	print*,'LFAPRECI/ERREUR: precision en octets' &
& 		,' non recevable!...',kprec
	call lfa_print_file(kul)
	stop 'call abort'
endif
end
subroutine lfaprecr(kul,kprec)
! --------------------------------------------------------------
! **** *LFAPRECR* Forcage de la precision d'ecriture des reels.
! **** *LFAPRECR* Force real data writing precision.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kul      unite logique du fichier LFA.
! kprec    precision des reels a ecrire ulterieurement, en octets.
! En sortie:
! --------------------------------------------------------------
! Input:
! kul      logical unit of LFA file.
! kprec    precision of real data to write, in bytes.
! Output:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
#include"lfayom.h"
integer(kind=jpintusr) kul,kprec
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfaprecr: forcage de la precision ',kprec
endif
if(kprec == 4.or.kprec == 8) then
	nprecr(kul)=kprec
else
	print*,'LFAPRECR/ERREUR: precision en octets' &
& 		,' non recevable!...',kprec
	call lfa_print_file(kul)
	stop 'call abort'
endif
end
subroutine lfarew(kul)
! --------------------------------------------------------------
! **** *LFAREW* Rebobinage d'un fichier LFA.
! **** *LFAREW* Rewind a LFA file.
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
! kul: unite logique du fichier LFA.
! En sortie:
! --------------------------------------------------------------
! Input:
! kul: logical unit of LFA file.
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
integer(kind=jpintusr) iversion
integer(kind=jpintesb) iversionesb
#include"lfayom.h"
!
! Rebobinage.
!
rewind(kul)
!
! Lecture de l'en-tete..
!
read(kul) iversionesb
iversion=iversionesb
!
! Position du pointeur.
!
lgpoint(kul)=.false.
end
subroutine lfatest(kul,cdnomf,ldlfa)
! --------------------------------------------------------------------------
! **** *LFATEST* Teste si un fichier est bien de type LFA.
! **** *LFATEST* Test if a file is a LFA one.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   97-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul         unite logique du fichier;
! .           ce doit etre une unite disponible:
! .           le fichier va etre ouvert sous cette unite logique.
! cdnomf      nom du fichier.
! En sortie:
! ldlfa=.true. si le fichier est de type LFA, .false. sinon.
! --------------------------------------------------------------------------
! Input:
! kul         logical unit of file.
! .           this unit has to be free:
! .           the file will be opened with this logical unit.
! cdnomf      file name.
! Output:
! ldlfa=.true. if the file is a LFA one, .false. else case.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
integer(kind=jpintusr) iversion
integer(kind=jpintesb) iversionesb
logical ldlfa,llex
#include"lfayom.h"
character*(*) cdnomf
character*200 classign
!
! Le fichier existe-t-il?
!
inquire(file=cdnomf,exist=llex)
if(.not.llex) then
	!
	! Le fichier n'existe pas.
	! On se contente de retourner qu'il est non-LFA.
	!
	ldlfa=.false.
else
	!
	! Le fichier existe.
	! On lit les premiers caracteres afin de voir s'ils
	! sont conformes a ce qu'on est en droit d'attendre
	! d'un LFA!...
	!
#ifdef cray
	!
	! Dans le cas du Cray, il faut executer un ordre assign:
	! en fortran, tous les read transductent alors du format IEEE
	! vers CRAY, et les write dans l'autre sens.
	!
	write(classign,fmt='(2a)') 'assign -N ieee -F f77 f:',cdnomf(1:len_trim(cdnomf))
	call assign(classign)
#endif
	open(kul,file=cdnomf,form='unformatted',status='old',err=100)
	read(kul,err=100,end=100) iversionesb
	iversion=iversionesb
	close(kul)
#ifdef cray
	call lfaiunassign(kul)
#endif
	if(iversion == 8.or.iversion == 9.or.iversion == 21) then
		!
		! L'en-tete lue est bien celle
		! attendue.
		!
		ldlfa=.true.
	else
		!
		! L'en-tete lue n'est pas celle
		! attendue.
		!
		ldlfa=.false.
	endif
	return
  100 	continue
	!
	! Si on est ici, c'est qu'il y a eu erreur
	! lors de la lecture de l'en-tete sur le fichier.
	! Le fichier n'est donc pas un LFA.
	!
	close(kul)
#ifdef cray
	call lfaiunassign(kul)
#endif
	ldlfa=.false.
endif
end
subroutine lfapplfafreq
! --------------------------------------------------------------
! **** *lfapplfafreq* Histogramme d'un article de fichier LFA.
! **** *lfapplfafreq* PDF of an LFA article.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) inile,iarg,iargc,jarg
real(kind=jpreeusr) zindef,zmin,zmax
character*200 clarg,clna,clfent
character*3 cllang
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
inile=20
zindef=8.325214
zmin=zindef
zmax=zindef
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargc() ! nombre d'arguments.
if(iarg == 0) then
	!
	!-------------------------------------------------
	! Le nombre d'arguments n'est pas celui requis
	! pour exécuter. On fournit la documentation.
	!-------------------------------------------------
	!
	if(cllang() == 'FRA') then
		write(*,'(a)') ' '
		write(*,'(a)') 'Courbes de fréquence et de répartition d''un champ.'
		write(*,'(a)') ' '
		write(*,'(a)') 'Utilisation: lfafreq FICHIER ARTICLE [-nNILES] [-xnVALn] [-xxVALx] '
		write(*,'(a)') ' '
		write(*,'(a)') 'Le programme lit l''article ARTICLE du fichier FICHIER, et en cherche les NILES n-iles'
		write(*,'(a)') 'de la valeur VALn à VALx'
		write(*,'(a)') ' '
		write(*,'(a,i3)') 'Défaut de NILES: ',inile
		write(*,'(a,i3)') 'Défaut de VALn: le min du champ, déterminé à partir des données lues sur le fichier.'
		write(*,'(a,i3)') 'Défaut de VALx: le max du champ, déterminé à partir des données lues sur le fichier.'
		write(*,'(a)') ' '
		write(*,'(a)') 'Il en effectue la courbe de fréquence, et la courbe de répartition, donnée par ses n-iles.'
		write(*,'(a)') 'Il sort 3 fichiers:'
		write(*,'(a)') ' '
		write(*,'(a)') '	- lfafreq.tmp.freq: courbe de fréquence.'
		write(*,'(a)') '	  Deux colonnes dans ce fichier: la 1ère est le centre de la classe,'
		write(*,'(a)') '	  la 2ème la fréquence correspondante.'
		write(*,'(a)') '	  On entend ici par fréquence la densité de probabilité de cette classe;'
		write(*,'(a)') '	  son unité est l''inverse de celle des données d''entrée.'
		write(*,'(a)') '	  La somme des éléments de la deuxième colonne, pondérée par la largeur'
		write(*,'(a)') '	  moyenne de chaque bande, donne 1..'
		write(*,'(a)') '	  UNITE DE LA COLONNE 1: celle des données d''entrée.'
		write(*,'(a)') '	  UNITE DE LA COLONNE 2: 1/(celle des données d''entrée).'
		write(*,'(a)') ' '
		write(*,'(a)') '	- lfafreq.tmp.rep: courbe de répartition.'
		write(*,'(a)') '	  Deux colonnes dans ce fichier: soit x la 1ère, y la 2ème; '
		write(*,'(a)') '	  alors p(X<x)=y.'
		write(*,'(a)') '	  UNITE DE LA COLONNE 1: celle des données d''entrée.'
		write(*,'(a)') '	  UNITE DE LA COLONNE 2: sans unité (et entre 0 et 1).'
		write(*,'(a)') ' '
		write(*,'(a)') '	- lfafreq.tmp.cl: valeur des classes, n-iles du champ:'
		write(*,'(a)') '	  une colonne dans ce fichier.'
		write(*,'(a)') '	  La ligne i du fichier contient la valeur v telle que p(X<v)=i/NILES.'
		write(*,'(a)') '	  UNITE DE L''UNIQUE COLONNE: celle des données d''entrée.'
		write(*,'(a)') ' '
	else
		write(*,'(a)') ' '
		write(*,'(a)') 'Probability density function of a LFA file article.'
		write(*,'(a)') ' '
		write(*,'(a)') 'Usage: lfafreq FILE ARTICLE [-nNILES] [-xnVALn] [-xxVALx] '
		write(*,'(a)') ' '
		write(*,'(a)') 'The program reads the article ARTICLE of the file FILE, and produces the NILES n-iles.'
		write(*,'(a)') 'from value VALn to VALx'
		write(*,'(a)') ' '
		write(*,'(a,i3)') 'Default value for NILES: ',inile
		write(*,'(a,i3)') 'Default value for VALn: minimum of the field.'
		write(*,'(a,i3)') 'Default value for VALx: maximum of the field.'
		write(*,'(a)') ' '
		write(*,'(a)') '3 files in output:'
		write(*,'(a)') ' '
		write(*,'(a)') '	- lfafreq.tmp.freq: PDF.'
		write(*,'(a)') '	  Two colums in this file: the first is the class center,'
		write(*,'(a)') '	  the second is the corresponding frequency.'
		write(*,'(a)') '	  UNIT OF COLUMN 1: these of input data.'
		write(*,'(a)') '	  UNIT OF COLUMN 2: 1 / these of input data.'
		write(*,'(a)') ' '
		write(*,'(a)') '	- lfafreq.tmp.rep: repartition function.'
		write(*,'(a)') '	  Two columns in this file: x the 1st, y the 2nd; '
		write(*,'(a)') '	  then p(X<x)=y.'
		write(*,'(a)') '	  UNIT OF COLUMN 1: these of input data.'
		write(*,'(a)') '	  UNIT OF COLUMN 2: adimensional (between 0 and 1).'
		write(*,'(a)') ' '
		write(*,'(a)') '	- lfafreq.tmp.cl: value of the field n-iles:'
		write(*,'(a)') '	  one column in this file.'
		write(*,'(a)') '	  The line i from file contains the value v so that p(X<v)=i/NILES.'
		write(*,'(a)') '	  UNIT OF COLUMN 1: these of input data.'
		write(*,'(a)') ' '
	endif
	stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
clna=' '
clfent=' '
do jarg=1,iarg
	call getargp(jarg,clarg)
	if(clarg(1:2) == '-n') then
		read(clarg(3:),fmt=*) inile
	elseif(clarg(1:3) == '-xn') then
		read(clarg(4:),fmt=*) zmin
	elseif(clarg(1:3) == '-xx') then
		read(clarg(4:),fmt=*) zmax
	elseif(clfent == ' ') then
		clfent=clarg
	elseif(clna == ' ') then
		clna=clarg
	else
		if(cllang() == 'FRA') then
			print*,'LFAFREQ/ERREUR: argument non attendu: ',clarg
		else
			print*,'LFAFREQ/ERROR: unexpected argument: ',clarg
		endif
		stop 'call abort'
	endif
enddo
call frequence(clfent,clna,zmin,zmax,zindef,inile)
end
subroutine frequence(cdfent,cdna,pmin,pmax,pindef,knile)
! --------------------------------------------------------------
! **** *FREQUENCE* Courbe de fréquence.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   1999-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! pmin	min du champ.
! pmax	max du champ.
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
CHARACTER*(*) CDFENT,CDNA
CHARACTER*200 CLTYPE
INTEGER(KIND=JPINTUSR) :: IBORDTMP
INTEGER(KIND=JPINTUSR) :: IBOUC
INTEGER(KIND=JPINTUSR) :: ISOUS
INTEGER(KIND=JPINTUSR) :: JNILE
INTEGER(KIND=JPINTUSR) :: JSOUS
INTEGER(KIND=JPINTUSR) :: JVAL
INTEGER(KIND=JPINTUSR) :: IUL1,ILONG,IERR,ILONGS,INBLIG,KNILE,IBORD
INTEGER(KIND=JPINTUSR) :: JBORD
INTEGER(KIND=JPINTUSR), PARAMETER :: JPBORD=5000
LOGICAL :: LLOK
LOGICAL :: LLTRO
REAL(KIND=JPREEUSR) :: ZCENTRE
REAL(KIND=JPREEUSR) :: ZDENS
REAL(KIND=JPREEUSR) :: ZPROBA
REAL(KIND=JPREEUSR) :: ZPROS,pindef
REAL(KIND=JPREEUSR) :: ZBORD(0:JPBORD)
REAL(KIND=JPREEUSR) :: ZBORDTMP(0:JPBORD)
REAL(KIND=JPREEUSR) :: ZFREQ(JPBORD)
REAL(KIND=JPREEUSR) :: ZFREQ_CUM(JPBORD)
REAL(KIND=JPREEUSR) :: ZMIN,ZMAX,PMIN,PMAX
REAL(KIND=JPREEUSR) :: ZNILE(KNILE-1)
REAL(KIND=JPREEUSR), ALLOCATABLE :: ZDAT(:)
!
!-------------------------------------------------
! Ouverture du fichier lfa.
!-------------------------------------------------
!
iul1=23
call lfaouv(iul1,cdfent,'R')
!
!-------------------------------------------------
! Quelle est la taille de l'article?
!-------------------------------------------------
!
call lfacas(iul1,cdna,cltype,ilong,ierr)
!
!-------------------------------------------------
! Lecture de cet article de données.
!-------------------------------------------------
!
allocate(zdat(ilong))
call lfalecr(iul1,cdna,ilong,zdat,ilongs,ierr)
inblig=ilong
print*,'lfafreq:'
print*,'	',inblig,' réels lus.'
print*,'	',knile,' niles désirés.'
zmin=minval(zdat)
zmax=maxval(zdat)
if(pmin /= pindef) then
	!
	!-------------------------------------------------
	! L'utilisateur veut imposer son min.
	!-------------------------------------------------
	!
	zmin=pmin
endif
if(pmax /= pindef) then
	!
	!-------------------------------------------------
	! L'utilisateur veut imposer son max.
	!-------------------------------------------------
	!
	zmax=pmax
endif
print*,'	Minimum=',zmin
print*,'	Maximum=',zmax
!
!-------------------------------------------------
! On initialise le tableau des bords.
!-------------------------------------------------
!
ibord=knile
do jbord=0,ibord
	zbord(jbord)=zmin+real(jbord)*(zmax-zmin)/real(ibord)
enddo
llok=.false.
ibouc=0
do while(.not.llok)
	ibouc=ibouc+1
	!
	!-------------------------------------------------
	! On répartit le champ dans les classes définies par le tableau zbord.
	!-------------------------------------------------
	!
	print*,'	Nombre de classes courantes: ',ibord
	zfreq=0.
	do jval=1,inblig
		lltro=.false.
		do jbord=1,ibord
			if(zdat(jval) <= zbord(jbord)) then
				!
				!-------------------------------------------------
				! La valeur réelle zdat(jval) est entre
				! zbord(jbord) et zbord(jbord-1).
				! On augmente donc de 1 le nombre d'éléments au sein de la classe
				! jbord.
				!-------------------------------------------------
				!
				zfreq(jbord)=zfreq(jbord)+1.
				lltro=.true.
				exit
			endif
		enddo
		if(.not.lltro) then
			!
			!-------------------------------------------------
			! La valeur courante n'a été affectée à aucune classe.
			! C'est qu'elle est égale au max.
			!-------------------------------------------------
			!
			zfreq(ibord)=zfreq(ibord)+1.
		endif
	enddo
	!
	!-------------------------------------------------
	! Si une classe donnée a une probabilité supérieure
	! au seuil zpros, on la casse en sous classes, de façon
	! itérative jusqu'à ce que la probabilité devienne inférieure
	! au seuil ou qu'il y ait trop de classes.
	!-------------------------------------------------
	!
	zpros=0.2 ! probabilité seuil.
	zpros=2. !!!!!! gol  probabilité seuil.
	ibordtmp=0
	zbordtmp(0)=zbord(0)
	llok=.true.
	print*,'	Test des seuils de classe...'
	do jbord=1,ibord
		zproba=zfreq(jbord)/real(inblig)
		if(zproba > zpros) then
			!
			!-------------------------------------------------
			! La classe a une probabilité supérieure
			! au seuil zpros, on la casse en sous classes.
			!-------------------------------------------------
			!
			print*,'	',nint(100.*zproba),'% des valeurs dans la classe [',zbord(jbord-1),' ; ',zbord(jbord),'];'
			isous=20 ! nombre de sous classes de la classes courante.
			if(ibord+isous < jpbord.and.ibouc < 6) then
				!
				!-------------------------------------------------
				! Eu égard à la taille statique du tableau
				! zbord, on peut ajouter des sous-classes.
				!-------------------------------------------------
				!
				print*,'	On casse cette classe en ',isous,' sous classes.'
				llok=.false.
				do jsous=1,isous
					ibordtmp=ibordtmp+1
					zbordtmp(ibordtmp)=zbord(jbord-1)+real(jsous)/real(isous)*(zbord(jbord)-zbord(jbord-1))
				enddo
			else
				!
				!-------------------------------------------------
				! Eu égard à la taille statique du tableau
				! zbord, on ne peut plus ajouter de sous-classe.
				! On arrête donc les frais ici.
				!-------------------------------------------------
				!
				llok=.true.
			endif
		else
			!
			!-------------------------------------------------
			! La classe a une probabilité inférieure
			! au seuil zpros, est donc OK telle quelle.
			!-------------------------------------------------
			!
			ibordtmp=ibordtmp+1
			zbordtmp(ibordtmp)=zbord(jbord)
		endif
	enddo
	if(.not.llok) then
		ibord=ibordtmp
		do jbord=1,ibordtmp
			zbord(jbord)=zbordtmp(jbord)
		enddo
	endif
enddo
!
!-------------------------------------------------
! A ce stade, on est sorti de la boucle de mise
! du tableau zdat en classes.
! On va imprimer les résultats.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Ecriture des courbes.
!-------------------------------------------------
!
print*,'	Ecriture des fichiers de sortie.'
open(21,file='lfafreq.tmp.freq',form='formatted')
open(22,file='lfafreq.tmp.rep',form='formatted')
do jbord=1,ibord
	!
	!-------------------------------------------------
	! Fréquence.
	!-------------------------------------------------
	!
	zcentre=0.5*(zbord(jbord-1)+zbord(jbord))
	zdens=zfreq(jbord)/real(inblig)/(zbord(jbord)-zbord(jbord-1)) ! densité de probabilité.
	write(21,*) zcentre,zdens
	!
	!-------------------------------------------------
	! Répartition.
	!-------------------------------------------------
	!
	zproba=zfreq(jbord)/real(inblig)
	if(jbord == 1) then
		write(22,*) zmin,0.
		zfreq_cum(jbord)=zproba
	else
		zfreq_cum(jbord)=zfreq_cum(jbord-1)+zproba
	endif
	write(22,*) zbord(jbord),zfreq_cum(jbord)
enddo
close(21)
print*,'	Fichier généré: lfafreq.tmp.freq'
close(22)
print*,'	Fichier généré: lfafreq.tmp.rep'
!
!-------------------------------------------------
! Recherche des n-lies.
!-------------------------------------------------
!
call nile(zbord,zfreq_cum,ibord,knile,znile)
!
!-------------------------------------------------
! n-iles.
!-------------------------------------------------
!
open(24,file='lfafreq.tmp.cl',form='formatted')
do jnile=1,knile-1
	write(24,*) znile(jnile)
enddo
close(24)
print*,'	Fichier généré: lfafreq.tmp.cl'
end
subroutine nile(pbord,prepar,krepar,knile,pnile)
! --------------------------------------------------------------
! **** *NILE* Fourniture des n-iles d'un champ, connaissant la fct de répartition.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Il s'agit simplement d'inverser la fonction de répartition.
! Externes:
! Auteur:   1999-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! pbord: valeur du champ au bord des classes de répartition.
! prepar: fct de répartition.
! krepar: taille du tableau prepar.
! knile: nombre de n-iles désirés.
! En sortie:
! pile: tableau des n-iles calculés.
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
real prepar(krepar)
real pnile(knile-1)
real pbord(0:krepar)
do jnile=1,knile-1
	zproba=real(jnile)/real(knile)
	!
	!-------------------------------------------------
	! Quelle valeur du champ
	! a la probabilité zproba?
	!-------------------------------------------------
	!
	llok=.false.
	do jrepar=1,krepar
		if(zproba < prepar(jrepar)) then
			llok=.true.
			if(jrepar == 1) then
				pnile(jnile)=pbord(jrepar-1)+(pbord(jrepar)-pbord(jrepar-1))*(zproba-0.)/(prepar(jrepar)-0.)
			else
				pnile(jnile)=pbord(jrepar-1)+(pbord(jrepar)-pbord(jrepar-1))*(zproba-prepar(jrepar-1))/(prepar(jrepar)-prepar(jrepar-1))
			endif
			exit
		endif
	enddo
	if(.not.llok) then
		print*,'lfafreq/NILE/ERREUR: non convergence!...'
		print*,zproba
		print*,prepar
		stop 'call abort'
	endif
enddo
end
subroutine lfa_print_file(kul)
! --------------------------------------------------------------
! **** *LFA_PRINT_FILE* Simple impression sur output standard du nom en clair du fichier.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kul    unité logique du fichier.
! En sortie:
! --------------------------------------------------------------
use yomlfa, only: cgfnom
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
character*3 cllang
if(cllang() == 'FRA') then
	print*,'Fichier LFA  : ',cgfnom(kul)(1:len_trim(cgfnom(kul)))
	print*,'Unité logique: ',kul
else
	print*,'LFA file     : ',cgfnom(kul)(1:len_trim(cgfnom(kul)))
	print*,'Logical unit : ',kul
endif
end
#include"cllang.f90"
#include"caracteres_lfa.f90"
