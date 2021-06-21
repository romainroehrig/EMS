subroutine lfpavan(kul,cdna,cdtype,klbouc,krep)
! --------------------------------------------------------------------------
! **** *LFPAVAN* Avancée d'un article dans un fichier LFP.
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
! Auteur:   97-01, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul               unite logique du fichier.
! En sortie:
! cdna              nom de l'article sur lequel on a avancé, s'il y a lieu.
! cdtype            type de l'article sur lequel on a avancé, s'il y a lieu.
! klbouc            longueur de l'article sur lequel on a avancé, s'il y a lieu.
! krep              0 si on a avancé d'un article, 1 si fin de fichier.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klbouc,krep,iskip,jskip,ilart
character*200 clart
character*(*) cdna
character*(*) cdtype
!
! Valeurs de retour par défaut.
!
cdna=' '
cdtype=' '
klbouc=0
krep=1
!
! On lit une ligne sur le fichier LFP.
!
read(kul,fmt='(a)',end=200) clart
if(clart(4:7) == '%%!!') then
	!
	! L'article est bien de type LFP.
	!
	cdna=clart(18:) ! nom de l'article.
	cdtype=clart(8:8) ! type d'article (réel, entier, etc...)
	read(clart(9:17),fmt='(i9)') klbouc ! longueur de l'article.
	krep=0 ! code de réponse de retour OK.
	if(clart(1:3) == 'LFP'.or.clart(1:3) == 'FAI'.or.clart(1:3) == 'FAA') then
		!
		! Le nombre de lignes à sauter est renseigné
		! sur l'article courant.
		!
		read(clart(9:17),fmt='(i9)') iskip
	else
		!
		! Le nombre de lignes à sauter est renseigné
		! sur l'article suivant.
		!
		read(kul,fmt='(3x,i9)') iskip
	endif
	!
	! L'article comporte iskip lignes, aussi on saute
	! sans lire leur contenu les iskip articles
	! physiques suivants du fichier.
	!
	do jskip=1,iskip
		read(kul,fmt='(a)',end=200)
	enddo
else
	print*,'LFPAVAN/ERREUR: non en-tete!...'
	ilart=len_trim(clart)
	print*,clart(1:ilart)
	stop 'call abort'
endif
return
  200 continue
krep=1
end
subroutine lfpcas(kul,cdna,cdtype,klbouc,kret)
! --------------------------------------------------------------------------
! **** *LFPCAS* Renseignements sur un article de fichier LFP.
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
! Auteur:   96-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul               unite logique du fichier.
! cdna: si cdna=' ' on recherche l'article suivant.
! .         cdna est alors en entrée/sortie,
! .         et en sortie il vaudra le nom de l'article suivant
! .         (si cet article existe).
! .         kret...retour de recherche: 0 si OK,
! .                1 si fin de fichier.
! .     si cdna<>' ' cdna est le nom de l'article cherché.
! .          Il est alors en entrée seulement.
! .         kret...retour de recherche: 0 si OK,
! .                1 si article inexistant.
! En sortie:
! cdtype            type d'article: 'R', 'I', 'L', 'C'.
! klbouc             nombre d'éléments de cet article.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klbouc,kret,imes
character*200 cldoc
logical llerf
#include"lfpyom.h"
character*(*) cdna
character*(*) cdtype
!
! On rend temporairement silencieux et tolérant le logiciel.
!
imes=nmes(kul)
nmes(kul)=0
llerf=lgerf(kul)
lgerf(kul)=.false.
!
! Recherche de l'article suivant.
!
call lfppos(kul,cdna,kret,cldoc)
!
! On remet le niveau de messagerie et de tolérance à l'initial.
!
nmes(kul)=imes
lgerf(kul)=llerf
!
! Type de d'article (réel, entier, etc...)
!
if(kret == 0) then
	!
	! On n'était pas en fin de fichier.
	! On lit le contenu de la documentation.
	!
	if(cdna == ' ') cdna=cldoc(18:)
	cdtype=cldoc(8:8)
	read(cldoc(9:17),fmt='(i9)') klbouc ! longueur de l'article.
endif
end
subroutine lfpcop(kule,cdnae,cdnas,kuls)
! --------------------------------------------------------------
! **** *LFPCOP* Copie d'un article d'un fichier LFP à un autre.
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
! kule unité logique du fichier LFP d'entrée.
! cdnae nom de l'article à lire.
! cdnas nom sous lequel l'article est recopié.
! kuls unité logique du fichier LFP de sortie.
! En sortie:
! Le fichier d'unité logique kuls est augmenté d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,kuls,ilong,iret
character*(*) cdnae, cdnas
character*1 cltype
!
! Renseignements sur l'article cdnae.
!
call lfpcas(kule,cdnae,cltype,ilong,iret)
if(iret == 0) then
	!
	! L'article existe.
	!
	if(cltype == 'R') then
		!
		! Article de type réel.
		!
		call lfpcopr(kule,cdnae,cdnas,ilong,kuls)
	elseif(cltype == 'I') then
		!
		! Article de type entier.
		!
		call lfpcopi(kule,cdnae,cdnas,ilong,kuls)
	elseif(cltype == 'C') then
		!
		! Article de type caractère.
		!
		call lfpcopc(kule,cdnae,cdnas,ilong,kuls)
	elseif(cltype == 'L') then
		!
		! Article de type logique.
		!
		call lfpcopl(kule,cdnae,cdnas,ilong,kuls)
	else
		print*,'LFPCOP/ATTENTION: type de donnée inconnu!...'
		print*,cltype
	endif
else
	print*,'LFPCOP/ATTENTION: article ',cdnae,' inexistant!...'
endif
end
subroutine lfpcopc(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *LFPCOPC* Copie d'un article caractères d'un fichier LFP à un autre.
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
! kule unité logique du fichier LFP d'entrée.
! cdnae nom de l'article à lire.
! cdnas nom sous lequel l'article est recopié.
! klong longeur de l'article à copier.
! kuls unité logique du fichier LFP de sortie.
! En sortie:
! Le fichier d'unité logique kuls est augmenté d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,kuls,ilong,ierr,klong
character*(*) cdnae, cdnas
character*200 clnas
character*200 clbouc(klong)
!
! Lecture de l'article d'entrée.
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
call lfpecrc(kuls,clnas,clbouc,ilong)
end
subroutine lfpcopi(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *LFPCOPI* Copie d'un article entier d'un fichier LFP à un autre.
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
! kule unité logique du fichier LFP d'entrée.
! cdnae nom de l'article à lire.
! cdnas nom sous lequel l'article est recopié.
! klong longeur de l'article à copier.
! kuls unité logique du fichier LFP de sortie.
! En sortie:
! Le fichier d'unité logique kuls est augmenté d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*200 clnas
integer(kind=jpintusr) ibouc(klong)
!
! Lecture de l'article d'entrée.
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
! Ecriture sur le fichier de sortie.
!
call lfpecri(kuls,clnas,ibouc,ilong)
end
subroutine lfpcopl(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *LFPCOPL* Copie d'un article logique d'un fichier LFP à un autre.
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
! kule unité logique du fichier LFP d'entrée.
! cdnae nom de l'article à lire.
! cdnas nom sous lequel l'article est recopié.
! klong longeur de l'article à copier.
! kuls unité logique du fichier LFP de sortie.
! En sortie:
! Le fichier d'unité logique kuls est augmenté d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*200 clnas
logical llbouc(klong)
!
! Lecture de l'article d'entrée.
!
call lfplecl(kule,cdnae,klong,llbouc,ilong,ierr)
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
call lfpecrl(kuls,clnas,llbouc,ilong)
end
subroutine lfpcopr(kule,cdnae,cdnas,klong,kuls)
! --------------------------------------------------------------
! **** *LFPCOPR* Copie d'un article réel d'un fichier LFP à un autre.
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
! kule unité logique du fichier LFP d'entrée.
! cdnae nom de l'article à lire.
! cdnas nom sous lequel l'article est recopié.
! klong longeur de l'article à copier.
! kuls unité logique du fichier LFP de sortie.
! En sortie:
! Le fichier d'unité logique kuls est augmenté d'un article.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kule,klong,kuls,ilong,ierr
character*(*) cdnae, cdnas
character*200 clnas
real(kind=jpreeusr) zbouc(klong)
!
! Lecture de l'article d'entrée.
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
! Ecriture sur le fichier de sortie.
!
call lfpecrr(kuls,clnas,zbouc,ilong)
end
subroutine lfpecrc(kul,cdna,cdbouc,klbouc)
! --------------------------------------------------------------------------
! **** *LFPECRC* Ecriture de caracteres sur fichier LFP.
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
! En entree:
! kul                unite logique du fichier.
! cdna               nom de l'article a ecrire.
! cdbouc(1,klbouc)   caracteres a ecrire.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klbouc,ilnomf,ilna,jbouc,ilb
character*(*) cdna
character*(*) cdbouc(klbouc)
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpecrc: ecriture de ',cdna
endif
if(ntypo(kul) == 1) then
	print* &
& 	,'LFPECRC/ERREUR: écriture sur fichier ouvert en lecture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
!
! Longueur du nom d'article.
!
ilna=len_trim(cdna)
!
! Ecriture du nom d'article dans le LFP.
!
write(kul,fmt='(2A,i9,a)') 'LFP%%!!','C',klbouc,cdna(1:ilna)
!
! Ecriture des caracteres dans le LFP.
!
do jbouc=1,klbouc
	!
	! Longueur de la chaine courante.
	!
	ilb=len_trim(cdbouc(jbouc))
	!
	! Ecriture.
	!
	write(kul,fmt='(a)') cdbouc(jbouc)(1:ilb)
enddo
end
subroutine lfpecri(kul,cdna,kbouc,klbouc)
! --------------------------------------------------------------------------
! **** *LFPECRI* Ecriture d'entiers sur fichier LFP.
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
! En entree:
! kul                  unite logique du fichier.
! cdna                  nom de l'article a ecrire.
! kbouc(1,klbouc)      entiers a ecrire.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klbouc,ilnomf,ilna,jbouc
character*(*) cdna
integer(kind=jpintusr) kbouc(klbouc)
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpecri: ecriture de ',cdna
endif
if(ntypo(kul) == 1) then
	print* &
& 	,'LFPECRI/ERREUR: écriture sur fichier ouvert en lecture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
!
! Longueur du nom d'article.
!
ilna=len_trim(cdna)
!
! Ecriture du nom d'article dans le LFP.
!
write(kul,fmt='(2A,i9,a)') 'LFP%%!!','I',klbouc,cdna(1:ilna)
!
! Ecriture des entiers dans le LFP.
!
do jbouc=1,klbouc
	write(kul,fmt=*) kbouc(jbouc)
enddo
end
subroutine lfpecrl(kul,cdna,ldbouc,klbouc)
! --------------------------------------------------------------------------
! **** *LFPECRL* Ecriture de logiques sur fichier LFP.
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
! Auteur:   95-02, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul                unite logique du fichier.
! cdna               nom de l'article a ecrire.
! ldbouc(1,klbouc)   logiques a ecrire.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klbouc,ilnomf,ilna,jbouc
character*(*) cdna
logical ldbouc(klbouc)
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpecrl: ecriture de ',cdna
endif
if(ntypo(kul) == 1) then
	print* &
& 	,'LFPECRL/ERREUR: écriture sur fichier ouvert en lecture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
!
! Longueur du nom d'article.
!
ilna=len_trim(cdna)
!
! Ecriture du nom d'article dans le LFP.
!
write(kul,fmt='(2A,i9,a)') 'LFP%%!!','L',klbouc,cdna(1:ilna)
!
! Ecriture des caracteres dans le LFP.
!
do jbouc=1,klbouc
	!
	! Ecriture.
	!
	write(kul,fmt=*) ldbouc(jbouc)
enddo
end
subroutine lfpecrr(kul,cdna,pbouc,klbouc)
! --------------------------------------------------------------------------
! **** *LFPECRR* Ecriture de reel sur fichier LFP.
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
! En entree:
! kul              unite logique du fichier.
! cdna             nom de l'article a ecrire.
! pbouc(1,klbouc)  reels a ecrire.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klbouc,ilnomf,ilna,ipal,jbouc,ibase &
& ,iprec,iprecb,ilog,isign,jprecb,iascm,imod,iasce,illis,ifinl &
& ,igol1,igoldebut,igol255,max,min,igol2
real(kind=jpreeusr) znref,znlfp,znlig,znpal,zbase,zuslog &
& ,zprecb,zr,zmant
character*200 cl1,clfor,cllis
character*(*) cdna
character*3 clmetc
real(kind=jpreeusr) pbouc(klbouc)
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpecrr: ecriture de l''article ',cdna
endif
if(ntypo(kul) == 1) then
	print* &
& 	,'LFPECRR/ERREUR: écriture sur fichier ouvert en lecture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
if(nprec(kul) <= 0.or.nprec(kul) > 30) then
	!
	! Le nombre de chiffres significatifs pour les réels
	! à porter sur fichier n'a pas été initialisé (cas
	! ou le fichier n'a pas été ouvert par lfpouv).
	! On l'initialise ici par défaut.
	!
	nprec(kul)=7
endif
!
! Longueur du nom d'article.
!
ilna=len_trim(cdna)
if(cgmetc(kul) == 'AUT') then
	!
	! L'utilisateur laisse le choix au logiciel
	! de la méthode de compression des réels.
	!
	ipal=1 ! nombre de valeurs réelles consécutivement différentes dans l'article.
	do jbouc=1,klbouc
		!
		! Recherche du nombre de valeurs consécutivement différentes.
		!
		igol1=1
		if(pbouc(jbouc) /= pbouc(max(jbouc-1,igol1))) then
			!
			! La valeur courante n'est pas égale à la précédente.
			!
			ipal=ipal+1
		endif
	enddo
	if(nmes(kul) == 2) then
		!
		! Messagerie bavarde.
		!
		print*,'++ lfpecrr: ipal: ',ipal
	endif
	!
	! Quelle compression choisir?
	!
	! Nombre d'octets pour coder de façon nécessaire et suffisante
	! un réel sur nprec(kul) chiffres significatifs, avec un exposant
	! à deux chiffres.
	!
	znref=log(400.*10.**real(nprec(kul)))/log(256.)
	!
	! Nombre d'octets pour coder de façon nécessaire et suffisante
	! un réel sur nprec(kul) chiffres significatifs, avec un exposant
	! à deux chiffres, par la méthode "LFP".
	!
	znlfp=real(7+nprec(kul))
	!
	! Nombre d'octets pour coder de façon nécessaire et suffisante
	! un réel sur nprec(kul) chiffres significatifs, avec un exposant
	! à deux chiffres, par la méthode "LIG".
	!
	znlig=log(10.**real(nprec(kul))*2./real(jpbase))/ &
& 	log(real(jpbase))+2.
	!
	! Nombre d'octets pour coder de façon nécessaire et suffisante
	! un réel sur nprec(kul) chiffres significatifs, avec un exposant
	! à deux chiffres, par la méthode "PAL".
	!
	znpal=real(ipal*(nprec(kul)+25))/klbouc
	if(nmes(kul) == 2) then
		!
		! Messagerie bavarde.
		!
		print*,'++ lfpecrr: znref=',znref
		print*,'++ lfpecrr: znlfp=',znlfp
		print*,'++ lfpecrr: znlig=',znlig
		print*,'++ lfpecrr: znpal=',znpal
	endif
	if(klbouc < 10) then
		!
		! Article très court. On ne gagnera pas à le comprimer.
		!
		clmetc='LFP'
	elseif(znpal < znlig) then
		!
		! Le champ comporte de nombreuses valeurs consécutives semblables
		! (ou est constant):
		! le taux de compression znpal
		! attendu par la méthode de compression "par paliers",
		! est meilleur que celui des autres.
		! On va compresser le champ en renseignant dans le fichier les seules valeurs
		! des différents paliers.
		!
		clmetc='PAL'
	else
		!
		! On applique la méthode générale.
		!
		clmetc='LIG'
	endif
else
	!
	! L'utilisateur impose la méthode de compression
	! des réels.
	!
	clmetc=cgmetc(kul)
endif
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpecrr: methode de compression: ',clmetc
endif
if(clmetc(1:3) == 'LIG') then
	!
	! Compression type LIG.
	!
	! Tout le champ de réels est codé sur une ligne,
	! en base jpbase.
	ibase=jpbase
	zbase=real(ibase)
	iprec=nprec(kul) ! nb ch. significatifs en base 10 garantis.
	zuslog=1./log(zbase)
	!
	! iprecb: nombre de chiffres sur lequel il faut coder
	! la mantisse en base ibase
	! pour avoir une précision de iprec chiffres significatifs
	! en base 10.
	!
	zprecb=log(10.**real(iprec)*2./real(jpbase))/ &
& 	log(real(jpbase))+1.
	iprecb=nint(zprecb-.5)+1
	write(kul,fmt='(3a,i9,a)') clmetc(1:3),'%%!!','R',klbouc,cdna(1 &
& 	:ilna)
	write(kul,fmt='(a,i9)') clmetc(1:3),2
	write(kul,fmt='(i3,2i4)') iprecb, jpbase, jpdebut
	do jbouc=1,klbouc
		zr=pbouc(jbouc)
		if(zr > 0.) then
			!
			! Réel > 0.
			! On détermine l'exposant de ibase le plus
			! voisin du réel.
			!
			ilog=nint(log(zr)*zuslog)
			isign=1
		elseif(zr < 0.) then
			!
			! Réel < 0.
			! On détermine l'exposant de ibase le plus
			! voisin du réel.
			!
			ilog=nint(log(-zr)*zuslog)
			isign=-1
			zr=-zr
		else
			!
			! Réel nul.
			! L'exposant de ibase le plus
			! voisin du réel est 0!...
			!
			ilog=0
			isign=1
		endif
		!
		! Ecriture de zr en base ibase, sur iprecb chiffres.
		!
		zmant=zr*zbase**(-real(ilog))
		if(zmant > zbase) then
			!
			! Suite à erreur de calcul de la machine,
			! le premier chiffre de la représentation
			! de zr en base ibase est plus grand que ibase.
			! On corrige le tir.
			!
			zmant=zmant/zbase
			ilog=ilog+1
		endif
		cl1=' '
		do jprecb=1,iprecb-1
			cl1(jprecb:jprecb)=char(jpdebut+int(zmant))
			zmant=zbase*(zmant-int(zmant))
		enddo
		!
		! Le signe de la mantisse en traité de la façon suivante:
		! s'il est positif, le code ascii du dernier chiffre de la mantisse
		! en base ibase sera impair, et pair sinon.
		!
		iascm=jpdebut+int(zmant)
		igol2=2
		imod=mod(iascm,igol2)
		if((imod == 0.and.isign == 1).or.(imod == 1.and.isign == -1)) &
& 		then
			iascm=iascm+1
			if(iascm == 256) iascm=254
		endif
		!
		! Exposant.
		!
		iasce=jpdebut+ibase/2+ilog
		igoldebut=jpdebut
		igol255=255
		iasce=min(igol255,max(igoldebut,iasce))
		!
		! Ecriture des 2 chaînes: mantisse, exposant.
		!
		write(kul,fmt='(3a,$)') cl1(1:iprecb-1),char(iascm), &
& 		char(iasce)
	enddo
	!
	! On ajoute un retour chariot à la fin du champ.
	!
	write(kul,fmt='(a,$)') char(10)
elseif(clmetc(1:3) == 'LFP') then
	!
	! Pas de compression.
	!
	!
	! Ecriture du nom d'article dans le LFP.
	!
	write(kul,fmt='(3a,i9,a)') clmetc(1:3),'%%!!','R',klbouc,cdna(1 &
& 	:ilna)
	!
	! Ecriture des reels dans le LFP.
	!
	write(clfor,fmt='(a,i2.2,a,i2.2,a)') '(E',nprec(kul)+6,'.', &
& 	nprec(kul),')'
	do jbouc=1,klbouc
		write(kul,fmt=clfor) pbouc(jbouc)
	enddo
elseif(clmetc(1:3) == 'LIS') then
	!
	! Pas de compression,
	! mais au contraire on privilégie
	! la lisibilité (LIS).
	!
	! Ecriture du nom d'article dans le LFP.
	!
	write(kul,fmt='(3a,i9,a)') 'LFP','%%!!','R',klbouc,cdna(1 &
& 	:ilna)
	!
	! Ecriture des reels dans le LFP.
	!
	do jbouc=1,klbouc
		call reecar(pbouc(jbouc),-2,1,cllis,illis)
		write(kul,fmt='(a)') cllis(1:illis)
	enddo
elseif(clmetc(1:3) == 'PAL') then
	!
	! Champ comportant de nombreuses valeurs consécutives semblables.
	! On le compresse en ne portant sur le fichier que les
	! valeurs des différents paliers successifs.
	!
	! Ecriture du nom d'article dans le LFP.
	!
	write(kul,fmt='(3a,i9,a)') clmetc(1:3),'%%!!','R',klbouc,cdna(1 &
& 	:ilna)
	ifinl=2*ipal ! nombre fin de lignes portées sur fichier pour le sous-codage courant.
	write(kul,fmt='(a,i9)') clmetc(1:3),ifinl
	!
	! Ecriture des paliers sur le LFP.
	!
	write(clfor,fmt='(a,i2.2,a,i2.2,a)') '(E',nprec(kul)+7,'.', &
& 	nprec(kul),')'
	ipal=0
	do jbouc=1,klbouc
		if(pbouc(jbouc) /= pbouc(max(jbouc-1,igol1))) then
			!
			! Changement de palier.
			!
			write(kul,*) ipal ! nombre de réels de ce palier.
			write(kul,clfor) pbouc(jbouc-1) ! valeur du palier.
			ipal=1
		else
			!
			! Suite du même palier.
			!
			ipal=ipal+1
		endif
	enddo
	if(ipal == 0) then
		!
		! La fin du tableau est survenue après écriture d'un palier
		! Le dernier réel est donc un palier à une seule valeur.
		!
		write(kul,*) 1 ! nombre de réels de ce palier.
		write(kul,clfor) pbouc(klbouc) ! valeur du palier.
	else
		!
		! La fin du tableau est survenue en cours de palier.
		!
		write(kul,*) ipal ! nombre de réels de ce palier.
		write(kul,clfor) pbouc(klbouc) ! valeur du palier.
	endif
else
	print* &
& 	,'LFPECRR/ERREUR: type de compression demandee inconnu!...'
	stop 'call abort'
endif
end
subroutine lfperf(kul,lderf)
! --------------------------------------------------------------------------
! **** *LFPERF* Niveau d'erreur tolere sur fichier LFP.
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
! En entree:
! kul               unite logique du fichier.
! lderf             .true. si toute erreur doit etre fatale,
! .false. si aucune ne doit l'etre.
! En sortie:
! lgerf             .true. si toute erreur est fatale,
! .false. si aucune ne l'est.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
logical lderf
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfperf: lgerf(',kul,' mis a ',lderf
endif
lgerf(kul)=lderf
end
subroutine lfpfer(kul)
! --------------------------------------------------------------------------
! **** *LFPFER* Fermeture de fichier LFP.
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
! En entree:
! kul        unite logique du fichier.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
#include"lfpyom.h"
close(kul)
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpfer: fermeture de l''unite logique ',kul
endif
end
subroutine lfplaf(kul,kulout)
! --------------------------------------------------------------------------
! **** *LFPLAF* Liste des articles d'un fichier LFP.
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
! En entree:
! kul             unite logique du fichier.
! kulout          unite logique sur laquelle sortir la liste.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kulout,iart,ilong,iret,ilna
character*200 clna,cltype
#include"lfpyom.h"
write(kulout,'(a,i2,3a)') 'LFPLAF du fichier d''unite logique ', &
& kul &
& ,', de nom ',cgnomf(kul)(1:index(cgnomf(kul),' ')),':'
rewind(kul)
iart=0
  100 continue
clna=' '
call lfpavan(kul,clna,cltype,ilong,iret)
if(iret == 0) then
	!
	! On n'est pas en fin de fichier.
	!
	iart=iart+1
	!
	! Sortie des renseignements sur l'article.
	!
	ilna=len_trim(clna)
	write(kulout,'(3a,i9,2a)') &
& 	'Type: ',cltype(1:1) &
& 	,' Longueur:',ilong &
& 	,' Nom: ',clna(1:ilna)
	!
	! Lecture de la suite du fichier.
	!
	goto 100
endif
rewind(kul)
end
subroutine lfplaft(kul,cdlis,kdlis,knlis)
! --------------------------------------------------------------------------
! **** *LFPLAFT* Liste des articles d'un fichier LFP sur tableau de caractères.
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
! En entree:
! kul             unite logique du fichier.
! kdlis           dimension physique du tableau cdlis.
! En sortie:
! knlis           nombre d'articles du fichier et d'éléments écrits sur cdlis.
! cdlis(1, ..., knlis) nom des articles du fichier.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdlis,knlis,ilong,iret
character*200 clna,cltype
character*(*) cdlis(kdlis)
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfplaft: entree.'
endif
rewind(kul)
knlis=0
  100 continue
clna=' '
call lfpavan(kul,clna,cltype,ilong,iret)
if(iret == 0) then
	!
	! On n'est pas en fin de fichier.
	!
	knlis=knlis+1
	if(knlis > kdlis) then
		print*,'LFPLAFT/ERREUR: trop de champs dans le fichier!...'
		print*,'Recompiler!...'
		print*,knlis,kdlis
		stop 'call abort'
	endif
	cdlis(knlis)=clna
	!
	! Lecture de la suite du fichier.
	!
	goto 100
endif
end
subroutine lfplecc(kul,cdna,kdimb,cdbouc,klbouc,kerr)
! --------------------------------------------------------------------------
! **** *LFPLECC* Lecture de caracteres sur fichier LFP.
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
! En entree:
! kul               unite logique du fichier.
! cdna              nom de l'article.
! kdimb             dimension du tableau cdbouc.
! En sortie:
! klbouc            nombre de caracteres lus.
! cdbouc(1,klbouc)  caracteres lus.
! kerr              indicateur d'erreur
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdimb,klbouc,kerr,ilnomf,ilna,jbouc
character*200 cldoc
character*(*) cdna
character*(*) cdbouc(kdimb)
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfplecc: lecture de ',cdna
endif
if(ntypo(kul) == 2) then
	print* &
& 	,'LFPLECC/ERREUR: lecture sur fichier ouvert en écriture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
klbouc=0
!
! Position du pointeur fichier sur l'article dans le LFP.
!
call lfppos(kul,cdna,kerr,cldoc)
if(kerr == 0) then
	!
	! L'article existe. On va le lire.
	!
	read (kul,fmt='(a)') ! on saute la ligne de documentation de l'article.
	read(cldoc(9:17),fmt='(i9)') klbouc ! longueur de l'article.
	do jbouc=1,klbouc
		read(kul,fmt='(a)') cdbouc(jbouc)
	enddo
endif
end
subroutine lfpleci(kul,cdna,kdimb,kbouc,klbouc,kerr)
! --------------------------------------------------------------------------
! **** *LFPLECI* Lecture d'entiers sur fichier LFP.
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
! En entree:
! kul               unite logique du fichier.
! cdna              nom de l'article.
! kdimb             dimension du tableau kbouc.
! En sortie:
! klbouc            nombre d'entiers lus.
! kbouc(1,klbouc)   entiers lus.
! kerr              indicateur d'erreur.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdimb,klbouc,kerr,ilnomf,ilna,jbouc
character*200 cldoc
character*(*) cdna
integer(kind=jpintusr) kbouc(kdimb)
#include"lfpyom.h"
#include"lfadoc.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpleci: lecture de ',cdna
endif
if(ntypo(kul) == 2) then
	print* &
& 	,'LFPLECI/ERREUR: lecture sur fichier ouvert en écriture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
klbouc=0
!
! Position du pointeur fichier sur l'article dans le LFP.
!
call lfppos(kul,cdna,kerr,cldoc)
if(kerr == 0) then
	!
	! L'article existe. On va le lire.
	!
	read (kul,fmt='(a)') ! on saute la ligne de documentation de l'article.
	read(cldoc(9:17),fmt='(i9)') klbouc ! longueur de l'article.
	cgdoc=cldoc(1:3)
	do jbouc=1,klbouc
		read(kul,fmt=*) kbouc(jbouc)
	enddo
endif
end
subroutine lfplecl(kul,cdna,kdimb,ldbouc,klbouc,kerr)
! --------------------------------------------------------------------------
! **** *LFPLECL* Lecture de logiques sur fichier LFP.
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
! Auteur:   95-02, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul               unite logique du fichier.
! cdna              nom de l'article.
! kdimb             dimension du tableau kbouc.
! En sortie:
! klbouc            nombre de logiques lus.
! ldbouc(1,klbouc)  logiques lus.
! kerr              indicateur d'erreur.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdimb,klbouc,kerr,ilnomf,ilna,jbouc
character*200 cldoc
character*(*) cdna
logical ldbouc(kdimb)
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfplecl: lecture de ',cdna
endif
if(ntypo(kul) == 2) then
	print* &
& 	,'LFPLECL/ERREUR: lecture sur fichier ouvert en écriture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
klbouc=0
!
! Position du pointeur fichier sur l'article dans le LFP.
!
call lfppos(kul,cdna,kerr,cldoc)
if(kerr == 0) then
	!
	! L'article existe. On va le lire.
	!
	read (kul,fmt='(a)') ! on saute la ligne de documentation de l'article.
	read(cldoc(9:17),fmt='(i9)') klbouc ! longueur de l'article.
	do jbouc=1,klbouc
		read(kul,fmt=*) ldbouc(jbouc)
	enddo
endif
end
subroutine lfplecr(kul,cdna,kdimb,pbouc,klbouc,kerr)
! --------------------------------------------------------------------------
! **** *LFPLECR* Lecture de reels sur fichier LFP.
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
! En entree:
! kul               unite logique du fichier.
! cdna              nom de l'article.
! kdimb             dimension du tableau pbouc.
! En sortie:
! pbouc(1,klbouc)   reels lus.
! klbouc            nombre de reels lus.
! kerr              indicateur d'erreur.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kdimb,klbouc,kerr,ilnomf,ilna &
& ,jbouc,ifinl &
& ,ilbouc,iprecb,ibase,idebut,ilchal,jfinl,ipal,ilmetc
real(kind=jpreeusr) zpal
character*200 cldoc,clmetc
character*(*) cdna
real(kind=jpreeusr) pbouc(kdimb)
#include"lfpyom.h"
#include"lfadoc.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfplecr: lecture de ',cdna
endif
if(ntypo(kul) == 2) then
	print* &
& 	,'LFPLECR/ERREUR: lecture sur fichier ouvert en écriture!...'
	print*,'Unité logique: ',kul
	ilnomf=len_trim(cgnomf(kul))
	print*,'Fichier ',cgnomf(kul)(1:ilnomf)
	ilna=len_trim(cdna)
	print*,'Article ',cdna(1:ilna)
	stop 'call abort'
endif
klbouc=0
!
! Position du pointeur fichier sur l'article dans le LFP.
!
call lfppos(kul,cdna,kerr,cldoc)
if(kerr == 0) then
	!
	! L'article existe. On va le lire.
	!
	read (kul,fmt='(a)') ! on saute la ligne de documentation de l'article.
	read(cldoc(9:17),fmt='(i9)') klbouc ! longueur de l'article.
	cgdoc=cldoc(1:3)
	if(cldoc(1:3) == 'LFP'.or.cldoc(1:3) == 'FAI'.or.cldoc(1:3) == 'FAA'.or.cldoc(3:3) == '-') then
		!
		! Le réel a été écrit sans compression.
		!
		do jbouc=1,klbouc
			read(kul,fmt=*) pbouc(jbouc)
		enddo
	else
		!
		! Le réel a été écrit avec compression.
		!
		read(kul,fmt='(a3,i9)') clmetc,ifinl
		ilbouc=0 ! nombre de réels écrits sur le tableau de sortie pbouc.
		if(clmetc(1:3) == 'LIG') then
			!
			! Le réel a été écrit avec la compression 'LIG'.
			!
			read(kul,fmt=*) iprecb, ibase, idebut
			!
			! ilchal: longueur en caractères de la chaîne
			! à lire sur le fichier d'entrée.
			! Cette chaîne contient tout le champ.
			!
			ilchal=(iprecb+1)*klbouc
			call lfpligdec(kul,ilchal,iprecb,ibase,idebut,pbouc,klbouc)
		elseif(clmetc(1:3) == 'PAL') then
			!
			! Le réel a été écrit avec la compression 'PAL'.
			!
			do jfinl=1,ifinl/2
				read(kul,*) ipal ! nombre de valeurs du palier.
				read(kul,*) zpal ! valeur du palier.
				!
				! On affecte les ipal valeurs au tableau de sortie.
				!
				do jbouc=1,ipal
					ilbouc=ilbouc+1
					pbouc(ilbouc)=zpal
				enddo
			enddo
			if(ilbouc /= klbouc) then
				print* &
& 				,'LFPLECR/ERREUR: inconsistence de nombre de paliers!...'
				print*,ilbouc,klbouc
				stop 'call abort'
			endif
		else
			print*,'LFPLECR/ERREUR: methode de compression inconnue!...'
			ilmetc=len_trim(clmetc)
			print*,clmetc(1:ilmetc)
			stop 'call abort'
		endif
	endif
endif
end
subroutine lfpligdec(kul,klchal,kprecb,kbase,kdebut,pbouc,klbouc)
! --------------------------------------------------------------------------
! **** *LFPLIGDEC* Decompression d'une ligne de caractères sur un tableau de réels.
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
! Auteur:   96-05, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul         unite logique du fichier.
! klchal      taille de la chaîne à lire sur le fichier LFP.
! kprecb      nombre de caractères sur lequel est écrit la mantisse du réel.
! kbase       base dans laquelle est exprimée la mantisse codée.
! kdebut      les chiffres de 0 à (kbase-1) correspondent
! .           aux codes ASCII kdebut à (kdebut+kbase-1).
! klbouc      longueur du tableau de réels pbouc.
! En sortie:
! pbouc       tableau de réels résultant.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klchal,kprecb,kbase,kdebut &
& ,klbouc,jbouc,ipos &
& ,ilog,iascm,isign,jprec,igol2
real(kind=jpreeusr) zbase,zusbase,zr
character*(klchal) clchal
real(kind=jpreeusr) pbouc(klbouc)
zbase=real(kbase)
zusbase=1./zbase
read(kul,fmt='(a)') clchal
do jbouc=1,klbouc
	!
	! Position du réel de rang jbouc dans la chaîne à décompresser.
	!
	ipos=(kprecb+1)*(jbouc-1)
	!
	! Exposant de ibase du réel.
	!
	ilog=ichar(clchal(ipos+kprecb+1:ipos+kprecb+1))-kdebut-kbase/2
	!
	! Code ascii du dernier chiffre de la mantisse.
	!
	iascm=ichar(clchal(ipos+kprecb:ipos+kprecb))
	!
	! On démarre la somme géométrique reconstituant le réel
	! à partir de son développement p-adique en base ibase.
	!
	zr=real(iascm-kdebut)
	!
	! Le signe de la mantisse est donné par la parité
	! du code ascii du dernier chiffre.
	!
	igol2=2
	isign=2*mod(iascm,igol2)-1
	do jprec=kprecb-1,1,-1
		zr=zr*zusbase+real(ichar(clchal(ipos+jprec:ipos+jprec))- &
& 		kdebut)
	enddo
	pbouc(jbouc)=isign*zr*zbase**real(ilog)
enddo
end
subroutine lfpmes(kul,kmes)
! --------------------------------------------------------------------------
! **** *LFPMES* Niveau de messagerie du logiciel LFP.
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
! En entree:
! kul         unite logique du fichier.
! kmes        niveau de messagerie:
! si 0 aucun message sorti par le logiciel LFP.
! si 1 messages d'ATTENTION et d'ERREUR sorties.
! si 2 LFP est bavard (mode debug)...
! En sortie:
! nmes(kul)  tableau common des niveaux de messagerie.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kmes
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpmes: niveau de messagerie de l''unite logique ', &
& 	kul,' porte a ',kmes
endif
nmes(kul)=kmes
end
subroutine lfpmetc(kul,cdmetc)
! --------------------------------------------------------------------------
! **** *LFPMETC* Choix de la méthode de compression des réels.
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
! Auteur:   96-05, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul         unite logique du fichier.
! cdmetc      méthode de compression des réels.
! .           - 'AUT': le logiciel choisit la méthode automatiquement.
! .             Il choisira en général 'LFP' pour les articles très courts,
! .             'PAL' pour ceux qui sont des fonctions en escalier,
! .             et 'LIG' pour les autres.
! .             Lors de l'ouverture d'un fichier en écriture, c'est
! .             ce choix qui est porté par défaut.
! .           - 'LFP': les réels sont écrits sous forme formattée.
! .             Le fichier est alors plus volumineux et plus long
! .             à écrire. On ne choisira donc cette méthode que
! .             lorsque l'article est très court,
! .             lorsque l'on souhaite que le fichier résultant
! .             soit éditable, ou imprimable, ou puisse être envoyé
! .             par courrier électronique.
! .           - 'PAL': écriture par paliers: cette méthode
! .             permet une bonne compression des données qui se présentent
! .             sous la forme d'une fonction en escalier.
! .           - 'LIG': quelle que soit sa taille, le tableau de réels
! .             est écrit sur une seule ligne de caractères, afin
! .             d'optimiser d'une part la taille des fins de ligne
! .             et d'autre part de permettre une relecture plus rapide
! .             des fichiers multi-articles (saut physique).
! En sortie:
! cgmetc      le choix de la méthode est porté sur common.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
character*(*) cdmetc
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpmetc: methode de compression portee a ',cdmetc
endif
cgmetc(kul)=cdmetc
end
subroutine lfpnx(kul)
! --------------------------------------------------------------
! **** *LFPNX* Extrema de tous les articles d'un fichier LFP.
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
! kul unité logique du fichier LFP d'entrée.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,ilong,iret
character*200 clna
! implicit character*100 (c)
character*1 cltype
rewind(kul)
  100 continue
!
! Renseignements sur l'article suivant du fichier.
!
clna=' '
call lfpcas(kul,clna,cltype,ilong,iret)
if(iret == 0) then
	!
	! On n'est pas en fin de fichier.
	!
	if(cltype == 'R') then
		!
		! Article de type réel.
		!
		call lfpnxr(kul,clna,cltype,ilong)
	elseif(cltype == 'I') then
		!
		! Article de type entier.
		!
		call lfpnxi(kul,clna,cltype,ilong)
	elseif(cltype == 'C') then
		!
		! Article de type caractère.
		!
		call lfpnxc(kul,clna,cltype,ilong)
	elseif(cltype == 'L') then
		!
		! Article de type logique.
		!
		call lfpnxl(kul,clna,cltype,ilong)
	else
		print*,'LFPNX/ATTENTION: type de donnée inconnu!...'
		print*,cltype
	endif
	goto 100
endif
end
subroutine lfpnxc(kul,cdnom,cdtype,klong)
! --------------------------------------------------------------
! **** *LFPNXC* Extrema d'un article de caractères de fichier LFP.
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
! kul unité logique du fichier LFP d'entrée.
! cdnom nom de l'article à lire.
! cdtype type d'article (réel, entier, etc...).
! klong longueur de cet article.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilong,ierr,jb,ilnom,ilmin,ilmax &
& ,igol18
character*200 cldonmin,cldonmax,cllong
! implicit character*(120) (c)
character*100 cdnom
character*(*) cdtype
character*120 cldon(klong)
call lfplecc(kul,cdnom,klong,cldon,ilong,ierr)
cldonmin=cldon(1)
cldonmax=cldon(1)
do jb=1,ilong
	if(cldon(jb) < cldonmin) cldonmin=cldon(jb)
	if(cldon(jb) > cldonmax) cldonmax=cldon(jb)
enddo
ilnom=len_trim(cdnom)
igol18=18
ilnom=max(igol18,ilnom)
ilmin=15
ilmax=ilmin
write(cllong,fmt='(i6)') ilong
write(*,'(50a)') cdnom(1:ilnom),'|',cdtype,'| longueur: ' &
& ,cllong(1:6),', min=',cldonmin(1:ilmin),' max=',cldonmax(1:ilmax)
end
subroutine lfpnxi(kul,cdnom,cdtype,klong)
! --------------------------------------------------------------
! **** *LFPNXI* Extrema d'un article d'entiers de fichier LFP.
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
! kul unité logique du fichier LFP d'entrée.
! cdnom nom de l'article à lire.
! cdtype type d'article (réel, entier, etc...).
! klong longueur de cet article.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilong,ierr,imin,imax &
& ,jb,ilnom,ilmoy,ilmin,ilmax,igol18
real(kind=jpreeusr) zmoy
character*100 cdnom,clmin,clmax,clmoy,cllong
character*(*) cdtype
integer(kind=jpintusr) idon(klong)
call lfpleci(kul,cdnom,klong,idon,ilong,ierr)
imin=idon(1)
imax=idon(1)
zmoy=0.
do jb=1,ilong
	imin=min(imin,idon(jb))
	imax=max(imax,idon(jb))
	zmoy=zmoy+real(idon(jb))
enddo
zmoy=zmoy/real(ilong)
ilnom=len_trim(cdnom)
igol18=18
ilnom=max(igol18,ilnom)
write(clmin,fmt='(7x,i8)') imin
write(clmax,fmt='(7x,i8)') imax
call reecar(zmoy,-2,1,clmoy,ilmoy)
ilmin=15
ilmax=ilmin
write(cllong,fmt='(i6)') ilong
write(*,'(50a)') cdnom(1:ilnom),'|',cdtype,'| longueur: ' &
& ,cllong(1:6),', min=',clmin(1:ilmin),' max=' &
& ,clmax(1:ilmax),' moy=',clmoy(1:ilmoy)
end
subroutine lfpnxl(kul,cdnom,cdtype,klong)
! --------------------------------------------------------------
! **** *LFPNXL* Extrema d'un article de logiques de fichier LFP.
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
! kul unité logique du fichier LFP d'entrée.
! cdnom nom de l'article à lire.
! cdtype type d'article (réel, entier, etc...).
! klong longueur de cet article.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilong,ierr,jb,ilnom,ilmin &
& ,ilmax,ilmoy,igol18
logical lldonmin,lldonmax
character*200 cllong,clmoy
real(kind=jpreeusr) zmoy
! implicit character*(120) (c)
character*100 cdnom
character*(*) cdtype
logical lldon(klong)
call lfplecl(kul,cdnom,klong,lldon,ilong,ierr)
lldonmin=lldon(1)
lldonmax=lldon(1)
zmoy=0.
do jb=1,ilong
	if(lldon(jb)) then
		lldonmax=.true.
		zmoy=zmoy+1.
	else
		lldonmin=.false.
	endif
enddo
zmoy=zmoy/real(ilong)
ilnom=len_trim(cdnom)
igol18=18
ilnom=max(igol18,ilnom)
ilmin=15
ilmax=ilmin
write(cllong,fmt='(i6)') ilong
call reecar(zmoy,-2,1,clmoy,ilmoy)
write(*,'(6a,l15,a,l15,2a)') &
& cdnom(1:ilnom),'|',cdtype,'| longueur: ',cllong(1:6),', min=', &
& lldonmin,' max=',lldonmax &
& ,' moy=',clmoy(1:ilmoy)
end
subroutine lfpnxr(kul,cdnom,cdtype,klong)
! --------------------------------------------------------------
! **** *LFPNXR* Extrema d'un article de réels de fichier LFP.
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
! kul unité logique du fichier LFP d'entrée.
! cdnom nom de l'article à lire.
! cdtype type d'article (réel, entier, etc...).
! klong longueur de cet article.
! En sortie:
! Extrema sur output standard.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,klong,ilong,ierr,jb &
& ,ilnom,ilmax,ilmoy,ilmin,igol18
character*200  clmin,clmax,clmoy,cllong
real(kind=jpreeusr) zmin,zmax,zmoy
character*100 cdnom
character*(*) cdtype
real(kind=jpreeusr) zdon(klong)
call lfplecr(kul,cdnom,klong,zdon,ilong,ierr)
zmin=zdon(1)
zmax=zdon(1)
zmoy=0.
do jb=1,ilong
	zmin=min(zmin,zdon(jb))
	zmax=max(zmax,zdon(jb))
	zmoy=zmoy+zdon(jb)
enddo
zmoy=zmoy/real(ilong)
ilnom=len_trim(cdnom)
igol18=18
ilnom=max(igol18,ilnom)
call reecar(zmin,-2,1,clmin,ilmin)
call reecar(zmax,-2,1,clmax,ilmax)
call reecar(zmoy,-2,1,clmoy,ilmoy)
write(cllong,fmt='(i6)') ilong
write(*,'(50a)') cdnom(1:ilnom),'|',cdtype,'| longueur: ' &
& ,cllong(1:6),', min=',clmin(1:ilmin),' max=',clmax(1:ilmax) &
& ,' moy=',clmoy(1:ilmoy)
end
subroutine lfpouv(kul,cdnomf,ktypo)
! --------------------------------------------------------------------------
! **** *LFPOUV* Ouverture de fichier LFP.
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
! En entree:
! kul         unite logique du fichier.
! cdnomf      nom en clair du fichier.
! ktypo       type d'ouverture: 1 READ, 2 WRITE, 3 READ-WRITE, 4 scratch.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,ktypo,ilnomf
character*200 clc
logical llex
character*(*) cdnomf
#include"lfpyom.h"
!
! Ouverture du fichier.
!
if(ktypo == 1) then
	!
	! READ
	!
	inquire(file=cdnomf,exist=llex)
	if(.not.llex) then
		print*,'LFPOUV/ERREUR: fichier d''entree inexistant!...'
		ilnomf=len_trim(cdnomf)
		print*,cdnomf(1:ilnomf)
		stop 'call abort'
	endif
	open(kul,file=cdnomf,form='formatted',status='old')
	!
	! On lit le premier article afin de voir s'il
	! est conforme à ce qu'on est en droit d'attendre
	! d'un LFP!...
	!
	clc=' '
	read(kul,'(a7)',err=300) clc(1:7)
  300 	continue
	if(clc(4:7) /= '%%!!') then
		print*,'LFPOUV/ERREUR: incompatibilite fichier/logiciel.'
		stop 'call abort'
	endif
	rewind(kul)
elseif(ktypo == 4) then
	!
	! SCRATCH.
	!
	open(kul,form='formatted',status='scratch')
else
	!
	! AUTRES CAS. OPEN DE TYPE 'PAR DEFAUT'.
	!
	open(kul,file=cdnomf,form='formatted',status='unknown')
endif
!
! Type d'ouverture du fichier.
!
ntypo(kul)=ktypo
!
! Precision des reels a ecrire sur ce fichier
! (valeur par defaut).
!
nprec(kul)=7
!
! Choix de la méthode de compression des réels.
! (valeur par defaut).
!
cgmetc(kul)='AUT'
!
! Niveau de messagerie
! (valeur par defaut).
!
nmes(kul)=1
!
! Niveau d'erreur fatale par defaut.
!
call lfperf(kul,.true.)
!
! Sauvegarde du nom en clair du fichier.
!
cgnomf(kul)=cdnomf
end
subroutine lfppos(kul,cdna,kerr,cddoc)
! --------------------------------------------------------------------------
! **** *LFPPOS* Recherche de la position d'un article dans un fichier LFP.
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
! En entree:
! kul          unite logique du fichier.
! cdna         nom de l'article.
! En sortie:
! >>>>>>>>>>>>>>>> si cdna chaine non blanche:
! On recherche l'article de nom cdna.
! kerr         indicateur d'erreur: 0 si l'article existe das le fichier.
! le pointeur du fichier sequentiel est
! alors positionne sur l'article demande.
! cddoc        chaine-documentation sur l'article.
! >>>>>>>>>>>>>>>> si cdna chaine blanche:
! On recherche l'article suivant.
! kerr         indicateur: 0 si article rencontré avant la fin du fichier.
! Le pointeur du fichier sequentiel est
! alors positionne sur l'article demande.
! cddoc        chaine-documentation sur l'article.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kerr,ilna,iskip,jskip,ilart
logical llrew
character*200 clart
character*(*) cdna
character*(*) cddoc
#include"lfpyom.h"
ilna=len_trim(cdna)
if(nmes(kul) == 2) then
	ilna=len_trim(cdna)
	print*,'++ lfppos: recherche de ',cdna(1:ilna),'...'
endif
kerr=1
llrew=.false. ! vrai si fichier rembobine, faux sinon.
  100 read(kul,fmt='(a)',end=200) clart
if(clart(4:7) == '%%!!') then
	if(clart(18:) == cdna.or.cdna == ' ') then
		!
		! L'article est celui cherché ou on cherche seulement
		! l'article suivant.
		!
		cddoc=clart
		kerr=0
		backspace(unit=kul)
		return
	else
		!
		! L'article LFP n'est pas celui cherché.
		!
		if(clart(1:3) == 'LFP'.or.clart(1:3) == 'FAI'.or.clart(1:3) == 'FAA') then
			!
			! Le nombre de lignes à sauter est renseigné
			! sur l'article courant.
			!
			read(clart(9:17),fmt='(i9)') iskip
		else
			!
			! Le nombre de lignes à sauter est renseigné
			! sur l'article suivant.
			!
			read(kul,fmt='(3x,i9)') iskip
		endif
		!
		! L'article comporte iskip lignes, aussi on saute
		! sans lire leur contenu les iskip articles
		! physiques suivants du fichier.
		!
		do jskip=1,iskip
			read(kul,fmt='(a)',end=200)
		enddo
	endif
else
	print*,'LFPPOS/ERREUR: non en-tete!...'
	ilart=len_trim(clart)
	print*,clart(1:ilart)
	stop 'call abort'
endif
goto 100
  200 continue
!
! On est en fin de fichier.
!
rewind(kul)
if(cdna == ' ') then
	!
	! On recherchait l'article suivant
	! mais on est en fait en fin de fichier.
	!
	kerr=-1
	return
endif
if(.not.llrew) then
	!
	! Si on est ici, c'est qu'a la fois on n'a pas rencontre
	! l'article demande et que le fichier n'etait pas
	! rembobine. Il est désormais rembobiné, et on tente
	! a nouveau la recherche de l'article demande.
	!
	llrew=.true.
	if(nmes(kul) >= 2) &
& 	print*,'LFPPOS/RECHERCHE de ',cdna(1:ilna) &
& 	,': fin du fichier lu et rebobinage...'
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
	print*,'LFPPOS/ERREUR: article ',cdna(1:ilna),' inexistant!...'
	stop 'call abort'
elseif(kerr /= 0.and.nmes(kul) >= 1) then
	!
	! Le niveau d'erreur doit provoquer un message d'alerte.
	!
	if(kerr == 1.and.nmes(kul) >= 1) then
		ilna=len_trim(cdna)
		print*,'LFPPOS/ATTENTION: article ' &
& 		,cdna(1:ilna),' inexistant!...'
	endif
endif
end
subroutine lfpprec(kul,kprec)
! --------------------------------------------------------------------------
! **** *LFPPREC* Precision des reels de fichier LFP.
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
! En entree:
! kul         unite logique du fichier.
! kprec       precision en nombre de chiffres significatifs.
! En sortie:
! nprec(kul)  tableau common des precisions.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul,kprec
#include"lfpyom.h"
if(nmes(kul) == 2) then
	!
	! Messagerie bavarde.
	!
	print*,'++ lfpprec: precision de l''unite logique ',kul &
& 	,' portee a ',kprec,' chiffres.'
endif
nprec(kul)=kprec
end
subroutine lfprew(kul)
! --------------------------------------------------------------
! **** *LFPREW* Rebobinage d'un fichier LFP.
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
! kul: unite logique.
! En sortie:
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
!
! Rebobinage.
!
rewind(kul)
end
subroutine lfptest(kul,cdnomf,ldlfp)
! --------------------------------------------------------------------------
! **** *LFPTEST* Teste si un fichier est bien de type LFP.
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
! Auteur:   97-01, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! kul         unite logique du fichier;
! .           ce doit être une unité disponible:
! .           le fichier va être ouvert sous cette unité logique.
! cdnomf      nom en clair du fichier.
! En sortie:
! ldlfp=.true. si le fichier est de type LFP, .false. sinon.
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kul
logical ldlfp,llex
character*200 clc
character*(*) cdnomf
!
! Le fichier existe-t-il?
!
inquire(file=cdnomf,exist=llex)
if(.not.llex) then
	!
	! Le fichier n'existe pas.
	! On se contente de retourner qu'il est non-LFP.
	!
	ldlfp=.false.
else
	open(kul,file=cdnomf,form='formatted',status='old')
	!
	! On lit le premier article afin de voir s'il
	! est conforme à ce qu'on est en droit d'attendre
	! d'un LFP!...
	!
	clc=' '
	read(kul,'(a7)',err=300) clc(1:7)
  300 	continue
	if(clc(4:7) /= '%%!!') then
		!
		! Fichier non LFP.
		!
		ldlfp=.false.
	else
		!
		! Fichier LFP.
		!
		ldlfp=.true.
	endif
	close(kul)
endif
end
