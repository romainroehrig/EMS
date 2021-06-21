subroutine carint(cdent,kpos1,kpos2,kres)
! --------------------------------------------------------------------------
! **** *CARINT *  - Conversion chaine de caractere > entier
! --------------------------------------------------------------------------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 91-01
! --------------------------------------------------------------------------
! En entree: cdent     chaine de caracteres (l.max. 200 caracteres)
! kpos1,kpos2 abscisses de debut de fin d'extraction dans cdent:
! on extrait du caractere kpos1 au caractere kpos2
! En sortie: kres  resultat entier
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kpos1,kpos2,kres
character *(*) cdent
read(cdent(kpos1:kpos2),fmt=*) kres
end
subroutine carree(cdent,klent,preel)
! --------------------------------------------------------------------------
! **** *CARREE *  Extraction d'un reel d'une chaine de caracteres.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Auteur:           92-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! en entree: cdent chaine contenant le reel
! klent nombre de caracteres significatifs ecrits sur cdent
! en sortie: preel reel lu
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) klent
real(kind=jpreeusr) preel
character*(*) cdent
read(cdent(1:klent),fmt=*) preel
end
subroutine casc(cdop,kcrim,cdmot,kmot)
! --------------------------------------------------------------------------
! **** *CASC*  - Cassure d'une chaine en ses differents mots.
! --------------------------------------------------------------------------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 93-06
! --------------------------------------------------------------------------
! En entree: cdop chaine a casser.
! kcrim critere de mot:
! 1 si mot=suite continue de "non (blancs ou tabulations)".
! 2 si mot=suite continue de "a-z,A-Z,0-9,.,_".
! 3 si mot=suite continue de "0-9,E,e,+,-,.".
! 4 si mot=suite continue de "a-z,A-Z,0-9".
! 5 si mot=suite continue de "a-z,A-Z,0-9,_,-".
! 6 si mot=suite continue de "non (- ou . ou blancs ou tabulations)".
! 7 si mot=suite continue de "non .".
! 8 si mot=suite continue de "0-9".
! 9 si mot=suite continue de "non /".
! 10 si mot=suite continue de "non ,".
! < 0 si mot=suite continue de "non CHAR(-kcrim)".
!
! En sortie: kmot nombre de mots trouves sur cdop.
! cdmot tableau contenant ces mots.
! --------------------------------------------------------------------------
#include"lfatail.h"
integer(kind=jpintusr) kcrim,kmot
character*(*) cdop
character*(*) cdmot(180)
character*01 cl1
logical lllet
kmot=0
ipresmot=0
ic=len(cdop)
do jc=1,ic
	cl1=cdop(jc:jc)
	iasc=ichar(cl1)
	!
	! lllet: vrai si le caractère est
	! une lettre ASCII accentuée.
	!
	lllet=.false.
	if(iasc >= 192.and.iasc <= 198) then
		lllet=.true.
	elseif(iasc == 199) then
		lllet=.true.
	elseif(iasc >= 200.and.iasc <= 203) then
		lllet=.true.
	elseif(iasc >= 204.and.iasc <= 207) then
		lllet=.true.
	elseif(iasc >= 210.and.iasc <= 214) then
		lllet=.true.
	elseif(iasc >= 217.and.iasc <= 220) then
		lllet=.true.
	elseif(iasc >= 221.and.iasc <= 221) then
		lllet=.true.
	elseif(iasc >= 224.and.iasc <= 229) then
		lllet=.true.
	elseif(iasc >= 231.and.iasc <= 231) then
		lllet=.true.
	elseif(iasc >= 232.and.iasc <= 235) then
		lllet=.true.
	elseif(iasc >= 236.and.iasc <= 239) then
		lllet=.true.
	elseif(iasc >= 242.and.iasc <= 246) then
		lllet=.true.
	elseif(iasc >= 249.and.iasc <= 252) then
		lllet=.true.
	endif
	if(kcrim == 1) then
		if(cl1 /= ' '.and.cl1 /= '	') then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 2) then
		if((cl1 >= 'a'.and.cl1 <= 'z') &
& 		.or.(cl1 >= 'A'.and.cl1 <= 'Z') &
& 		.or.(cl1 >= '0'.and.cl1 <= '9') &
& 		.or.cl1 == '_' &
& 		.or.cl1 == '.'.or.lllet) then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 3) then
		if(cl1 == 'E'.or.cl1 == 'e') then
			if(jc == 1.or.jc == ic) then
				!
				! On est en présence d'un "E" en début
				! ou en fin de ligne. Ce mot n'est pas un
				! nombre valide.
				!
				imot=0
			else
				!
				! On est en présence d'un "E"
				! à l'intérieur de la ligne.
				! Ce "E" sera celui d'une exponentiation
				! ssi le caractère qui le précède est
				! un chiffre et que le suivant est un chiffre
				! ou un signe.
				!
				if(((cdop(jc-1:jc-1) >= '0' &
& 				.and.cdop(jc-1:jc-1) <= '9').or.cdop(jc-1:jc-1) == '.') &
& 				.and.((cdop(jc+1:jc+1) == '+' &
& 				.or.cdop(jc+1:jc+1) == '-'.or.(cdop(jc+1:jc+1) >= '0' &
& 				.and.cdop(jc+1:jc+1) <= '9')))) then
					imot=1
				else
					imot=0
				endif
			endif
		elseif(cl1 == '+'.or.cl1 == '-') then
			if(jc == 1) then
				!
				! On est en présence d'un signe en début
				! de ligne.
				!
				imot=0
				if(ic == 1) then
					!
					! La ligne a un seul caractère, ce signe!...
					!
					imot=0
				else
					!
					! Le caractère suivant doit être
					! un chiffre ou séparateur décimal.
					!
					if((cdop(2:2) >= '0'.and.cdop(2:2) <= '9') &
& 					.or.cdop(2:2) == '.') then
						imot=1
					else
						imot=0
					endif
				endif
			elseif(jc == ic) then
				!
				! On est en présence d'un signe en fin
				! de ligne.
				!
				imot=0
			else
				!
				! On est en présence d'un signe à l'intérieur de la ligne.
				! Le caractère suivant doit être
				! un chiffre ou séparateur décimal,
				! et le précédent doit être un "e" ou "E" ou un non chiffre non point
				! (ce afin de séparer les cas d'entrée incorrectes du type 3.63E00-3.7E07,
				! qui conduirait au deux mots 3.63E00 et 3.7E07;
				! la sortie de mots sera en ce dernier cas fausse, mais l'entrée
				! de l'utilisateur l'était également!...).
				!
				if(((cdop(jc+1:jc+1) >= '0'.and.cdop(jc+1:jc+1) <= '9') &
& 				.or.cdop(jc+1:jc+1) == '.') &
& 				.and.(cdop(jc-1:jc-1) /= '.'.and. &
& 				(cdop(jc-1:jc-1) < '0'.or.cdop(jc-1:jc-1) > '9'))) then
					imot=1
				else
					imot=0
				endif
			endif
		elseif(cl1 >= '0'.and.cl1 <= '9') then
			imot=1
		elseif(cl1 == '.') then
			if(jc == 1) then
				!
				! On est en présence d'un point en début
				! de ligne.
				! Le caractère suivant doit être
                ! un chiffre.
				!
				if(cdop(2:2) >= '0'.and.cdop(2:2) <= '9') then
					imot=1
				else
					imot=0
				endif
			elseif(jc == ic) then
				!
				! On est en présence d'un point en fin
				! de ligne.
				! Le caractère précédent doit être
				! un chiffre.
				!
				if(cdop(jc-1:jc-1) >= '0'.and.cdop(jc-1:jc-1) <= '9') then
                    imot=1
                else
                    imot=0
                endif
			else
				!
				! On est en présence d'un point à l'intérieur de la ligne.
				! Le caractère suivant ou précédent doit être
				! un chiffre.
				!
				if((cdop(jc+1:jc+1) >= '0'.and.cdop(jc+1:jc+1) <= '9') &
& 				.or.(cdop(jc-1:jc-1) >= '0'.and.cdop(jc-1:jc-1) <= '9')) then
                    imot=1
                else
                    imot=0
                endif
			endif
		else
			imot=0
		endif
	elseif(kcrim == 4) then
		if((cl1 >= 'a'.and.cl1 <= 'z') &
& 		.or.(cl1 >= 'A'.and.cl1 <= 'Z') &
& 		.or.(cl1 >= '0'.and.cl1 <= '9') &
& 		.or.lllet) then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 5) then
		if((cl1 >= 'a'.and.cl1 <= 'z') &
& 		.or.(cl1 >= 'A'.and.cl1 <= 'Z') &
& 		.or.(cl1 >= '0'.and.cl1 <= '9') &
& 		.or.cl1 == '_' &
& 		.or.cl1 == '-' &
& 		.or.lllet) then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 6) then
		if(cl1 /= ' '.and.cl1 /= '	'.and.cl1 /= '-'.and.cl1 /= '.') then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 7) then
		if(cl1 /= '.') then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 8) then
		if(cl1 >= '0'.and.cl1 <= '9') then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 9) then
		if(cl1 /= '/') then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim == 10) then
		if(cl1 /= ',') then
			imot=1
		else
			imot=0
		endif
	elseif(kcrim < 0) then
		if(cl1 /= char(-kcrim)) then
			imot=1
		else
			imot=0
		endif
	else
		print*,'CASC/ERREUR: option kcrim=',kcrim,' non reconnue!...'
		stop 'call abort'
	endif
	if(imot == 0.and.ipresmot == 0) then
		!
		! Le caractère courant est hors d'un mot, le précédent aussi.
		!
	elseif(imot == 0) then
		!
		! Le caractère courant est hors d'un mot, le précédent était dedans.
		!
		kmot=kmot+1
		cdmot(kmot)=cdop(ipresmot:jc-1)
		ipresmot=0
	elseif(ipresmot == 0) then
		!
		! Le caractère courant est dans un mot, le précédent était en dehors.
		!
		ipresmot=jc
	endif
	if(jc == ic.and.imot /= 0) then
		!
		! Le dernier caractère de la ligne est dans un mot.
		!
		kmot=kmot+1
		cdmot(kmot)=cdop(ipresmot:jc)
	endif
enddo
end
subroutine intcar(kent,cdcar,kcar)
! --------------------------------------------------------------------------
! **** *INTCAR*  - Conversion entier > chaine de caractere
! --------------------------------------------------------------------------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 95-03
! --------------------------------------------------------------------------
! En entree:
! kent entier a convertir
! En sortie:
! cdcar chaine recevant l'entier
! kcar: nombre de caracteres ecrits sur cdcar
! --------------------------------------------------------------------------
#include"lfatail.h"
integer(kind=jpintusr) kcar,kent
character *(*) cdcar
character*1 clint
cdcar=' '
ient=kent
kcar=0
!
! --------------------------------------------------------------------------
!
! Traitement du signe.
if(ient < 0) then
	isign=-1
	ient=-ient
else
	isign=1
endif
!
! --------------------------------------------------------------------------
!
! Conversion entier vers chaine de caracteres.
  100 continue
imod=mod(ient,10)
kcar=kcar+1
write(cdcar(kcar:kcar),fmt='(i1.1)') imod
ient=ient/10
if(ient /= 0) goto 100
!
! --------------------------------------------------------------------------
!
! A ce stade, cdcar(1:kcar) contient l'entier cherche,
! mais ecrit en sens inverse.
! On va operer une symetrie centrale sur cdcar.
do jc=1,kcar/2
	clint=cdcar(jc:jc)
	cdcar(jc:jc)=cdcar(kcar-jc+1:kcar-jc+1)
	cdcar(kcar-jc+1:kcar-jc+1)=clint
enddo
!
! --------------------------------------------------------------------------
!
! Traitement du signe: ajout du "-" si necessaire.
if(isign == -1) then
	cdcar='-'//cdcar(1:kcar)
	kcar=kcar+1
endif
end
subroutine majmin(cdce,cdcs)
! --------------------------------------------------------------
! **** *MAJMIN* Passage majuscules > minuscules.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   95-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree: cdce
! En sortie: cdcs
! --------------------------------------------------------------
character*(*) cdce, cdcs
character*1 cl1
cdcs=' '
do jc=1,len(cdce)
	cl1=cdce(jc:jc)
	if(cl1 >= 'A'.and.cl1 <= 'Z') cl1=char(ichar(cl1)+32)
	cdcs(jc:jc)=cl1
enddo
end
subroutine minmaj(cdce,cdcs)
! --------------------------------------------------------------
! **** *MINMAJ* Passage minuscules > majuscules.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   95-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree: cdce
! En sortie: cdcs
! --------------------------------------------------------------
character*(*) cdce, cdcs
character*1 cl1
cdcs=' '
do jc=1,len_trim(cdce)
	cl1=cdce(jc:jc)
	if(cl1 >= 'a'.and.cl1 <= 'z') cl1=char(ichar(cl1)-32)
	cdcs(jc:jc)=cl1
enddo
end
subroutine oteblancs(cdent,cdsor,klsor)
! --------------------------------------------------------------------------
! **** *oteblancs* Suppression des blancs et tabulations d'une chaîne.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Auteur:  J.M.Piriou
! -------
! Original : 91-11
! ----------
! Modifications:
! --------------------------------------------------------------------------
! Exemple:
! cdent='    sisi non'
! =====>
! cdsor='sisinon'
! klsor=7
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) klsor,jtest
character *(*) cdent,cdsor
klsor=0
cdsor=' '
do jtest=1,len(cdent)
	if(cdent(jtest:jtest) /= ' '.and.cdent(jtest:jtest) /= '	') then
		klsor=klsor+1
		cdsor(klsor:klsor)=cdent(jtest:jtest)
	endif
enddo
end
subroutine otedbl(cdce,cdcs)
! --------------------------------------------------------------
! **** *otedbl* Ote d'une chaîne les double blancs, i.e. on n'aura plus deux blancs consécutifs.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree: cdce
! En sortie: cdcs
! --------------------------------------------------------------
character*(*) cdce, cdcs
cdcs=' '
cdcs(1:1)=cdce(1:1)
ics=1
do jc=2,len_trim(cdce)
	if(cdce(jc:jc) /= ' ' .or. cdce(jc-1:jc-1) /= ' ') then
		ics=ics+1
		cdcs(ics:ics)=cdce(jc:jc)
	endif
enddo
end
subroutine posit(cdmot,klmot,kmot,cdc,kdebmot,kfinmot,ldreel)
! --------------------------------------------------------------
! **** *POSIT* Position des différents mots dans la chaîne principale.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-08, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! cdmot: mots de la chaîne cdc.
! klmot: dimension physique de cdmot, kdebmot, kfinmot.
! kmot: nombre de mots dans la chaîne cdc.
! cdc: chaîne principale, chaîne de laquelle les mots ont été extraits.
! En sortie:
! kdebmot: kdebmot(jmot) contient la position dans cdc (en caractères depuis la gauche) du début du mot numéro jmot.
! kfinmot: kfinmot(jmot) contient la position dans cdc (en caractères depuis la gauche) de la fin du mot numéro jmot.
! --------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) klsor,klmot,kmot
integer(kind=jpintusr) idebrech,jmot,ilmot,ipos
character*(*) cdmot(klmot)
character*(*) cdc
integer(kind=jpintusr) kdebmot(klmot)
integer(kind=jpintusr) kfinmot(klmot)
logical ldreel(klmot)
idebrech=1 ! position de début de recherche pour la fonction "index".
do jmot=1,kmot
	!
	! Longueur du mot jmot de la ligne.
	!
	ilmot=len_trim(cdmot(jmot))
	!
	! On recherche le mot jmot dans la ligne cdc.
	!
	ipos=index(cdc(idebrech:),cdmot(jmot)(1:ilmot))
	kdebmot(jmot)=ipos+idebrech-1
	kfinmot(jmot)=ipos+idebrech-1+ilmot-1
	!
	! On recherche un point décimal
	! afin de savoir si on est en présence
	! d'un réel ou d'un entier.
	!
	ipos=index(cdc(kdebmot(jmot):kfinmot(jmot)),'.')
	if(ipos == 0) then
		!
		! Pas de point décimal. Le nombre est un entier.
		!
		ldreel(jmot)=.false.
	else
		!
		! Point décimal. Le nombre est un réel.
		!
		ldreel(jmot)=.true.
	endif
	idebrech=kdebmot(jmot)+ilmot
enddo
end
subroutine posnbc(cdchaine,k1,k2,kdebut,kfin)
! --------------------------------------------------------------------------
! **** *posnbc*  - Reperage du debut et de la fin du premier mot d'une chaine.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Auteur:  J.M.Piriou
! -------
! Original : 91-11
! ----------
! Modifications:
! --------------------------------------------------------------------------
! En entree
! cdchaine: chaine a estimer.
! k1, k2: zone a explorer.
! En sortie
! kdebut: premier caractere non blanc.
! kfin: dernier caractere non blanc.
! --------------------------------------------------------------------------
implicit logical (l)
#include"lfatail.h"
integer(kind=jpintusr) k1,k2,kdebut,kfin
character*(*) cdchaine
lldebut=.true.
kdebut=k1-1
kfin=k2
do jc=k1,k2
	if(cdchaine(jc:jc) == ' ') then
		if(lldebut) then
			kdebut=jc
		else
			kfin=jc-1
			goto 200
		endif
	else
		lldebut=.false.
	endif
enddo
  200 continue
kdebut=kdebut+1
return
end
subroutine rech(cd1,kdeb1,kfin1,cd2,kdeb2,kfin2,krep)
! --------------------------------------------------------------------------
! **** *rech*  - Recherche de la chaine cd1 dans la chaine cd2.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Methode:
! --------
! Auteur:  J.M.Piriou
! -------
! Original : 91-11
! ----------
! Modifications:
! --------------------------------------------------------------------------
! En entree
! cd1(kdeb1:kfin1): chaine-occurrence a chercher.
! cd2(kdeb2:kfin2): chaine dans laquelle chercher.
! si kdeb1=kfin1=0, recherche dans toute la chaîne cd1.
! si kdeb2=kfin2=0, recherche dans toute la chaîne cd2.
! En sortie
! krep: 0 si chaine non trouvee, sinon position de debut de cd1 dans cd
! Si cd1 est presente plusieurs fois dans cd2, la position
! de la premiere est retournee.
! --------------------------------------------------------------------------
#include"lfatail.h"
integer(kind=jpintusr) kdeb1,kfin1,kdeb2,kfin2,krep 
character*(*) cd1
character*(*) cd2
if(kdeb1 == 0) kdeb1=1
if(kdeb2 == 0) kdeb2=1
if(kfin1 == 0) kfin1=len(cd1)
if(kfin2 == 0) kfin2=len(cd2)
il1=kfin1-kdeb1+1
il2=kfin2-kdeb2+1
krep=0
if(il1 > il2) return
do jc=0,il2-il1
	if(cd1(kdeb1:kfin1) == cd2(kdeb2+jc:kdeb2+jc+il1-1)) then
		krep=kdeb2+jc
		goto 200
	endif
enddo
  200 continue
return
end
subroutine reecar(px,kopt,knc,cdsor,ksor)
! --------------------------------------------------------------------------
! **** *REECAR *  - Conversion reel > chaine de caracteres.
! --------------------------------------------------------------------------
!
! Sujet:
! ------
!
! Arguments explicites: /
! ---------------------
!
! Arguments implicites: /
! ---------------------
!
! Methode:
! --------
!
! Externes: /
! ---------
!
! Original:
! ---------
! 92-05-19, J.M. Piriou
!
! Modifications:
! --------------
!
! --------------------------------------------------------------------------
! En Entree       :
! px          : reel a convertir
! kopt: option:
! si kopt=-1 on veut knc chiffres significatifs.
! si kopt=-2 on veut un affichage fixe ou scientifique
! ...........suivant la valeur absolue du réel
! ...........(fixe pour les valeurs absolues "proches" de 1).
! ...........Si knc=3, 3 chiffres significatifs garantis, 7 sinon.
! si kopt>=0 et knc=0 affichage a kopt chiffres
! ........... apres la virgule.
! si kopt>=0 et knc>0 affichage a kopt chiffres
! ...........apres la virgule, et si de plus px est voisin d'un entier
! ...........a mieux que 10.**(-knc), on l'affiche
! ...........au format entier.
! En Sortie       :
! cdsor         : chaine de caracteres resultante
! ksor          : nombre de caracteres ecrits sur cdsor
! --------------------------------------------------------------------------
! Exemples:
! px          kopt        knc         cdsor       ksor
! 6.22        -1          2           '6.2'       3
! 1.E-6       -1          3           '1.00E-6'   8
! 1.E-6       -1          indifférent '1.00E-6'   8
! -6.22       1           0           '-6.2'      4
! 6.00012     2           0           '6.00'      4
! 6.00012     2           2           '6'         1
! 1.E-6       3           0           '0.000'     5
! 1.E-6       3           5           '0'         1
! .1          2           4           '0.10'      4
! --------------------------------------------------------------------------
implicit none
#include"lfatail.h"
integer(kind=jpintusr) kopt,knc,ksor,isign,ilog,ilonlog,ix,iaff,ilog2 &
&,iafftmp,jch,nint,mod,inint
real(kind=jpreeusr) px,zx,zlog,zmant,z10,zvala,zxnor,zint,zc10,zp10
character *(*) cdsor
character *144 clxprov
character *12 clmanttmp
character *11 clmant
character *10 clmant2
character *04 cllog
character *22 clfor
character *08 clforf
if(kopt == -1) then
	! On veut un certain nombre de chiffres SIGNIFICATIFS.
	if(px == 0.) then
		! Cas nombre nul
		cdsor='0.'
		ksor=2
	else
		! Cas general
		zx=px
		if(zx < 0.) then
			zx=-zx
			isign=-1
		else
			isign=1
		endif
		zlog=log(zx)/log(10.)
		if(zlog < 0.) then
			ilog=int(zlog)-1
		else
			ilog=int(zlog)
		endif
		! On calcule la mantisse de zx, soit un nombre
		! zmant tel que 1.<=zmant<10.
		zmant=zx/10.**real(ilog)
		! On ne conserve que knc chiffres significatifs
		z10=10.**real(knc-1)
		zmant=nint(zmant*z10)/z10
		write(clmanttmp,fmt='(f12.9)') zmant
		clmant=clmanttmp(2:12)
		if(clmant(1:1) == '*') then
			! Il y a eu un probleme lors de l'arrondi
			! permettant l'obtention de la mantisse de px,
			! ce pour les valeurs de mantisse voisines de 1. ou 10.
			! On reecrit ici le resultat.
			clmant='1.000000000'
			if(zmant < 5.) then
				ilog=ilog-1
			else
				ilog=ilog+1
			endif
		endif
		if(ilog <= -100) then
			write(cllog,fmt='(i4)') ilog
			ilonlog=4
		elseif(ilog <= -10) then
			write(cllog,fmt='(i3)') ilog
			ilonlog=3
		elseif(ilog < 0) then
			write(cllog,fmt='(i2)') ilog
			ilonlog=2
		elseif(ilog < 10) then
			write(cllog,fmt='(i1)') ilog
			ilonlog=1
		elseif(ilog < 100.) then
			write(cllog,fmt='(i2)') ilog
			ilonlog=2
		else
			write(cllog,fmt='(i3)') ilog
			ilonlog=3
		endif
		clmant2=clmant(1:1)//clmant(3:11)
		if(ilog >= 5) then
			!
			! Cas des nombres x <= 10**5.
			!
			clxprov=clmant(1:knc+1)//'E'//cllog
			ksor=knc+1+1+ilonlog
		elseif(ilog >= 0) then
			!
			! Cas des nombres 1.<= x < 10**5.
			!
			if(knc >= ilog+2) then
				clxprov=clmant2(1:ilog+1)//'.'//clmant2(ilog+2:knc)
				ksor=knc+1
			else
				clxprov=clmant2(1:ilog+1)//'.'
				ksor=ilog+2
			endif
		elseif(ilog >= -1) then
			clxprov='0.'//clmant2(1:knc)
			ksor=knc+2
		elseif(ilog >= -2) then
			clxprov='0.0'//clmant2(1:knc)
			ksor=knc+3
		else
			clxprov=clmant(1:knc+1)//'E'//cllog
			ksor=knc+1+1+ilonlog
		endif
		if(isign == -1) then
			cdsor='-'//clxprov(1:ksor)
			ksor=ksor+1
		else
			cdsor=clxprov(1:ksor)
		endif
	endif
elseif(kopt == -2) then
	!
	! On veut un affichage fixe ou scientifique
	! suivant la valeur absolue du réel.
	!
	if(knc == 3) then
		!
		! On affiche 3 chiffres significatifs.
		!
		zvala=abs(px)
		zx=px
		if(zvala == 0.) then
			!
			! Nombre nul.
			! Choix du format fixe.
			!
			clfor='(F11.4)'
			ksor=11
		elseif(zvala >= 100000..or.zvala < 0.01) then
			!
			! Grandes ou petites valeurs absolues.
			! Choix du format scientifique.
			!
			clfor='(ES11.5)'
			ksor=11
		else
			!
			! Valeurs absolues moyennes.
			! Choix du format fixe.
			!
			clfor='(F11.4)'
			ksor=11
		endif
	else
		!
		! On affiche 7 chiffres significatifs.
		!
		zvala=abs(px)
		zx=px
		if(zvala == 0.) then
			!
			! Nombre nul.
			! Choix du format fixe.
			!
			clfor='(F15.8)'
			ksor=15
		elseif(zvala >= 10000.) then
			!
			! Grandes valeurs absolues.
			! Choix du format scientifique.
			!
			clfor='(ES15.7)'
			ksor=15
		elseif(zvala >= 1.E-03) then
			!
			! Valeurs absolues moyennes.
			! Choix du format fixe.
			!
			clfor='(F15.9)'
			ksor=15
		else
			!
			! Petites valeurs absolues.
			! Choix du format scientifique.
			!
			clfor='(ES15.7)'
			ksor=15
		endif
	endif
	write(cdsor,fmt=clfor) zx
else
	! On veut un affichage a kopt chiffres apres la virgule.
	zx=px
	if(zx /= 0.) then
		if(zx > 0.) then
			isign=1
		else
			isign=-1
			zx=-zx
		endif
		ilog=nint(log(zx)/log(10.)-0.5)
		! Pour les nombres dont la mantisse est voisine de 1., il y a ris
		! d'erreur d'arrondi. On teste si ce cas d'erreur est arrive,
		! et si oui on modifie la valeur de ilog.
		zxnor=zx/10.**real(ilog)
		if(zxnor < 1.) then
			ilog=ilog-1
		elseif(zxnor >= 10.) then
			ilog=ilog+1
		endif
	else
		isign=1
		ilog=0
	endif
	zint=zx*10.**real(knc)
	! Arrondi de zint a l'entier le plus proche.
	if(zint == 0.) then
		ix=0
	else
		ix=nint(zint-0.5)
	endif
	zc10=10.
	zp10=zc10**real(knc,jpreeusr)
	inint=nint(zp10)
	if(mod(ix,inint) == 0.and.knc /= 0) then
		! Le reel est voisin d'un entier
		iaff=ix/nint(10.**real(knc))
		if(iaff == 0) then
			cdsor='0'
			ksor=1
		else
			! Obtention du nombre de chiffres ilog2 de l'entier iaff
			ilog2=1
			iafftmp=iaff
			do jch=0,12
				iafftmp=iafftmp/10
				if(iafftmp /= 0) ilog2=ilog2+1
			enddo
			write(clfor,fmt='(A2,I3.3,A1)')'(I',ilog2,')'
			write(cdsor,fmt=clfor) iaff
			ksor=ilog2
			if(isign == -1) then
				cdsor='-'//cdsor(1:ksor)
				ksor=ksor+1
			endif
		endif
	else
		! Le reel n'est pas voisin d'un entier.
		! On l'affiche au format reel + kopt ch. apres la virgule.
		if(ilog < 0) ilog=0
		ksor=ilog+2+kopt
		write(clforf,fmt='(A2,I2.2,A1,I2.2,A1)') '(F',ksor,'.',kopt,')'
		write(cdsor,fmt=clforf) zx
		if(cdsor(1:1) == ' ') then
			cdsor(1:1)='0'
		endif
		if(isign == -1) then
			cdsor='-'//cdsor(1:ksor)
			ksor=ksor+1
		endif
	endif
endif
end
