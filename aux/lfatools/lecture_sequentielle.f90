PROGRAM LFAECT
!     --------------------------------------------------------------
!     **** *lfaect* Ecarts-types des articles r�els d'un fichier LFA.
!     --------------------------------------------------------------
!     Sujet:
!     Arguments explicites:
!     Arguments implicites:
!     Methode:
!     Externes:
!     Auteur:   97-10, J.M. Piriou.
!     Modifications:
!     --------------------------------------------------------------
!     En entree:
!     En sortie:
!     --------------------------------------------------------------
IMPLICIT CHARACTER*100 (C)
CHARACTER*1 CLTYPE
!
!     Saisie de la ligne de commande.
!
CALL GETARGP(1,CLF)
IF(CLF == ' ') THEN
  PRINT* &
&   ,'Sortie des �carts-types des articles r�els d''un fichier LFA.'
  PRINT*,'Utilisation: lfaect nomf'
  STOP
ENDIF
!
!     Ouverture du fichier.
!
iullfa1=72
CALL LFAOUV(iullfa1,CLF,'R')
WRITE(*,'(9a)') 'lfaect du fichier ',CLF(1:len_trim(clf)),':'
ierr=0
do while(ierr == 0)
	!
	!     Renseignements sur l'article suivant du fichier.
	!
	CLNA=' '
	CALL LFACAS(iullfa1,CLNA,CLTYPE,ILONG,ierr)
	IF(ierr == 0) THEN
		!
		!       On n'est pas en fin de fichier.
		!
		IF(CLTYPE == 'R') THEN
			!
			!         Article de type r�el.
			!         On le lit.
			!
			CALL LITEC(iullfa1,CLNA,ILONG)
		ELSE
			!
			!         Article non r�el.
			!         On passe au suivant.
			!
			write(*,fmt=*) 'L''article "',clna(1:len_trim(clna)),'" n''est pas r�el (',cltype(1:len_trim(cltype)),'); on l''ignore.'
			CALL LFAAVAN(iullfa1)
		ENDIF
	ENDIF
enddo
!
!     Fermeture du fichier.
!
CALL LFAFER(iullfa1)
END
SUBROUTINE LITEC(KUL,CDNA,KLONG)
!     --------------------------------------------------------------
!     **** *litec* Calcul de moyenne et �cart-type.
!     --------------------------------------------------------------
!     Sujet:
!     Arguments explicites:
!     Arguments implicites:
!     Methode:
!     Externes:
!     Auteur:   97-10, J.M. Piriou.
!     Modifications:
!     --------------------------------------------------------------
!     En entree:
!     En sortie:
!     --------------------------------------------------------------
REAL ZVAL(KLONG)
CHARACTER*(*) CDNA
CALL LFALECR(KUL,CDNA,KLONG,ZVAL,ILONG,IERR)
!     Ligne suivante: somme des x.
ZSX=0.
!     Ligne suivante: somme des x**2.
ZSX2=0.
!
!     Boucle de calcul des sommes x et x**2.
!
DO JB=1,KLONG
  ZSX=ZSX+ZVAL(JB)
  ZSX2=ZSX2+ZVAL(JB)*ZVAL(JB)
ENDDO
!
!     Calcul des moyenne et �cart-type.
!
ZMOY=ZSX/FLOAT(KLONG)
ZECT=SQRT(ZSX2/FLOAT(KLONG)-ZMOY*ZMOY)
!
!     Longueur de la cha�ne-nom de l'article.
!
ilna=len_trim(cdna)
!
!     Affichage.
!
PRINT*,CDNA(1:ILNA),' moyenne=',ZMOY,' ecart-type=',ZECT
END
