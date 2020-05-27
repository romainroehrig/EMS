! Compilation
! ATTENTION : probleme avec big-endian... Ai rajoute l'option
! convert='BIG-ENDIAN' pour tous les open dans lfa.f90
!      
!/opt/cdat/bin/f2py -m lfa -h famusc.pyf famusc.f90
!gfortran -fconvert=big-endian -cpp  -c -fPIC lfa.f90
!gfortran -fconvert=big-endian -cpp  -c -fPIC famusc.f90
!/opt/cdat/bin/f2py -lm -lgfortran --f77flags='-fPIC -fconvert=big-endian -cpp' --f90flags='-fPIC -fconvert=big-endian -cpp' --fcompiler=gnu95 -c famusc.pyf famusc.o lfa.o  

!new test
! Ce n'est pas le f2py compatible avec cdat...      
! ATTENTION : probleme avec big-endian... Ai rajoute l'option
! convert='BIG-ENDIAN' pour tous les open dans lfa.f90      
!f2py -m lfa -h famusc.pyf famusc.f90 (comment lfaread* subroutines)
!gfortran -I./ -fconvert=big-endian -cpp  -c -fPIC cllang.f90
!gfortran -I./ -fconvert=big-endian -cpp  -c -fPIC caracteres_lfa.f90
!gfortran -I./ -fconvert=big-endian -cpp  -c -fPIC lfa.f90
!ar r liblfa.a *.o
!gfortran -L./ -llfa -I./ -fconvert=big-endian -cpp  -c -fPIC -o famusc.o famusc.f90
!f2py -L./ -llfa -lm -lgfortran --f77flags='-L./ -llfa -I./ -fPIC -fconvert=big-endian -cpp' --f90flags='-L./ -llfa -I./ -fPIC -fconvert=big-endian -cpp' --fcompiler=gnu95 -c famusc.pyf famusc.o      

      subroutine readr(filename,varname,zdim,zchp)
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
      character*(*) filename,varname     
      integer(kind=4) zdim
      real(kind=4) zchp(zdim)
      integer(kind=4) iul,iuls,ilong,ierr,ilnomf
      character*2 cltype
!f2py intent(in) filename, varname, zdim
!f2py intent(out) zchp      
!
! Saisie de la ligne de commande.
!
!
! Ouverture du fichier.
!
      ilnomf=200
      iul=7
      iuls=17
      call lfaouv(iul,filename,'R')
!
! Recherche de l'article souhaite.
!
      call lfacas(iul,varname,cltype,ilong,ierr)
      if(ierr == 0) then
!
! L'article existe dans le fichier.
!
        if(cltype(1:1) == 'R') then
!
! Article de type reel 8.
!
          call lfareadr(ilong,iul,varname,zchp)
        else
          print*,'readr: ERREUR interne: type de champ non prevu!...'
          stop
        endif
        close(iuls)
      endif
!
! Fermeture du fichier.
!
      call lfafer(iul)
      end

      subroutine readi(filename,varname,zdim,zchp)
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
      character*(*) filename,varname     
      integer(kind=4) zdim
      integer(kind=4) zchp(zdim)
      integer(kind=4) iul,iuls,ilong,ierr,ilnomf
      character*2 cltype
!f2py intent(in) filename, varname, zdim
!f2py intent(out) zchp       
!
! Saisie de la ligne de commande.
!
!
! Ouverture du fichier.
!
      ilnomf=200
      iul=7
      iuls=17
      call lfaouv(iul,filename,'R')
!
! Recherche de l'article souhaite.
!
      call lfacas(iul,varname,cltype,ilong,ierr)
      if(ierr == 0) then
!
! L'article existe dans le fichier.
!
        if(cltype(1:1) == 'I') then
!
! Article de type reel 8.
!
          call lfareadi(ilong,iul,varname,zchp)
        else
          print*,'readr: ERREUR interne: type de champ non prevu!...'
          stop
        endif
        close(iuls)
      endif
!
! Fermeture du fichier.
!
      call lfafer(iul)
      end

      subroutine readc(filename,varname,zdim,zchp)
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
      character*(*) filename,varname     
      integer(kind=4) zdim
      character*400 zchp(zdim)
      integer(kind=4) iul,iuls,ilong,ierr,ilnomf
      character*2 cltype
!f2py intent(in) filename, varname, zdim
!f2py intent(out) zchp       
!
! Saisie de la ligne de commande.
!
!
! Ouverture du fichier.
!
      ilnomf=200
      iul=7
      iuls=17
      call lfaouv(iul,filename,'R')
!
! Recherche de l'article souhaite.
!
      call lfacas(iul,varname,cltype,ilong,ierr)
      if(ierr == 0) then
!
! L'article existe dans le fichier.
!
        if(cltype(1:1) == 'C') then
!
! Article de type reel 8.
!
          call lfareadc(ilong,iul,varname,zchp)
        else
          print*,'readr: ERREUR interne: type de champ non prevu!...'
          stop
        endif
        close(iuls)
      endif
!
! Fermeture du fichier.
!
      call lfafer(iul)
      end


      subroutine lfareadr(klbouc,kul,cdna,zreel)
! --------------------------------------------------------------
! **** *lfaaffr* Lecture de reels.
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
      integer(kind=4) klbouc,kul,ilong,ierr
      character*(*) cdna
      real(kind=4) zreel(klbouc)
!f2py intent(in) klbouc,kul,cdna
!f2py intent(out) zreel       
      call lfalecr(kul,cdna,klbouc,zreel,ilong,ierr)
      end

      subroutine lfareadi(klbouc,kul,cdna,zint)
! --------------------------------------------------------------
! **** *lfaaffi* Lecture d'entiers.
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
      integer(kind=4) klbouc,kul,ilong,ierr
      character*(*) cdna
      integer(kind=4) zint(klbouc)
!f2py intent(in) klbouc,kul,cdna
!f2py intent(out) zint      
      call lfaleci(kul,cdna,klbouc,zint,ilong,ierr)
      end

      subroutine lfareadc(klbouc,kul,cdna,zcha)
! --------------------------------------------------------------
! **** *lfaaffi* Lecture d'entiers.
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
      integer(kind=4) klbouc,kul,ilong,ierr
      character*(*) cdna
      character*400 zcha(klbouc)
!f2py intent(in) klbouc,kul,cdna
!f2py intent(out) zcha      
      call lfalecc(kul,cdna,klbouc,zcha,ilong,ierr)
      end
      
