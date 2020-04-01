
      subroutine itreadr(varname,zdim,tstep,nstep,zchp)
! --------------------------------------------------------------------------
! **** 
! ****
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
! Auteur:   2020-03: R. Roehrig
! -------
! Modifications:
! --------------------------------------------------------------------------
      implicit none
      character(100),  intent(in)  :: varname     
      integer(kind=4), intent(in)  :: zdim
      real(kind=4),    intent(in)  :: tstep
      integer(kind=4), intent(in)  :: nstep
      real(kind=4),    intent(out) :: zchp(nstep,zdim)
      integer(kind=4) :: iul,ilong,ierr, jc, it
      character*100   :: filename
      character*2     :: cltype
      character*12    :: timestamp
      real(kind=8)    :: timestep
      real(kind=8)    :: time1, time2
      real(kind=4)    :: tmp(zdim)
!
! Quelques initialisations
!
      iul=7
!
! Boucle sur les fichiers
!
      do it=1,nstep 
!
! Ouverture du fichier.
!

        WRITE(timestamp,FMT='(F12.4)') (it-1)*tstep/3600.
        DO jc=1,12
          IF(timestamp(JC:JC) == ' ') timestamp(JC:JC)='0'
        ENDDO
        WRITE(filename,FMT='(9A)')'LFA/Out.',timestamp,'.lfa'

!        print*, filename

        call lfaouv(iul,trim(filename),'R')
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
            call lfareadr(ilong,iul,varname,zchp(it,:))
          else
            print*,'readr: ERREUR interne: type de champ non prevu!...'
            stop
          endif
        endif
!
! Fermeture du fichier.
!
        call lfafer(iul)

      enddo

      end

      subroutine itreadi(varname,zdim,tstep,nstep,zchp)
! --------------------------------------------------------------------------
! **** 
! **** 
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
! Auteur:  2020-03: R. Roehrig
! -------
! Modifications:
! --------------------------------------------------------------------------
      implicit none
      character(100),  intent(in)  :: varname     
      integer(kind=4), intent(in)  :: zdim
      real(kind=4),    intent(in)  :: tstep
      integer(kind=4), intent(in)  :: nstep
      integer(kind=4), intent(out) :: zchp(nstep,zdim)
      integer(kind=4) :: iul,ilong,ierr, jc, it
      character*100   :: filename
      character*2     :: cltype
      character*12    :: timestamp
      real(kind=8)    :: timestep
      real(kind=8)    :: time1, time2
      real(kind=4)    :: tmp(zdim)
!
! Quelques initialisations
!
      iul=7
!
! Boucle sur les fichiers
!
      do it=1,nstep 
!
! Ouverture du fichier.
!

        WRITE(timestamp,FMT='(F12.4)') (it-1)*tstep/3600.
        DO jc=1,12
          IF(timestamp(JC:JC) == ' ') timestamp(JC:JC)='0'
        ENDDO
        WRITE(filename,FMT='(9A)')'LFA/Out.',timestamp,'.lfa'

!        print*, filename

        call lfaouv(iul,trim(filename),'R')
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
            call lfareadi(ilong,iul,varname,zchp(it,:))
          else
            print*,'readr: ERREUR interne: type de champ non prevu!...'
            stop
          endif
        endif
!
! Fermeture du fichier.
!
        call lfafer(iul)

      enddo

      end

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
      character(100),  intent(in) :: filename
      character(100),  intent(in) :: varname     
      integer(kind=4), intent(in) :: zdim
      real(kind=4),    intent(out) :: zchp(zdim)
      integer(kind=4) iul,ilong,ierr
      character*2 cltype
      real(kind=8) time1, time2
!
! Saisie de la ligne de commande.
!
!
! Ouverture du fichier.
!
      iul=7

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
      character(100),  intent(in)  :: filename
      character(100),  intent(in)  :: varname
      integer(kind=4), intent(in)  :: zdim
      integer(kind=4), intent(out) :: zchp(zdim)
      integer(kind=4) iul,ilong,ierr
      character*2 cltype
!
! Saisie de la ligne de commande.
!
!
! Ouverture du fichier.
!
      iul=7
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
      integer(kind=4) iul,ilong,ierr
      character*2 cltype
!
! Saisie de la ligne de commande.
!
!
! Ouverture du fichier.
!
      iul=7
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
      call lfalecc(kul,cdna,klbouc,zcha,ilong,ierr)
      end
      
