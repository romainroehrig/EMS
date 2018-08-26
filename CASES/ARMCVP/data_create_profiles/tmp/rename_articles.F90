program lit
! --------------------------------------------------------------
! **** ** .
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
integer, parameter :: jppdt=97
integer, parameter :: jplev=300

real zcalday(jppdt)
real zyear(jppdt)
real zmonth(jppdt)
real zday(jppdt)
real zhour(jppdt)
real zminute(jppdt)
real zlh(jppdt)
real zsh(jppdt)
real zts(jppdt)

real za(jplev)
real zb(jplev)
real zprof(jplev)

real, allocatable :: zin(:,:)
real, allocatable :: ztempe(:,:)
real, allocatable :: zps_surdim(:,:) ! à jpdt donné, la valeur est la même quel que soit jniv.
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
read *,clin,clout
!
!-------------------------------------------------
! Lecture des coordonnées A et B.
!-------------------------------------------------
!
iullfa1=23
call lfaouv(iullfa1,clin,'R')
call lfaleci(iullfa1,'NLEV',1,ilev,ilong,ierr)
if(ilev > jplev) then
	write(*,fmt=*) 'rename_articles/ERREUR: recompiler avec jplev plus grand!...'
	write(*,fmt=*) ilev,jplev
	stop 'call abort'
endif
call lfaleci(iullfa1,'NTIME',1,itime1,ilong,ierr)
iprod=ilev*itime1
!
!-------------------------------------------------
! Lecture de la date.
!-------------------------------------------------
!
call lfalecr(iullfa1,'Calday',jppdt,zcalday,ilong,ierr)
call lfalecr(iullfa1,'Year',jppdt,zyear,ilong,ierr)
call lfalecr(iullfa1,'Month',jppdt,zmonth,ilong,ierr)
call lfalecr(iullfa1,'Day',jppdt,zday,ilong,ierr)
call lfalecr(iullfa1,'Hour',jppdt,zhour,ilong,ierr)
call lfalecr(iullfa1,'Minute',jppdt,zminute,ilong,ierr)
call lfalecr(iullfa1,'LH (upward W/m2)',jppdt,zlh,ilong,ierr)
call lfalecr(iullfa1,'SH (upward W/m2)',jppdt,zsh,ilong,ierr)
call lfalecr(iullfa1,'Ts',jppdt,zts,ilong,ierr)
!
!-------------------------------------------------
! Allocation des tableaux de variables en entrée et sortie.
!-------------------------------------------------
!
allocate(zin(itime1,ilev))
allocate(ztempe(itime1,ilev))
allocate(zps_surdim(itime1,ilev))
!
!-------------------------------------------------
! Boucle sur les instants de forçage.
!-------------------------------------------------
!
do jpdt=1,jppdt
	!
	!-------------------------------------------------
	! Ouverture du fichier LFA de sortie.
	!-------------------------------------------------
	!
	ztime=real(jpdt-1)/real(jppdt-1)*48.
	write(clfout,fmt='(a,a,f6.2,a)') clout(1:len_trim(clout)),'/Profile.',ztime,'.lfa'
	do jc=1,len_trim(clfout)
		if(clfout(jc:jc) == ' ') clfout(jc:jc)='0'
	enddo
	iullfa2=24
	call lfaouv(iullfa2,clfout,'W')
	!
	!-------------------------------------------------
	! Lat/lon.
	!-------------------------------------------------
	!
	zpi=4.*atan(1.)
	zlon=-97.5*zpi/180.
	zlat=36.61666*zpi/180.
	call lfaecrr(iullfa2,'LONGITUDE',zlon,1)
	call lfaecrr(iullfa2,'LATITUDE',zlat,1)
	!
	!-------------------------------------------------
	! Date.
	!-------------------------------------------------
	!
	iindat=nint(zyear(jpdt))*10000+nint(zmonth(jpdt))*100+nint(zday(jpdt))
	call lfaecri(iullfa2,'KINDAT',iindat,1)
	isssss=nint(zhour(1))*3600+nint(zminute(1))*60
	call lfaecri(iullfa2,'KSSSSS',iSSSSS,1)
	zstati=zhour(jpdt)*3600.+zminute(jpdt)*60.-isssss
	call lfaecrr(iullfa2,'RSTATI',zstati,1)
	call lfaecri(iullfa2,'KLEV',ilev,1)
	inebh=ilev+1
	call lfaecri(iullfa2,'KNEBH',inebh,1)
	ival=8 ; call lfaecri(iullfa2,'KVCLIV',ival,1)
	ival=0 ; call lfaecri(iullfa2,'KSGST',ival,1)
	ival=1 ; call lfaecri(iullfa2,'KCSS',ival,1)
	call lfacop(iullfa1,'VAH','VAH',iullfa2)
	call lfacop(iullfa1,'VBH','VBH',iullfa2)
	!
	!-------------------------------------------------
	! Profils de variables.
	!-------------------------------------------------
	!
	!
	!-------------------------------------------------
	! PUT0.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'u_wind_(m/s)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=zin(jpdt,jlev)
	enddo
	call lfaecrr(iullfa2,'PUT0',zprof,ilev)
	!
	!-------------------------------------------------
	! PVT0.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'v_wind_(m/s)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=zin(jpdt,jlev)
	enddo
	call lfaecrr(iullfa2,'PVT0',zprof,ilev)
	!
	!-------------------------------------------------
	! PTT0.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'Temp_(K)',iprod,ztempe,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=ztempe(jpdt,jlev)
	enddo
	call lfaecrr(iullfa2,'PTT0',zprof,ilev)
	!
	!-------------------------------------------------
	! PQT0.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'H2O_Mixing_Ratio_(g/kg)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zr=zin(jpdt,jlev)/1000.
		zprof(jlev)=zr/(1.+zr)
	enddo
	call lfaecrr(iullfa2,'PQT0',zprof,ilev)
	!
	!-------------------------------------------------
	! PSPT0.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'Area Mean Ps(mb)',iprod,zin,ilong,ierr)
	zval=log(zin(jpdt,1)*100.)
	call lfaecrr(iullfa2,'PSPT0',zval,1)
	!
	!-------------------------------------------------
	! PTS0.
	!-------------------------------------------------
	!
	ztsloc=zts(jpdt)
	call lfaecrr(iullfa2,'PTS0',ztsloc,1)
	call lfaecrr(iullfa2,'PTP0',ztsloc,1)
	zval=0. ; call lfaecrr(iullfa2,'PWL0',zval,1)
	zval=0. ; call lfaecrr(iullfa2,'PWS0',zval,1)
	zval=0. ; call lfaecrr(iullfa2,'PWSI0',zval,1)
	zval=800. ; call lfaecrr(iullfa2,'PWP0',zval,1)
	zval=0.02 ; call lfaecrr(iullfa2,'PWPI0',zval,1)
	zval=0. ; call lfaecrr(iullfa2,'PSNS0',zval,1)
	zval=0.1 ; call lfaecrr(iullfa2,'PGZ0F',zval,1)
	zval=0.01 ; call lfaecrr(iullfa2,'PGZ0HF',zval,1)
	zprof=0.
	ilev1=ilev+1
	call lfaecrr(iullfa2,'PNEBH',zprof,ilev1)
	!
	!-------------------------------------------------
	! PDIVT0.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'Wind_Div_(1/s)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=zin(jpdt,jlev)
	enddo
	call lfaecrr(iullfa2,'PDIVT0',zprof,ilev)
	!
	!-------------------------------------------------
	! PVORT0.
	!-------------------------------------------------
	!
	do jlev=1,ilev
		zprof(jlev)=0.
	enddo
	call lfaecrr(iullfa2,'PVORT0',zprof,ilev)
	!
	!-------------------------------------------------
	! PITM.
	!-------------------------------------------------
	!
	zval=1.
	call lfaecrr(iullfa2,'PITM',zval,1)
	!
	!-------------------------------------------------
	! PVRLAN.
	!-------------------------------------------------
	!
	zval=0. ; call lfaecrr(iullfa2,'PVRLAN',zval,1)
	zval=0. ; call lfaecrr(iullfa2,'PVRLDI',zval,1)
	zval=0. ; call lfaecrr(iullfa2,'PGETRL',zval,1)
	zval=25. ; call lfaecrr(iullfa2,'PARG',zval,1)
	zval=2. ; call lfaecrr(iullfa2,'PD2',zval,1)
	zval=3. ; call lfaecrr(iullfa2,'PIVEG',zval,1)
	zval=1.5 ; call lfaecrr(iullfa2,'PLAI',zval,1)
	zval=68. ; call lfaecrr(iullfa2,'PRSMIN',zval,1)
	zval=50. ; call lfaecrr(iullfa2,'PSAB',zval,1)
	zval=0.05 ; call lfaecrr(iullfa2,'PHV',zval,1)
	zval=0.2 ; call lfaecrr(iullfa2,'PVEG0',zval,1)
	!
	!-------------------------------------------------
	! PALBF.
	!-------------------------------------------------
	!
	zval=0.15 ; call lfaecrr(iullfa2,'PALBF',zval,1)
	!
	!-------------------------------------------------
	! PEMISF.
	!-------------------------------------------------
	!
	zval=0.97 ; call lfaecrr(iullfa2,'PEMISF',zval,1)
	!
	!-------------------------------------------------
	! SH.
	!-------------------------------------------------
	!
	zval=-zsh(jpdt)
	call lfaecrr(iullfa2,'SENSIBLE_HEAT_FLUX',zval,1)
	!
	!-------------------------------------------------
	! LH.
	!-------------------------------------------------
	!
	zval=-zlh(jpdt)
	call lfaecrr(iullfa2,'LATENT_HEAT_FLUX',zval,1)
	!
	!-------------------------------------------------
	! Forçage: chauffage radiatif.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'Total radiative heating idealized case (K/day)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=zin(jpdt,jlev)/86400.
	enddo
	call lfaecrr(iullfa2,'RADIATIVE_HEATING',zprof,ilev)
	!
	!-------------------------------------------------
	! Forçage: advection de T.
	!-------------------------------------------------
	!
	lladvec=.true.
	if(lladvec) then
		call lfalecr(iullfa1,'Horizontal_Temp_Advec_(K/hour)',iprod,zin,ilong,ierr)
		do jlev=1,ilev
			zprof(jlev)=zin(jpdt,jlev)/3600.
		enddo
!		call lfalecr(iullfa1,'Vertical_T_Advec(K/hour)',iprod,zin,ilong,ierr)
		call lfalecr(iullfa1,'Vertical_s_Advec(K/hour)',iprod,zin,ilong,ierr)
		do jlev=1,ilev
			zprof(jlev)=zprof(jlev)+zin(jpdt,jlev)/3600.
		enddo
	else
		do jlev=1,ilev
			zprof(jlev)=0.
		enddo
	endif
	call lfaecrr(iullfa2,'DYN-T',zprof,ilev)
	!
	!-------------------------------------------------
	! Forçage: advection de qv.
	!-------------------------------------------------
	!
	if(lladvec) then
		call lfalecr(iullfa1,'Horizontal_r_Advec_(g/kg/hour)',iprod,zin,ilong,ierr)
		do jlev=1,ilev
			zprof(jlev)=zin(jpdt,jlev)/1000./3600.
		enddo
		call lfalecr(iullfa1,'Vertical_r_Advec(g/kg/hour)',iprod,zin,ilong,ierr)
		do jlev=1,ilev
			zprof(jlev)=zprof(jlev)+zin(jpdt,jlev)/1000./3600.
		enddo
	else
		do jlev=1,ilev
			zprof(jlev)=0.
		enddo
	endif
	call lfaecrr(iullfa2,'DYN-Q',zprof,ilev)
	!
	!-------------------------------------------------
	! Pour diagnostic: Q1.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'Q1_(k/hour)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=zin(jpdt,jlev)/3600.
	enddo
	!call lfaecrr(iullfa2,'Q1',zprof,ilev)
	!
	!-------------------------------------------------
	! Pour diagnostic: Q2.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'Q2_(K/hour)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=zin(jpdt,jlev)/3600.
	enddo
	!call lfaecrr(iullfa2,'Q2',zprof,ilev)
	!
	!-------------------------------------------------
	! Pour diagnostic: vitesse verticale omega.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'omega_(mb/hour)',iprod,zin,ilong,ierr)
	do jlev=1,ilev
		zprof(jlev)=zin(jpdt,jlev)*100./3600.
	enddo
	call lfaecrr(iullfa2,'OMEGA',zprof,ilev)
	!
	!-------------------------------------------------
	! Pour diagnostic: nébulosité initiale.
	!-------------------------------------------------
	!
	zprof=0.
	call lfaecrr(iullfa2,'PNEB',zprof,ilev)
	call lfaecrr(iullfa2,'Q1',zprof,ilev)
	call lfaecrr(iullfa2,'Q2',zprof,ilev)

	call lfaecrr(iullfa2,'QVTTOT',zprof,ilev)
	call lfaecrr(iullfa2,'QVTDYN',zprof,ilev)
	call lfaecrr(iullfa2,'QVTSGSTRANSP',zprof,ilev)
	call lfaecrr(iullfa2,'QVTTURB',zprof,ilev)
	call lfaecrr(iullfa2,'QVTSGSTRANSPCONV',zprof,ilev)
	call lfaecrr(iullfa2,'QVTMICROPHYS',zprof,ilev)

	call lfaecrr(iullfa2,'PFPLCL',zprof,ilev1)
	call lfaecrr(iullfa2,'PFPLCN',zprof,ilev1)
	call lfaecrr(iullfa2,'PFPLSL',zprof,ilev1)
	call lfaecrr(iullfa2,'PFPLSN',zprof,ilev1)

	call lfaecrr(iullfa2,'PFHPCL',zprof,ilev1)
	call lfaecrr(iullfa2,'PFHSCL',zprof,ilev1)
	call lfaecrr(iullfa2,'PFHPCN',zprof,ilev1)
	call lfaecrr(iullfa2,'PFHSCN',zprof,ilev1)
	!
	!-------------------------------------------------
	! Indice expérience.
	!-------------------------------------------------
	!
	clnamx='CRM'
	call lfaecrc(iullfa2,'INDICE EXPERIENCE',clnamx,1)
	!
	!-------------------------------------------------
	! Pour diagnostic: vitesse verticale w.
	!-------------------------------------------------
	!
	call lfalecr(iullfa1,'omega_(mb/hour)',iprod,zin,ilong,ierr)
	call lfalecr(iullfa1,'Area Mean Ps(mb)',iprod,zps_surdim,ilong,ierr)
	call lfalecr(iullfa1,'VAH',jplev,za,ilong,ierr)
	call lfalecr(iullfa1,'VBH',jplev,zb,ilong,ierr)
	do jlev=1,ilev
		zomega=zin(jpdt,jlev)*100./3600.
		zpress=0.5*(za(jlev)+za(jlev+1))+0.5*(zb(jlev)+zb(jlev+1))*zps_surdim(jpdt,ilev)*100.
		zrho=zpress/287.05/ztempe(jpdt,jlev)
		zw=-zomega/9.80665/zrho
		zprof(jlev)=zw
	enddo
	call lfaecrr(iullfa2,'W',zprof,ilev)
	!
	!-------------------------------------------------
	! Fermeture du fichier de sortie.
	!-------------------------------------------------
	!
	call lfafer(iullfa2)
enddo
end
