program demo
! --------------------------------------------------------------------------
! **** *demo* Programme de demonstration du logiciel LFP.
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
parameter(jpl=100)
parameter(jpliste=88)
implicit logical (l)
implicit character*200 (c)
character*15 clt(jpl),cls(jpl)
integer it(jpl),is(jpl)
real zt(jpl),zs(jpl)
logical llt(jpl),lls(jpl)
character*200 clliste(jpliste)
!
! Initialisation des tableaux a ecrire sur les fichiers.
!
do jl=1,jpl
	write(clt(jl),fmt='(I3,a,i5)') jl,' RR ',jl*jl
	it(jl)=jl*jl*jl
	zt(jl)=float(jl)+float(jl)/100.
	zs(jl)=float((2*jl)/jpl)
	zgol=zt(jl)-int(zt(jl))
	llt(jl)=mod(jl,3) /= 0
enddo
!
! Ouverture du fichier.
!
iul=7
call lfpouv(iul,'LFP',2)
!
! Niveau de messagerie: ici le niveau bavard.
!
call lfpmes(iul,2)
!
! Méthode de compression.
!
!call lfpmetc(iul,'LIG')
!call lfpmetc(iul,'AUT')
!
! Ecriture de reels.
!
clna='Des reels'
call lfpecrr(iul,clna,zt,jpl)
clna='Des reels paliers'
call lfpecrr(iul,clna,zs,jpl)
!
! Ecriture d'entiers.
!
clna='Tab.entier'
call lfpecri(iul,clna,it,jpl)
!
! Ecriture de caracteres.
!
clna='Tableau\de\car'
call lfpecrc(iul,clna,clt,jpl)
!
! Ecriture de logiques.
!
clna='LOGIQUES'
call lfpecrl(iul,clna,llt,jpl)
!
! Liste des articles du fichier sur un tableau.
!
call lfplaft(iul,clliste,jpliste,ilnoma)
do jl=1,ilnoma
	illoc=len_trim(clliste(jl))
	print*,'Nom ',jl,' = ',clliste(jl)(1:illoc)
enddo
!
! Liste des articles du fichier sur output standard.
!
call lfplaf(iul,6)
!
! Fermeture du fichier.
!
call lfpfer(iul)
!
! Mise à "zéro" des tableaux a lire sur les fichiers.
!
do jl=1,jpl
	cls(jl)=' '
	is(jl)=0
	zs(jl)=0.
	lls=.false.
enddo
!
! Ouverture du fichier.
!
call lfpouv(iul,'LFP',1)
!
! Test d'existence d'un article.
!
clna='Tab.entier'
call lfpcas(iul,clna,cltype,ilong,irep)
if(irep == 0) then
	print*,'L''article ',clna,' existe.'
else
	print*,'L''article ',clna,' n''existe pas.'
endif
clna='SCRONTCH!...'
call lfpcas(iul,clna,cltype,ilong,irep)
if(irep == 0) then
	print*,'L''article ',clna,' existe.'
else
	print*,'L''article ',clna,' n''existe pas.'
endif
!
! Lecture d'entiers.
!
clna='Tab.entier'
call lfpleci(iul,clna,jpl,is,ilres,ierr)
!
! Lecture de reels.
!
clna='Des reels'
call lfplecr(iul,clna,jpl,zs,ilres,ierr)
!
! Lecture de reels.
!
clna='A ke gol'
call lfperf(iul,.false.)
call lfplecr(iul,clna,jpl,zs,ilres,ierr)
!
! Lecture de caracteres.
!
clna='Tableau\de\car'
call lfplecc(iul,clna,jpl,cls,ilres,ierr)
!
! Lecture de logiques.
!
clna='LOGIQUES'
call lfplecl(iul,clna,jpl,lls,ilres,ierr)
!
! Copie d'un article du fichier iul sur le fichier iul2.
!
iul2=78
call lfpouv(iul2,'LFPcop',2)
call lfpcop(iul,'LOGIQUES',' ',iul2)
call lfpfer(iul2)
!
! Impression sur output standard des extrêmes
! de tous les articles.
!
call lfpnx(iul)
!
! Fermeture du fichier.
!
call lfpfer(iul)
end
#include"lfp.f90"
