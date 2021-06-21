program lec
! --------------------------------------------------------------
! **** *LEC* G�n�ration de la documentation lfa pour latex � partir du source.
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
implicit character*200 (c)
implicit logical (l)
clbarre= &
& '	! -------------------------------------------------------------'
!
! -------------------------------------------------
! Source d'entr�e.
! -------------------------------------------------
!
iultex=22
open(iultex,file='lfa.F',form='formatted')
!
! -------------------------------------------------
! Documentation de sortie.
! -------------------------------------------------
!
iulfra=77
open(iulfra,file='lfa_interface_ftn.tmp.tex',form='formatted')
ix=0
llintzi=.false. ! vrai si on est � l'int�rieur d'une zone � imprimer.
!
! -------------------------------------------------
! Lecture s�quentielle du source.
! -------------------------------------------------
!
  100 read(iultex,fmt='(a)',end=200) clc
ilc=len_trim(clc)
ix=ix+1
if(clc(1:11) == '	subroutine') then
	!
	! -------------------------------------------------
	! D�but de subroutine � renseigner.
	! -------------------------------------------------
	!
	write(iulfra,'(a)') ' '
	write(iulfra,'(a)') ' '
	write(iulfra,'(a)') ' '
	write(iulfra,'(a)') '\begin{verbatim}'
	write(iulfra,'(a)') clc(1:ilc)
elseif(clc(1:8) == '	! -----'.and.ilc == 48) then
	write(iulfra,'(a)') clc(1:ilc)
elseif(clc(1:7) == '	! ****') then
	write(iulfra,'(a)') clbarre
	write(iulfra,'(a)') clc(1:ilc)
elseif(clc(1:9) == '	! En ent') then
	!
	! -------------------------------------------------
	! D�but de la zone d�crivant les arguments en entr�e/sortie.
	! -------------------------------------------------
	!
	write(iulfra,'(a)') clbarre
	write(iulfra,'(a)') clc(1:ilc)
  400 	read(iultex,fmt='(a)',end=200) clc
	ix=ix+1
	ilc=len_trim(clc)
	if(clc(1:2) == '	!') then
		!
		! -------------------------------------------------
		! On est toujours dans la zone d�crivant les arguments en entr�e/sortie.
		! -------------------------------------------------
		!
		write(iulfra,'(a)') clc(1:ilc)
		goto 400
	endif
	!
	! -------------------------------------------------
	! Si on est ici, c'est qu'on a atteint la fin
	! de la zone d�crivant les arguments en entr�e/sortie.
	! -------------------------------------------------
	!
	write(iulfra,'(a)') '\end{verbatim}'
else
	!
	!-------------------------------------------------
	! Rien � faire!...
	!-------------------------------------------------
	!
endif
goto 100
  200 continue
print*,ix,' articles lus.'
close(iultex)
end
