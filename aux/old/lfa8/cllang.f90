function cllang()
! --------------------------------------------------------------
! **** *CLLANG* Returns current language choice, depending on LANG environment variable.
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
! cllang='FRA', 'ENG', ...
! --------------------------------------------------------------
implicit none
character*200 clvar
character*3 cllang
!
! -------------------------------------------------
! Get $HOME current value.
! -------------------------------------------------
!
call getenv('LANG',clvar)
if(clvar(1:2) == 'fr') then
	cllang='FRA'
else
	cllang='ENG'
endif
end
