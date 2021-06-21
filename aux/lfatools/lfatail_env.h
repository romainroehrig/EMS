	!
	!-------------------------------------------------
	! Taille en octets des entiers et reels
	! que l'utilisateur a l'intention de passer
	! en argument au logiciel LFA.
	!-------------------------------------------------
	!
	integer, parameter :: jpreeusr=XXIPRECRXX ! reels
	integer, parameter :: jpintusr=XXIPRECIXX ! entiers
	!
	!-------------------------------------------------
	! Taille en octets des entiers et reels
	! que l'utilisateur a l'intention d'ecrire par defaut
	! sur les fichiers.
	! Cette taille peut etre modifiee
	! lors de l'execution, par appel a lfaprec.
	!-------------------------------------------------
	!
	integer, parameter :: jpreedef=XXIPRECRXX ! reels
	integer, parameter :: jpintdef=XXIPRECIXX ! entiers
	!
	!-------------------------------------------------
	! Taille en octets des entiers a usage interne LFA,
	! et qui renseignent dans le fichier
	! la version du logiciel, la taille des articles, etc...
	! Afin que les fichiers soient relisibles 
	! entre les differentes versions du logiciel LFA
	! et les differentes precisions possibles,
	! NE PAS CHANGER la valeur 4 ci-dessous.
	!-------------------------------------------------
	!
	integer, parameter :: jpintesb=4 ! entiers
