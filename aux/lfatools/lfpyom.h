	integer, parameter :: jperf=100 ! nombre maximal de fichiers ouvrables.
	integer, parameter :: jpdebut=20 ! début de la table ascii utile pour la compression.
	integer, parameter :: jpbase=256-jpdebut ! longueur de la table ascii utile pour la compression.
	common/lfpcom/lgerf,nprec,cgnomf,ntypo,nmes,cgmetc
	!                                                                         
	! lgerf(kul): .true. si toute erreur sur le fichier kul                   
	! doit etre fatale.                                                       
	!                                                                         
	logical lgerf(jperf)
	!                                                                         
	! ntypo(kul): type d'ouverture du fichier: 1 READ, 2 WRITE, 3 READ-WRITE, 4 scratch.
	!                                                                         
	integer(kind=jpintusr) ntypo(jperf)
	!                                                                         
	! nprec(kul): nombre de chiffes significatifs lors de l'ecriture          
	! de reels sur le fichier d'unite logique kul.                            
	!                                                                         
	integer(kind=jpintusr) nprec(jperf)
	!                                                                         
	! nmes(kul): niveau de messagerie associe au fichier                      
	! d'unite logique kul:
	! 0 rien sauf erreurs fatales.
	! 1 erreurs fatales + messages d'attention.
	! 2 bavard (mode utile en développement du logiciel uniquement).
	!                                                                         
	integer(kind=jpintusr) nmes(jperf)
	!                                                                         
	! cgnomf(kul): nom en clair du fichier d'unite logique kul.               
	!                                                                         
	character*80 cgnomf(jperf)
	!                                                                         
	! cgmetc: choix de la méthode de compression des réels:
	! 'AUT' si on veut que le logiciel choisisse lui-même la méthode
	! en fonction de la nature du champ.
	! 'MIN', 'LFP', 'PAL', etc...: voir routine lfpecrr pour explications.
	!                                                                         
	character*3 cgmetc(jperf)
