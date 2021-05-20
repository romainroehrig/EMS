	!
	! -------------------------------------------------
	! Parametres.
	! -------------------------------------------------
	!
	integer jpnoma
	parameter(jpnoma=80) ! nombre maximal de caractères des noms d'article.
	integer jperf
	parameter(jperf=100) ! nombre maximal de fichiers ouvrables.
	!
	! -------------------------------------------------
	! Tableaux globaux renseignant la position
	! du pointeur dans le fichier:
	! lgpoint est
	! .    - faux si le pointeur est avant une autodocumentation
	! .    - vrai s'il est avant un article de donnees.
	! Dans le cas ou lgpoint est vrai:
	! .    - ntype est le type de l'article.
	! .    - nlong est sa longueur.
	! .    - cgna est son nom.
	! Dans le cas ou lgpoint est faux ces 3 tableaux
	! .    ne sont pas utilises.
	! -------------------------------------------------
	!
	!
	! -------------------------------------------------
	! Logiques.
	! -------------------------------------------------
	!
	!
	! lgerf(kul): .true. si toute erreur sur le fichier kul
	! doit etre fatale.
	logical lgerf(jperf)
	logical lgpoint(jperf)
	logical lglang ! .true. if french, .false. if english.
	common/lfacoml/lgerf,lgpoint,lglang
	!
	!-------------------------------------------------
	! Entiers.
	!-------------------------------------------------
	!
	!
	! nmes(kul): niveau de messagerie associe au fichier
	! d'unite logique kul:
	! 0 rien sauf erreurs fatales.
	! 1 erreurs fatales + messages d'attention.
	! 2 bavard (mode utile en développement du logiciel uniquement).
	!
	integer nmes(jperf)
	!
	! nversion: version du logiciel qui a produit le fichier lu.
	!
	integer nversion(jperf)
	integer ntype(jperf)
	integer nlong(jperf)
	!
	! Precision des reels et entiers en octets.
	!
	integer nprecr(jperf)
	integer npreci(jperf)
	common/lfacomi/nmes,ntype,nlong,nversion,nprecr,npreci
	!
	!-------------------------------------------------
	! Caracteres.
	!-------------------------------------------------
	!
	! cgnomf(kul): nom en clair du fichier d'unite logique kul.
	character*80 cgnomf(jperf)
	!
	! cgtypo(kul): type d'ouverture du fichier: 'R' READ, 'W' WRITE, 'S' scratch.
	character*1 cgtypo(jperf)
	character*(jpnoma) cgna(jperf)
	common/lfacomc/cgnomf,cgtypo,cgna
