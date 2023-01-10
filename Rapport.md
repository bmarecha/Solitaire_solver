## Identifiants

Dedeoglu Dilara @dedeoglu
N etudiant : 21964171

Maréchal Baptiste @marechal
N etudiant : 21959171

## Fonctionnalité

Aucune pour l'instant

## Compilation et execution

$ make 
$ ./run

## Découpage modulaire
- `Card.ml` : contient la definitions des cartes et les fonctions necessaires pour convertir le type card à un string. Le type card est définie par un int qui signifie le nombre de cartes et un suit qui signifie l'enseigne d'une carte. 

- `FArray.ml` : les colonnes sont des FArray liste de carte. Donc pour voir si on peut jouer le coup, les nouvelles versions des colonnes apres avoir joué un coup etc. on a utilisé les fonctions dans FArray.

- `Fifo.ml` : on a changé l'implementation de FIFO en changeant type 'a t en etant {o : 'a list; i : 'a list} (o: output list et i : input list). 

- `PArray.ml` : 

- `State.ml` : contient la variable state qui est l'état du jeu apres certains coups. On a utilisé cette classe pour vérifier si les differents coups nous amènent à meme état, si c'est le cas on continue avec une seule liste des coups pour ne pas vérifier si le meme structure est resolvable. 

- `XpatRandom.ml` : genere une liste aléatoire des cartes/ melanger les cartes.

- `XpatSearch.ml` : 

- `XpatSolver.ml` :


## Organisation du travail
On a séparé la premiere partie du projet en deux grande taches; une personne a fait la partie shuffle et l'autre s'est occupé de verification de la solution possible. Donc chacun a travaillé separement. Par contre pour la deuxieme partie, on a avancé ensemble. Il n'y a pas eu des taches précis.   

Rapport.txt
Un fichier texte Rapport.txt (ou Rapport.md) à la racine de votre dépôt devra nous
fournir toutes les informations spécifiées ci-dessous, clairement
séparées en sections distinctes. Ce rapport n'est pas une simple
formalité : il sera pris en compte dans l'évaluation.


(Identifiants)
Commencez par mentionner, pour chacun des membres du groupe :
nom, prénom, identifiant sur GitLab, numéro d'étudiant.


(Fonctionnalités)
Donnez une description précise des fonctionnalités implémentées
par votre rendu - sujet minimal, extensions éventuelles,
éventuellement parties non réalisées ou non encore fonctionnelles.


(Compilation et exécution)
Documentez ensuite de façon précise la manière dont votre
projet doit être compilé (normalement via dune) et exécuté (en donnant
les options acceptées par votre programme). Pour ce projet, aucune
bibliothèque externe n'est autorisée a priori. Nous contacter si
cela vous semble problématique.


(Découpage modulaire)
Donnez une description des traitements pris en charge par chaque
module (.ml) de votre projet. Précisez le rôle et la nécessité
de chaque module ajouté au dépôt initial.


(Organisation du travail)
Cette partie est plus libre dans sa forme. Indiquez la manière
dont les tâches ont été réparties entre les membres du groupe
au cours du temps. Donnez une brève chronologie de votre travail
sur ce projet au cours de ce semestre, avant et après le
confinement.


(Misc)
Cette partie est entièrement libre : remarques, suggestions,
questions...