## Identifiants
Dedeoglu Dilara @dedeoglu
N etudiant : 21964171

Maréchal Baptiste @marechal
N etudiant : 21959171


## Fonctionnalité
XpatSolver est un projet qui contient quatre jeu de solitaire dont chacun a des regles et emplacement different. Notre projet crée une permutation des cartes et les place par rapport a la configuration de jeu demandé. Si on donne une liste des coups, le programme traite chacun de ces coups pour vérifier si on peut les jouer. 

La deuxième fonctionnalité du programme est il trouve une solution à la permutation donnée car il arrive à trouver tous les coups possibles. Par contre trouver une solution prends beaucoup de temps malgre tous ce qu'on a fait afin d'ameliorer l'efficacité:
- Changment de Fifo.ml pour améliorer l'éfficacité de l'ajout
- Ajout la condition si il y a une seule carte sur un colonne ne prendre pas en compte de la bouger sur les autres colonnes vides
- Si il existe un état parmi les états parcourus qui possedent un score supérieur au score du nouveau coup alors on ignore le coup


## Compilation et execution
-pour vérifier si la solution donnée est correcte
$ make 
$ ./run -check <fichier solution> <type>.<seed>

-pour vérifier si la permution de jeu est resovable 
$ make 
$ ./run -search <fichier solution> <type>.<seed>


## Découpage modulaire
- `Card.ml` : contient la definitions des cartes et les fonctions necessaires pour convertir le type card à un string. Le type card est définie par un int qui signifie le nombre de cartes et un suit qui signifie l'enseigne d'une carte. 

- `FArray.ml` : les colonnes sont des FArray liste de carte. Donc pour voir si on peut jouer le coup, les nouvelles versions des colonnes apres avoir joué un coup etc. on a utilisé les fonctions dans FArray.

- `Fifo.ml` : on a changé l'implementation de FIFO en changeant type 'a t en etant {o : 'a list; i : 'a list} (o: output list et i : input list).

- `PArray.ml` : non utilisé

- `State.ml` : contient la variable state qui est l'état du jeu apres certains coups. On a utilisé cette classe pour vérifier si les differents coups nous amènent à meme état, si c'est le cas on continue avec une seule liste des coups pour ne pas traité le meme structure plusieurs fois.

- `XpatRandom.ml` : genere une liste aléatoire des cartes/ melanger les cartes.

- `XpatSearch.ml` : cherche une solution et renvoie SUCCES si il trouve une solution et INSOLUBLE si il n'en trouve aucune.

- `XpatSolver.ml` : est utiisé pour distribuer les cartes et verifié si une permutation d'un jeu a une solution.


## Organisation du travail
On a séparé la premiere partie du projet en deux grande taches; une personne a fait la partie shuffle et l'autre s'est occupé de verification de la solution possible. Donc chacun a travaillé separement. Par contre pour la deuxieme partie, on a avancé ensemble. Il n'y a pas eu des taches précis.   