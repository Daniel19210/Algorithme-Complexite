# Algorithmique-Complexite
Répertoire pour le projet de M1 du cours d'Algorithmique et Complexité

Le projet est divisé en deux dossier:
images : contient toutes les images sur lesquelles nous avons fait des tests
compression : Le dossier contenant un projet dune dans lequel on exécute notre code

Il est possible de lancer le code de 2 façon différentes.
Soit on utilise le makefile dans le dossier compression (qui fait appel à dune pour lancer le projet) avec la commande ```make```
Soit on utilise dune et on lance ```dune exec compression```

Il est possible de changer l'image cible de la compression en altérant le fichier bin/main.ml en changeant la variable 'image'
Le taux de compression est aussi défini dans bin/main.ml

Le code donne par défaut des information sur les matrices sur lesquelles on effectue la SVD.
Pour pouvoir enlever ceci, il est possible de modifier l'argument true à false lors de l'appel de la fonction 'Compress.make_compression'

Décommenter la ligne 92 et commenter la ligne 93 permet d'avoir le résultat figure 2.
Pour avoir le résultat intermédiaire tel que montré dans le rapport à la figure 3, il suffit de mettre en commentaire les lignes 10 à 24 et décommenter les lignes 28 à 31.
Il faut bien évidemment choisir les images correspondantes.
