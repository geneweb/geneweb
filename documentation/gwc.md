# GeneWeb — Commande gwc

## Synopsis

```
gwc [options] [fichiers]
```

- Fichiers d’entrée:
  - `.gw`: fichiers source GeneWeb (texte)
  - `.gwo`: fichiers objets (générés par `gwc -c` ou déjà fournis)
- Sortie: création d’une base GeneWeb (répertoire) nommée via `-o <nom>` dans le répertoire bases `-bd <DIR>`.

## Description

`gwc` est l’outil en ligne de commande qui:
- compile des fichiers `.gw` en `.gwo` (phase « compilation », gérée par `gwcomp`),
- puis lie des `.gwo` pour construire une base GeneWeb (phase « link », gérée par `db1link`).

Le nom de la base se déduit de `-o <nom>` (obligatoire si plusieurs fichiers). Si `-o` est omis avec un seul fichier, le nom par défaut est le nom d’entrée sans extension; sinon `a`.

Seuls les caractères `a..z`, `A..Z`, `0..9`, `-` sont autorisés pour le nom de base.

Par défaut, les particules (mots comme « de », « du », etc.) proviennent du fichier `etc/particles.txt`, sauf si `-particles` est utilisé.

## Options

- `-bd <DIR>`: répertoire des bases. Si absent, `.` est utilisé.
- `-bnotes [drop|erase|first|merge]`: stratégie pour les notes de base du prochain fichier:
  - `drop`: ignorer ce contenu.
  - `erase`: écraser le contenu courant.
  - `first`: garder la première version non vide, sinon ignorer.
  - `merge`: concaténer (valeur par défaut).
- `-c`: ne faire que compiler les `.gw` en `.gwo` (ne crée pas la base).
- `-cg`: calculer la consanguinité après la création de la base.
- `-ds <str>`: définir la source par défaut pour personnes/familles sans source.
- `-f`: forcer la création (supprime la base si elle existe déjà).
- `-gwo`: supprimer les `.gwo` après création réussie de la base.
- `-mem`: réduire l’usage mémoire (plus lent).
- `-nc`: désactiver la vérification de cohérence de la base.
- `-nofail`: en cas d’erreur de syntaxe `.gw`, ne pas interrompre; signaler et continuer.
- `-nolock`: ne pas verrouiller la base pendant la création.
- `-nopicture`: ne pas conserver les chemins d’images (`#image`, `#photo`) depuis les `.gw`.
- `-o <file>`: nom de la base de sortie (obligatoire si plusieurs fichiers en entrée).
- `-particles <file>`: fichier des particules à utiliser.
- `-q`: mode silencieux.
- `-reorg`: mode « reorg » (affecte les chemins de configuration et le RGPD).
- `-rgpd <dir>`: activer RGPD si `<dir>` existe; modifie l’accès (Public / SemiPublic / Private) selon la présence de fichiers `fn.occ.sn.pdf` dans `<dir>`.
- `-sep`: séparer les personnes du prochain fichier (occurrences locales non fusionnées globalement).
- `-sh <int>`: décaler les numéros d’occurrence des personnes du prochain fichier.
- `-stats`: afficher les statistiques de la base après création.
- `-v`: mode verbeux (inclut progression si beaucoup de `.gwo`).

## Entrées et blocs pris en charge (dans `.gw`)

- Blocs familiaux `fam` (parents, relation, mariage/divorce, témoins, enfants…)
- Blocs d’événements de personne `pevt` (naissance, baptême, décès, inhumation/crémation, autres événements)
- Blocs de relations `rel` (parents nourriciers, adoption, etc.)
- Blocs de notes `notes`, `notes-db`, `page-ext`
- Blocs de notes « wizard » `wizard-note`
- Directives `encoding: utf-8` et `gwplus` (force la création de clés même incomplètes)

## Comportement détaillé

- Compilation (`gwcomp`):
  - Parse les fichiers `.gw`, signale les erreurs (ou continue avec `-nofail`).
  - Gère l’encodage (`encoding:utf-8`) et les extensions `gwplus`.
  - Produit des `.gwo` avec en-tête `GnWo000o`.
- Linkage (`db1link`):
  - Résout et fusionne personnes/familles/événements, construit les tableaux internes et les « unique strings ».
  - Infère des champs standards à partir des événements (et inversement) lorsque pertinent.
  - Applique la politique des notes de base (`-bnotes`) et crée éventuellement les pages étendues.
  - Vérifie la cohérence (sauf `-nc`), calcule la consanguinité (`-cg`), affiche des statistiques (`-stats`).
  - Crée `wiznotes/` (notes wizard) et `command.txt` dans la base.

## Résultat attendu

- En l’absence de `-c`, création d’une base GeneWeb dans `-bd/<nom>`:
  - Données synchronisées, nettoyage du répertoire temporaire interne.
  - Si `-gwo` est présent: suppression des `.gwo` intermédiaires après succès.
- Avec `-c`, seuls les `.gwo` sont produits (pas de base).

## Exemples

- Créer une base à partir de deux sources:
  - `gwc -bd bases -o ma_base src1.gw src2.gw`
- Compiler seulement:
  - `gwc -c src.gw`
- Lier un `.gwo` existant avec fusion des notes et décalage des occurrences:
  - `gwc -o ma_base -bnotes merge -sh 100 src.gwo`

## Codes de sortie

- 0: succès et base créée (sauf option `-c`).
- 2: erreur bloquante (nom de base invalide, verrouillage, etc.).

## Performances et mémoire

- Utiliser `-mem` pour réduire la consommation mémoire (au prix de performances).
- `-v` affiche les progrès; avec de nombreux `.gwo`, une barre de progression est visible.

## Bonnes pratiques

- Spécifier `-o` dès qu’il y a plusieurs fichiers en entrée.
- Vérifier le nommage (`a..z`, `A..Z`, `0..9`, `-`).
- Regrouper les options `-bnotes`, `-sep`, `-sh` par fichier pour un contrôle fin de l’assemblage.
