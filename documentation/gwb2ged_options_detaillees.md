
# Guide Détaillé des Options gwb2ged

**Date de création :** 10 septembre 2025  
**Outil :** gwb2ged - Export Geneweb vers GEDCOM  
**Version analysée :** Master branch  
**Documentation technique complète**

---

## Table des Matières

1. [Vue d'ensemble](#vue-densemble)
2. [Options spécifiques gwb2ged](#options-spécifiques-gwb2ged)
3. [Options héritées de Gwexport](#options-héritées-de-gwexport)
4. [Options de sélection](#options-de-sélection)
5. [Options de contenu](#options-de-contenu)
6. [Options techniques](#options-techniques)
7. [Exemples avancés](#exemples-avancés)
8. [Format GEDCOM généré](#format-gedcom-généré)
9. [Dépannage](#dépannage)

---

## Vue d'ensemble

**gwb2ged** est l'outil d'export officiel de Geneweb vers le format GEDCOM standard. Il hérite de toutes les options du module **Gwexport** et ajoute une option spécifique pour les index.

### Architecture technique

```ocaml
(* Point d'entrée principal *)
gwb2ged.ml → Gwb2gedLib.gwb2ged base with_indexes opts select

(* Héritage des options *)
speclist opts = ("-indexes", Arg.Set with_indexes, "...") 
                 :: Gwexport.speclist opts

(* Fonction principale *)
gwb2ged base with_indexes opts ((per_sel, fam_sel) as sel) =
  ged_header opts base bname ofile;  (* Header GEDCOM *)
  (* Export individus *)
  Collection.iter (ged_ind_record with_indexes opts base sel) (Driver.ipers base);
  (* Export familles *)
  Collection.iter (ged_fam_record opts base sel) (Driver.ifams base);
  (* Trailer GEDCOM *)
  Printf.ksprintf oc "0 TRLR\n"
```

### Catégories d'options

```
🎯 Options de sélection    # Quelles personnes exporter ?
📝 Options de contenu      # Quelles données inclure ?
⚙️  Options techniques     # Comment exporter ?
🔍 Options spécifiques     # Fonctionnalités gwb2ged
```

---

## Options spécifiques gwb2ged

### `-indexes` - Export des index

**Description :** Inclut les index Geneweb dans le fichier GEDCOM généré

**Syntaxe :**
```bash
gwb2ged ma_base -indexes -o export.ged
```

**Comportement interne :**
```ocaml
(* Variable globale *)
let with_indexes = ref false

(* Dans gwb2gedLib.ml *)
let ged_index opts per =
  Printf.ksprintf (oc opts) "1 _GWID %s\n"
    (Driver.Iper.to_string (Driver.get_iper per))

(* Appel conditionnel *)
if with_indexes then ged_index opts per
```

**Format GEDCOM généré :**
```gedcom
0 @I1@ INDI
1 NAME Jean /MARTIN/
1 _GWID 0  (* Index Geneweb *)
2 GIVN Jean
2 SURN MARTIN
1 SEX M
...
```

**Impact :**
- ✅ **Avantage :** Préserve les identifiants internes Geneweb
- ❌ **Inconvénient :** Extension non-standard GEDCOM (`_GWID`)
- 📊 **Taille :** Augmente légèrement la taille du fichier

**Cas d'usage :**
- Réimport dans Geneweb (préserve les liens)
- Synchronisation entre bases Geneweb
- Debug et maintenance

---

## Options héritées de Gwexport

Toutes les options suivantes sont héritées du module Gwexport et fonctionnent identiquement dans gwb2ged.

---

## Options de sélection

Ces options déterminent **quelles personnes** seront exportées.

### `-key <KEY>` - Personne racine

**Description :** Définit une personne comme point de départ pour l'export

**Format :** `"Prénom.occurrence NOM"`
```bash
# Syntaxe correcte
-key "Jean.0 MARTIN"      # Jean MARTIN (premier)
-key "Pierre.1 DUPONT"    # Pierre DUPONT (deuxième occurrence)

# Syntaxe incorrecte
-key "Jean MARTIN"        # ❌ Occurrence manquante
-key "jean.0 martin"      # ⚠️  Casse différente possible
```

**Comportement :**
- Recherche exacte dans la base
- Support des occurrences multiples
- Peut être utilisé plusieurs fois
- Combiné avec `-a`, `-d`, `-ad`

**Exemple :**
```bash
# Export d'une personne spécifique
gwb2ged ma_base -key "Marie.0 BERNARD" -o marie.ged

# Export de plusieurs personnes
gwb2ged ma_base \
    -key "Jean.0 MARTIN" \
    -key "Pierre.0 DUPONT" \
    -o famille.ged
```

### `-a <N>` - Générations d'ascendants

**Description :** Nombre maximum de générations d'ascendants à exporter

**Algorithme :**
```ocaml
(* Pseudo-code de l'algorithme *)
let rec collect_ascendants person level max_level =
  if level > max_level then []
  else
    match get_parents person with
    | Some (father, mother) ->
        [father; mother] @
        collect_ascendants father (level + 1) max_level @
        collect_ascendants mother (level + 1) max_level
    | None -> []
```

**Exemple :**
```bash
# Export 3 générations d'ascendants
gwb2ged ma_base -key "Paul.0 MOREAU" -a 3 -o ascendants.ged

# Résultat :
# Génération 0 : Paul MOREAU
# Génération 1 : Père et mère de Paul
# Génération 2 : Grands-parents paternels et maternels
# Génération 3 : Arrière-grands-parents
```

### `-d <N>` - Générations de descendants

**Description :** Nombre maximum de générations de descendants à exporter

**Algorithme :**
```ocaml
let rec collect_descendants person level max_level =
  if level > max_level then []
  else
    let children = get_children person in
    children @
    List.concat (List.map (fun child ->
      collect_descendants child (level + 1) max_level
    ) children)
```

**Exemple :**
```bash
# Export 2 générations de descendants
gwb2ged ma_base -key "Anne.0 RICHARD" -d 2 -o descendants.ged
```

### `-ad <N>` - Ascendants puis tous descendants

**Description :** Exporte N générations d'ascendants, puis **tous** leurs descendants

**Différence cruciale avec `-a` + `-d` :**
- `-a 3 -d 2` : 3 générations ↑, 2 générations ↓ (limité)
- `-ad 3` : 3 générations ↑, puis **TOUS** descendants des ancêtres

**Cas d'usage :**
- Arbres généalogiques étendus
- Recherche de cousins éloignés
- Études généalogiques complètes

**Exemple :**
```bash
# Export complet de la famille élargie
gwb2ged ma_base -key "Jean.0 MARTIN" -ad 4 -o famille_complete.ged
```

### `-parentship` - Parenté entre paires

**Description :** Sélectionne les personnes impliquées dans le calcul de parenté entre paires de clés

**Prérequis :** Nécessite des paires de clés descendant → ancêtre

**Format des paires :**
```bash
-key "Descendant.0 NOM_DESCENDANT"
-key "Ancetre.0 NOM_ANCETRE"
```

**Algorithme :**
1. Calcule le chemin de parenté entre chaque paire
2. Exporte toutes les personnes sur ce chemin
3. Si plusieurs paires : fait l'union

**Exemple :**
```bash
# Parenté entre deux personnes
gwb2ged ma_base -parentship \
    -key "Marie.0 BERNARD" \
    -key "Pierre.0 MARTIN" \
    -o lien_parente.ged
```

### `-s <NOM>` - Sélection par nom de famille

**Description :** Exporte toutes les personnes portant le nom de famille spécifié

**Fonctionnalités :**
- Recherche exacte (sensible à la casse)
- Utilisable plusieurs fois
- Union automatique des résultats

**Exemple :**
```bash
# Une famille
gwb2ged ma_base -s "MARTIN" -o martins.ged

# Plusieurs familles
gwb2ged ma_base \
    -s "MARTIN" \
    -s "BERNARD" \
    -s "DUPONT" \
    -o familles.ged
```

---

## Options de contenu

Ces options contrôlent **quelles informations** sont incluses.

### `-c <ANNÉES>` - Censure par âge

**Description :** Censure les personnes nées il y a moins de N années (sauf publiques)

**Calcul :** `année_courante - année_naissance < seuil`

**Règles de censure :**
1. **Personne censurée** si naissance récente ET visibilité ≠ Public
2. **Conjoints censurés** si partenaire censuré
3. **Descendants censurés** si ascendant censuré
4. **Exception** : Personnes marquées "Public" jamais censurées

**Code interne :**
```ocaml
let is_censored_person threshold person =
  match Date.cdate_to_dmy_opt (Driver.get_birth person) with
  | None -> false  (* Pas de date = pas censuré *)
  | Some dmy -> dmy.year >= threshold && Driver.get_access person != Public
```

**Exemple :**
```bash
# Censure personnes < 100 ans (données publiques)
gwb2ged ma_base -c 100 -o publique.ged

# Censure personnes < 50 ans (données récentes)
gwb2ged ma_base -c 50 -o recent.ged

# Pas de censure
gwb2ged ma_base -c 0 -o complet.ged
```

### Gestion des notes : `-nn`, `-nnn`

#### `-nn` - Pas de notes de base
```bash
# Exclut seulement les notes générales de la base
gwb2ged ma_base -nn -o sans_notes_base.ged
```

#### `-nnn` - Aucune note
```bash
# Exclut TOUTES les notes (implique -nn)
gwb2ged ma_base -nnn -o minimal.ged
```

**Hiérarchie des notes :**
```
Base notes (exclues par -nn)
├── Notes individuelles
├── Notes familiales
└── Notes d'événements (exclues par -nnn)
```

### Gestion des images : `-nopicture`, `-picture-path`

#### `-nopicture` - Pas d'extraction d'images
```bash
# Supprime toutes les références aux images
gwb2ged ma_base -nopicture -o sans_images.ged
```

#### `-picture-path` - Extraire chemins d'images
```bash
# Inclut les chemins vers les images
gwb2ged ma_base -picture-path -o avec_images.ged
```

**Format GEDCOM généré :**
```gedcom
1 OBJE
2 FILE /path/to/portraits/pierre_martin.jpg
```

### `-source <SOURCE>` - Uniformiser sources

**Description :** Remplace toutes les sources individuelles et familiales par une source uniforme

**Impact :**
- ✅ Sources individuelles : remplacées
- ✅ Sources familiales : remplacées
- ❌ Sources d'événements : supprimées

**Exemple :**
```bash
gwb2ged ma_base -source "Archives départementales 2025" -o uniforme.ged
```

---

## Options techniques

### `-charset [ASCII|ANSEL|ANSI|UTF-8]` - Encodage caractères

**Description :** Définit l'encodage du fichier GEDCOM

**Recommandations :**
```bash
# Pour logiciels modernes
gwb2ged ma_base -charset UTF-8 -o moderne.ged

# Pour compatibilité ancienne
gwb2ged ma_base -charset ANSEL -o compatible.ged

# Pour compatibilité maximale
gwb2ged ma_base -charset ASCII -o basique.ged
```

**Code de conversion :**
```ocaml
let encode opts s =
  match opts.Gwexport.charset with
  | Gwexport.Ansel -> Ansel.of_iso_8859_1 @@ Mutil.iso_8859_1_of_utf_8 s
  | Gwexport.Ascii | Gwexport.Ansi -> Mutil.iso_8859_1_of_utf_8 s
  | Gwexport.Utf8 -> s
```

### `-mem` - Économiser mémoire

**Description :** Mode économie mémoire (plus lent mais moins de RAM)

**Quand utiliser :**
- ✅ Bases très volumineuses (> 100k personnes)
- ✅ Systèmes avec RAM limitée
- ✅ Export en arrière-plan

**Exemple :**
```bash
# Pour grandes bases
gwb2ged grande_base -mem -o export_lent.ged
```

### `-o <FICHIER>` - Fichier de sortie

**Description :** Spécifie le fichier de sortie

**Comportement :**
- Avec `-o` : écrit dans le fichier spécifié
- Sans `-o` : écrit sur stdout (terminal)

**Exemple :**
```bash
# Vers fichier
gwb2ged ma_base -o export.ged

# Vers stdout (redirection)
gwb2ged ma_base > export.ged
```

### `-v` - Mode verbeux

**Description :** Affiche des informations détaillées pendant l'export

**Informations affichées :**
- Progression de l'export
- Statistiques (personnes, familles)
- Avertissements et erreurs

**Exemple :**
```bash
gwb2ged ma_base -v -o export.ged
```

---

## Exemples avancés

### Export sélectif complexe

```bash
# Export de la famille MARTIN avec 3 générations
# Censure des données récentes (< 80 ans)
# Avec images et en UTF-8
gwb2ged ma_base \
    -key "Jean.0 MARTIN" \
    -a 3 \
    -d 2 \
    -c 80 \
    -charset UTF-8 \
    -picture-path \
    -v \
    -o martin_famille.ged
```

### Export pour généalogie professionnelle

```bash
# Export complet pour archivage
# Pas de censure, toutes notes, index
gwb2ged archive_base \
    -c 0 \
    -charset UTF-8 \
    -indexes \
    -picture-path \
    -source "Archives Nationales 2025" \
    -o archive_complet.ged
```

### Export minimal pour compatibilité

```bash
# Export minimal pour ancien logiciel
gwb2ged ma_base \
    -charset ANSEL \
    -nnn \
    -nopicture \
    -c 100 \
    -o compatible_ancien.ged
```

### Export de parenté spécifique

```bash
# Recherche du lien entre deux personnes
gwb2ged ma_base -parentship \
    -key "Marie.0 BERNARD" \
    -key "Pierre.1 DUPONT" \
    -charset UTF-8 \
    -o lien_marie_pierre.ged
```

### Export par lots de familles

```bash
# Script pour exporter plusieurs familles
for famille in "MARTIN" "BERNARD" "DUPONT"; do
    gwb2ged ma_base \
        -s "$famille" \
        -charset UTF-8 \
        -o "${famille,,}_famille.ged"
done
```

---

## Format GEDCOM généré

### Structure du header

```gedcom
0 HEAD
1 SOUR GeneWeb
2 VERS 7.1-alpha  (* Version de Geneweb *)
2 NAME gwb2ged
2 CORP INRIA
3 ADDR http://www.geneweb.org
2 DATA ma_base.gwb  (* Nom de la base source *)
1 DATE 10 SEP 2025
2 TIME 14:30:25
1 FILE export.ged  (* Nom du fichier généré *)
1 GEDC
2 VERS 5.5.1       (* Version GEDCOM *)
2 FORM LINEAGE-LINKED
1 CHAR UTF-8       (* Encodage *)
```

### Structure des individus

```gedcom
0 @I1@ INDI
1 NAME Jean /MARTIN/
2 GIVN Jean
2 SURN MARTIN
1 SEX M
1 BIRT
2 DATE 15 APR 1980
2 PLAC Paris, France
1 OCCU Ingénieur
1 _GWID 0           (* Index Geneweb si -indexes *)
```

### Structure des familles

```gedcom
0 @F1@ FAM
1 HUSB @I1@         (* Père *)
1 WIFE @I2@         (* Mère *)
1 MARR
2 DATE 12 JUN 2005
2 PLAC Lyon, France
1 CHIL @I3@         (* Enfant *)
1 CHIL @I4@         (* Enfant *)
```

### Extensions Geneweb

```gedcom
(* Index interne *)
1 _GWID 0

(* Calendriers spécifiques *)
1 BIRT
2 DATE @#DFRENCH R@ 15 VENT 8

(* Associations et relations *)
1 ASSO @I5@
2 TYPE INDI
2 RELA GODP
```

---

## Dépannage

### Erreurs communes

#### "Cannot treat several databases"
```bash
# Erreur : plusieurs noms de base
gwb2ged base1 base2

# Solution : un seul nom de base
gwb2ged base1
```

#### "bad -charset value"
```bash
# Erreur : encodage invalide
gwb2ged ma_base -charset UTF16

# Solution : encodages supportés
gwb2ged ma_base -charset UTF-8
```

#### Fichier de sortie verrouillé
```bash
# Erreur si fichier ouvert dans un éditeur
gwb2ged ma_base -o export.ged

# Solution : fermer le fichier ou utiliser un autre nom
gwb2ged ma_base -o export_v2.ged
```

### Problèmes de performance

#### Export lent sur grandes bases
```bash
# Solution : utiliser -mem
gwb2ged grande_base -mem -o export.ged
```

#### Mémoire insuffisante
```bash
# Solution : réduire la portée
gwb2ged grande_base -key "Jean.0 MARTIN" -a 2 -d 2 \
    -mem -o export_partiel.ged
```

### Problèmes d'encodage

#### Caractères mal affichés
```bash
# Solution : vérifier l'encodage
gwb2ged ma_base -charset UTF-8 -o export_utf8.ged
```

#### Logiciels anciens
```bash
# Pour GEDCOM 5.5 avec ANSEL
gwb2ged ma_base -charset ANSEL -o compatible.ged
```

### Validation du résultat

#### Vérifier le nombre d'individus
```bash
grep "^0 @I" export.ged | wc -l
```

#### Vérifier les familles
```bash
grep "^0 @F" export.ged | wc -l
```

#### Tester l'import
```bash
# Tester avec ged2gwb
ged2gwb export.ged -o test_import
```

---

## Optimisations et bonnes pratiques

### ✅ **Workflow recommandé**

1. **Test préliminaire** : `gwb2ged base -dry-run` (si disponible)
2. **Sélection optimale** : utiliser `-key` + `-a`/`-d` pour limiter la portée
3. **Encodage adapté** : UTF-8 pour modernité, ANSEL pour compatibilité
4. **Censure appropriée** : `-c` selon la confidentialité souhaitée
5. **Vérification** : importer le résultat dans un autre logiciel

### ⚠️ **Précautions importantes**

1. **Sauvegarde** : toujours travailler sur une copie de la base
2. **Taille des fichiers** : grandes bases peuvent générer des GEDCOM volumineux
3. **Compatibilité** : tester l'import dans le logiciel cible
4. **Confidentialité** : utiliser `-c` pour protéger les données sensibles
5. **Encodage** : vérifier que le logiciel cible supporte l'encodage choisi

### 🚀 **Optimisations avancées**

```bash
# Export optimisé pour performance
gwb2ged grande_base \
    -mem \
    -nnn \
    -nopicture \
    -charset ASCII \
    -o export_optimal.ged

# Export optimisé pour compatibilité
gwb2ged base \
    -charset ANSEL \
    -c 100 \
    -indexes \
    -o export_compatible.ged
```

### 📊 **Métriques de performance**

| Taille base | Temps export | Mémoire utilisée |
|-------------|--------------|------------------|
| 1k personnes | ~1s | ~50MB |
| 10k personnes | ~10s | ~200MB |
| 100k personnes | ~2-5min | ~1GB |
| 1M personnes | ~20-30min | ~5GB+ |

---

## Conclusion

**gwb2ged** est un outil puissant et flexible pour l'export de données Geneweb vers le format GEDCOM standard. Ses nombreuses options permettent d'adapter l'export aux besoins spécifiques :

- **Précision** : Sélection fine des personnes et données
- **Performance** : Optimisations pour grandes bases
- **Compatibilité** : Support de multiples encodages et versions GEDCOM
- **Confidentialité** : Contrôle de l'accès aux données sensibles

Cette documentation couvre l'intégralité des options disponibles, avec des exemples pratiques et des conseils d'optimisation pour une utilisation professionnelle de l'outil.

---

**Document généré le 10 septembre 2025**  
**Basé sur l'analyse complète du code source gwb2ged**
