# Guide Complet des Commandes Geneweb

**Date de création :** 10 septembre 2025  
**Version analysée :** Master branch  
**Description :** Documentation complète de toutes les commandes et outils Geneweb

---

## Table des Matières

1. [Vue d'ensemble des outils](#vue-densemble-des-outils)
2. [Serveur Web - gwd](#serveur-web---gwd)
3. [Compilateur - gwc](#compilateur---gwc)
4. [Export/Import](#exportimport)
5. [Maintenance et réparation](#maintenance-et-réparation)
6. [Outils d'analyse](#outils-danalyse)
7. [Utilitaires système](#utilitaires-système)
8. [Exemples d'utilisation](#exemples-dutilisation)

---

## Vue d'ensemble des outils

Geneweb fournit un ensemble complet d'outils en ligne de commande pour gérer les bases de données généalogiques :

### Classification des outils

```
📂 Outils principaux
├── gwd          # Serveur web principal
├── gwc          # Compilateur de bases .gw → .gwb
├── gwu          # Export base → .gw
└── gwsetup      # Interface de configuration

📂 Import/Export
├── ged2gwb      # Import GEDCOM → Geneweb
└── gwb2ged      # Export Geneweb → GEDCOM

📂 Maintenance
├── gwfixbase    # Réparation et optimisation
├── consang      # Calcul de consanguinité
└── connex       # Analyse de connexité

📂 Utilitaires
├── gwdiff       # Comparaison de bases
├── cache_files  # Génération de caches
└── update_nldb  # Mise à jour NLDB
```

---

## Serveur Web - gwd

**Fonction :** Serveur web principal de Geneweb pour interface utilisateur

### Syntaxe
```bash
gwd [options]
```

### Options principales

#### Configuration réseau
```bash
-a <ADDRESS>                # Adresse IP spécifique (défaut: toutes)
-p <PORT>                   # Port d'écoute (défaut: 2317)
-daemon                     # Mode daemon Unix
-cgi                        # Mode CGI forcé
```

#### Répertoires et fichiers
```bash
-bd <DIR>                   # Répertoire des bases (défaut: "bases")
-hd <DIR>                   # Répertoire etc/images/lang (défaut: "gw")
-etc_prefix <DIR>           # Répertoire etc spécifique
-images_prefix <DIR>        # Répertoire images spécifique
-images_dir <DIR>           # Nom relatif du répertoire images
```

#### Sécurité et authentification
```bash
-auth <FILE>                # Fichier d'autorisation "user:password"
-digest                     # Schéma d'autorisation Digest (plus sécurisé)
-friend <PASSWD>            # Mot de passe "ami"
-cgi_secret_salt <STRING>   # Sel secret pour les digests de formulaire
```

#### Performance et gestion
```bash
-n_workers <NUM>            # Nombre de workers (défaut: 20)
-max_pending_requests <NUM> # Requêtes en attente max (défaut: 150)
-conn_tmout <SEC>           # Timeout connexion (défaut: 120s)
-login_tmout <SEC>          # Timeout login CGI (défaut: 1800s)
-cache-in-memory <DATABASE> # Précharger base en mémoire
```

#### Langue et localisation
```bash
-lang <LANG>                # Langue par défaut (défaut: fr)
-blang                      # Détecter langue navigateur
-add_lexicon <FILE>         # Ajouter fichier lexique
-cache_langs                # Langues lexique à mettre en cache
```

#### Debug et logging
```bash
-debug                      # Mode debug activé
-log <FILE>                 # Fichier de log ("-" pour stdout)
-log_level <N>              # Niveau syslog (défaut: 6)
-min_disp_req               # Nb min requêtes trace robot (défaut: 6)
```

#### Options avancées
```bash
-allowed_tags <FILE>        # Tags HTML autorisés
-no-images <HOST>           # Pas d'images pour cet hôte
-only <FILE>                # Seules adresses autorisées
-redirected_addr <URL>      # Redirection d'adresse
-robot_xcl <FILE>           # Exclusion robots
-setup_link                 # Lien vers page setup
-trace_failed_passwd        # Tracer échecs mot de passe
-wizard_passwd <PASSWD>     # Mot de passe sorcier
```

### Exemples d'utilisation
```bash
# Démarrage basique
gwd

# Serveur sur port spécifique avec authentification
gwd -p 8080 -auth users.txt

# Mode daemon avec log
gwd -daemon -log /var/log/geneweb.log

# Configuration complète
gwd -bd /var/geneweb/bases -hd /usr/share/geneweb \
    -auth auth.txt -friend secret123 -lang en -p 2317
```

---

## Compilateur - gwc

**Fonction :** Compile les fichiers sources .gw en bases de données .gwb

### Syntaxe
```bash
gwc [options] [files]
```

### Types de fichiers
- **Fichiers source :** Extension `.gw`
- **Fichiers objet :** Extension `.gwo`
- **Base de données :** Extension `.gwb`

### Options principales

#### Compilation et sortie
```bash
-o <file>                   # Base de sortie (défaut: <input>.gwb)
-c                          # Compilation seulement (pas de base)
-gwo                        # Supprimer .gwo après création base
-f                          # Forcer (supprimer base existante)
```

#### Répertoires
```bash
-bd <DIR>                   # Répertoire des bases (défaut: ".")
```

#### Traitement des données
```bash
-cg                         # Calculer consanguinité
-ds <str>                   # Source par défaut pour personnes/familles
-nc                         # Pas de vérification cohérence
-nofail                     # Pas d'échec en cas d'erreur
-nopicture                  # Pas de photos associatives
```

#### Gestion mémoire
```bash
-mem                        # Économiser mémoire (plus lent)
-nolock                     # Pas de verrouillage base
```

#### Notes de base
```bash
-bnotes [drop|erase|first|merge]
                           # Comportement notes de base
                           # drop: supprimées
                           # erase: effacer contenu actuel
                           # first: supprimer si contenu non vide
                           # merge: concaténer (défaut)
```

### Exemples d'utilisation
```bash
# Compilation simple
gwc famille.gw

# Compilation avec nom de base spécifique
gwc -o ma_famille famille.gw

# Compilation avec calcul de consanguinité
gwc -cg -o famille_complete famille.gw

# Compilation multiple avec gestion mémoire
gwc -mem -f famille1.gw famille2.gw -o base_complete

# Compilation avec source par défaut
gwc -ds "État civil de Paris" -o paris famille_paris.gw
```

---

## Export/Import

### gwu - Export vers format .gw

**Fonction :** Exporte une base .gwb vers format texte .gw

#### Syntaxe
```bash
gwu <BASE> [OPTIONS]
```

#### Options de sélection
```bash
-key <KEY>                  # Personne racine (format: "Prénom.occ NOM")
-a <N>                      # Générations d'ascendants max
-d <N>                      # Générations de descendants max  
-ad <N>                     # Générations ascendants-descendants max
-isolated                   # Exporter personnes isolées
-parentship                 # Sélection par parenté entre paires de clés
```

#### Options de contenu
```bash
-c <NUM>                    # Censure si né < NUM années (sauf Public)
-nn                         # Pas de notes base
-nnn                        # Pas de notes du tout
-nopicture                  # Pas d'extraction photos
-picture-path               # Extraire chemins photos
-all_files                  # Tout le contenu notes_d dans .gw
```

#### Options techniques  
```bash
-o <FILE>                   # Fichier sortie (défaut: stdout)
-odir <dir>                 # Créer fichiers par nom original
-charset [ASCII|ANSEL|ANSI|UTF-8]  # Jeu caractères
-mem                        # Économiser mémoire
-raw                        # Sortie brute (pas conversion UTF-8)
-old_gw                     # Pas de champs additionnels (compatibilité < 7.00)
```

#### Exemples
```bash
# Export complet
gwu ma_base -o export_complet.gw

# Export ascendants/descendants d'une personne
gwu ma_base -key "Pierre.0 MARTIN" -a 5 -d 3 -o martin_famille.gw

# Export avec censure (personnes < 100 ans)
gwu ma_base -c 100 -o export_public.gw

# Export parenté entre deux personnes
gwu ma_base -key "Jean.0 DUPONT" -key "Marie.0 DURAND" -parentship -o parente.gw
```

### gwb2ged - Export vers GEDCOM

**Fonction :** Exporte une base .gwb vers format GEDCOM standard

#### Syntaxe
```bash
gwb2ged <BASE> [OPTIONS]
```

#### Options (identiques à gwu plus)
```bash
-indexes                    # Exporter index dans GEDCOM
-s <SN>                     # Sélectionner ce nom (utilisable plusieurs fois)
-source <SRC>               # Remplacer sources individus/familles
-v                          # Mode verbeux
```

#### Exemples
```bash
# Export GEDCOM complet
gwb2ged ma_base -o export.ged

# Export avec index
gwb2ged ma_base -indexes -o export_index.ged

# Export famille spécifique
gwb2ged ma_base -s "MARTIN" -s "BERNARD" -o martin_bernard.ged
```

### ged2gwb - Import depuis GEDCOM

**Fonction :** Importe un fichier GEDCOM vers base Geneweb

#### Syntaxe (script Python détecté)
```bash
ged2gwb [-h] [-o OUTPUT] [-f FILE] [--options] [input]
```

#### Options détectées
```bash
-o OUTPUT, --output OUTPUT  # Base Geneweb de sortie
-f FILE, --file FILE        # Fichier GEDCOM (méthode alternative)
-fne, --first-names-not-extracted  # Pas extraction prénoms
--lower-first-names         # Prénoms en minuscules
--charset {ansel,ansi,ascii,msdos,macintosh,utf8}  # Encodage
--alive ALIVE               # Années pour être vivant
--dead DEAD                 # Années pour être mort
--no-check                  # Pas de vérification base
```

---

## Maintenance et réparation

### gwfixbase - Réparation et optimisation

**Fonction :** Répare, optimise et reconstruit les index d'une base

#### Syntaxe
```bash
gwfixbase [OPTION] base
```

#### Options principales
```bash
-dry-run                    # Ne pas appliquer (seulement afficher)
-index                      # Reconstruire index (activé par autres options)
-fast                       # Mode rapide (plus de mémoire)
-o                          # Dump personnes dans fichier
```

#### Vérifications et réparations
```bash
-dump                       # Lister personnes
-families-children          # Enfants des familles
-families-parents           # Parents des familles  
-fevents-witnesses          # Témoins événements familiaux
-invalid-utf8               # UTF-8 invalide
-marriage-divorce           # Mariages/divorces
-person-key                 # Clés personnes
-persons-NBDS               # Personnes NBDS
-persons-families           # Familles des personnes
-persons-parents            # Parents des personnes
-pevents-witnesses          # Témoins événements personnels
```

#### Mode silencieux
```bash
-q                          # Mode silencieux
-qq                         # Mode très silencieux
```

#### Exemples
```bash
# Reconstruction index simple
gwfixbase ma_base

# Vérification sans modification
gwfixbase -dry-run ma_base

# Réparation complète avec toutes vérifications
gwfixbase -index -families-children -persons-families ma_base

# Mode rapide pour grandes bases
gwfixbase -fast -index ma_base
```

### consang - Calcul de consanguinité

**Fonction :** Calcule les coefficients de consanguinité dans la base

#### Syntaxe
```bash
consang [options] <base>
```

#### Options
```bash
-fast                       # Plus rapide (plus de mémoire)
-mem                        # Économiser mémoire (plus lent réécriture)
-nolock                     # Pas de verrouillage base
-scratch                    # Recalcul depuis zéro
-q                          # Mode silencieux
-qq                         # Mode très silencieux
```

#### Exemples
```bash
# Calcul standard
consang ma_base

# Calcul rapide (pour grandes bases)
consang -fast ma_base

# Recalcul complet
consang -scratch ma_base

# Mode silencieux
consang -q ma_base
```

---

## Outils d'analyse

### connex - Analyse de connexité

**Fonction :** Analyse les composantes connexes de la base

#### Syntaxe
```bash
connex <base> [options]
```

#### Options d'analyse
```bash
-a                          # Toutes composantes connexes
-bf                         # Par fichiers d'origine
-s                          # Produire statistiques
-d <int>                    # Détail pour cette longueur
```

#### Options de nettoyage
```bash
-del <int>                  # Supprimer branches taille <= valeur
-cnt <int>                  # Supprimer cnt branches taille <= -del
-exact                      # Supprimer seulement taille = -del exactement
```

#### Options de sortie
```bash
-o <file>                   # Sortie vers fichier
-i <file>                   # Ignorer ce fichier
```

#### Connexion serveur
```bash
-server <string>            # Nom serveur (défaut: 127.0.0.1)
-gwd_p <number>             # Port gwd (défaut: 2317)
```

#### Exemples
```bash
# Analyse statistiques
connex ma_base -s

# Toutes composantes connexes
connex ma_base -a -o connexes.txt

# Nettoyage branches <= 5 personnes
connex ma_base -del 5

# Analyse par fichiers origine
connex ma_base -bf -o analyse_origine.txt
```

### gwdiff - Comparaison de bases

**Fonction :** Compare deux bases de données généalogiques

#### Syntaxe
```bash
gwdiff [options] base1 base2
```

#### Options obligatoires
```bash
-1 <fn> <occ> <sn>          # Personne départ base1 (obligatoire)
-2 <fn> <occ> <sn>          # Personne départ base2 (obligatoire)
```

#### Options de comparaison
```bash
-d                          # Vérifier descendants (défaut)
-ad                         # Vérifier descendants de tous ascendants
```

#### Options de sortie
```bash
-html <root>                # Format HTML pour rapport
-mem                        # Économiser mémoire (plus lent)
```

#### Exemples
```bash
# Comparaison simple
gwdiff -1 "Pierre" 0 "MARTIN" -2 "Pierre" 0 "MARTIN" base1 base2

# Comparaison avec descendants des ascendants
gwdiff -1 "Jean" 0 "DUPONT" -2 "Jean" 0 "DUPONT" -ad base1 base2

# Rapport HTML
gwdiff -1 "Marie" 0 "BERNARD" -2 "Marie" 0 "BERNARD" \
       -html /tmp/rapport base_old base_new
```

---

## Utilitaires système

### gwsetup - Configuration système

**Fonction :** Interface de configuration pour Geneweb

#### Syntaxe
```bash
gwsetup [options]
```

#### Options
```bash
-bd <dir>                   # Répertoire bases installées
-bindir <string>            # Répertoire binaires (défaut: -gd)
-daemon                     # Mode daemon Unix
-gd <string>                # Répertoire gwsetup
-gwd_p <number>             # Port gwd (défaut: 2317)
-lang <string>              # Langue par défaut
-only <file>                # Fichier adresses autorisées seules
-p <number>                 # Port gwsetup (défaut: 2316)
```

#### Exemples
```bash
# Démarrage standard
gwsetup

# Configuration personnalisée
gwsetup -bd /var/geneweb -p 8080 -lang fr

# Mode daemon
gwsetup -daemon -gwd_p 2317 -p 2316
```

### update_nldb - Mise à jour NLDB

**Fonction :** Met à jour la base de données de liens naturels

#### Syntaxe
```bash
update_nldb [options] <base>
```

#### Options
```bash
-bd <DIR>                   # Répertoire bases (défaut: ".")
-debug                      # Mode debug
```

### cache_files - Génération de caches

**Fonction :** Génère les fichiers de cache pour accélération

#### Syntaxe
```bash
cache_files [options] base
```

#### Options de cache
```bash
-al                         # Alias
-all                        # Tout
-es                         # Domaines
-fn                         # Prénoms
-fna                        # Ajouter alias prénoms
-oc                         # Professions
-pl                         # Lieux
-pu                         # Noms publics
-qu                         # Qualificatifs
-sn                         # Noms de famille
-sna                        # Ajouter alias noms
```

#### Options système
```bash
-bd <DIR>                   # Répertoire bases
-prog                       # Barre de progression
```

#### Exemples
```bash
# Cache complet
cache_files -all ma_base

# Cache noms et lieux
cache_files -sn -fn -pl ma_base

# Cache avec barre de progression
cache_files -all -prog ma_base
```

---

## Exemples d'utilisation

### Workflow complet : GEDCOM → Geneweb → Web

```bash
# 1. Import GEDCOM
ged2gwb famille.ged -o ma_famille

# 2. Compilation en base Geneweb
gwc -cg -o ma_famille famille.gw

# 3. Calcul consanguinité
consang ma_famille

# 4. Réparation et optimisation
gwfixbase -index ma_famille

# 5. Génération caches
cache_files -all ma_famille

# 6. Démarrage serveur web
gwd -bd . -p 2317 -auth users.txt
```

### Maintenance régulière

```bash
# Vérification intégrité
gwfixbase -dry-run ma_base

# Nettoyage connexité
connex ma_base -s -del 2

# Mise à jour consanguinité
consang -fast ma_base

# Reconstruction index
gwfixbase -index ma_base
```

### Export sélectif

```bash
# Export famille complète (5 générations)
gwu ma_base -key "Jean.0 MARTIN" -a 5 -d 5 -o martin_famille.gw

# Export GEDCOM public (censure < 100 ans)
gwb2ged ma_base -c 100 -o export_public.ged

# Export par nom de famille
gwb2ged ma_base -s "MARTIN" -s "BERNARD" -o martin_bernard.ged
```

### Configuration serveur production

```bash
# Serveur sécurisé avec logs
gwd -daemon \
    -bd /var/geneweb/bases \
    -hd /usr/share/geneweb \
    -auth /etc/geneweb/users.txt \
    -digest \
    -log /var/log/geneweb.log \
    -p 2317 \
    -n_workers 50 \
    -max_pending_requests 200
```

### Comparaison et synchronisation

```bash
# Comparaison deux versions base
gwdiff -1 "Pierre" 0 "MARTIN" -2 "Pierre" 0 "MARTIN" \
       -html /tmp/diff base_v1 base_v2

# Analyse différences connexité
connex base_v1 -a -o v1_connex.txt
connex base_v2 -a -o v2_connex.txt
diff v1_connex.txt v2_connex.txt
```

---

## Résumé des bonnes pratiques

### ✅ **Workflow recommandé**

1. **Import** : `ged2gwb` ou création `.gw`
2. **Compilation** : `gwc -cg` avec consanguinité
3. **Vérification** : `gwfixbase -dry-run`
4. **Optimisation** : `gwfixbase -index`
5. **Cache** : `cache_files -all`
6. **Serveur** : `gwd` avec configuration sécurisée

### ⚠️ **Précautions importantes**

- **Toujours sauvegarder** avant `gwfixbase` sans `-dry-run`
- **Tester avec `-dry-run`** avant modifications
- **Utiliser `-mem`** pour économiser mémoire sur grandes bases
- **Configurer authentification** pour serveur public
- **Monitorer logs** en production

### 🚀 **Optimisations performance**

- **`-fast`** pour calculs sur grandes bases
- **`cache_files`** pour accélération interface
- **`-n_workers`** ajusté selon CPU
- **`-cache-in-memory`** pour bases fréquemment consultées

---

**Document généré le 10 septembre 2025**  
**Basé sur l'analyse des commandes Geneweb disponibles**
