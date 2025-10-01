# Guide Détaillé des Options gwgc

Date de création : 11 septembre 2025
Outil : gwgc - Garbage collector de base GeneWeb
Version analysée : master

---

## Vue d'ensemble

`gwgc` exécute le garbage collector sur une base GeneWeb. Il parcourt la base et identifie les éléments référencés (personnes, familles, chaînes) et effectue le nettoyage des éléments non utilisés. Il peut fonctionner en mode simulation (dry-run) sans rien modifier.

---

## Usage

```
gwgc [OPTION] base
```

- `base`: chemin vers la base GeneWeb (répertoire `.gwb` ou chemin complet). Le répertoire parent est utilisé pour définir `-bd` implicitement.

### Options

- `--dry-run`: ne pas committer les changements; affiche uniquement ce qui serait fait.

---

## Verrouillage et sécurité

- `gwgc` utilise un fichier de verrou (`gwd.lck` via `Mutil.lock_file`) pour éviter les accès concurrents à la base pendant le GC.
- En cas d’erreur pendant la section critique, l’exception est formatée et le processus termine avec un code 2.

---

## Sortie

En fin d’exécution, `gwgc` imprime un récapitulatif:
```
<base>:
	nb of persons: <Np>
	nb of families: <Nf>
	nb of strings: <Ns>
```

- `nb of persons`: nombre d’entrées personnes après GC
- `nb of families`: nombre d’entrées familles après GC
- `nb of strings`: nombre de chaînes uniques après GC

En mode `--dry-run`, ces nombres correspondent à ce que donnerait le GC sans appliquer les changements.

---

## Exemples

- Exécuter un GC avec modifications:
```
gwgc bases/ma_base.gwb
```

- Simulation (aucun changement), pour contrôle:
```
gwgc --dry-run bases/ma_base.gwb
```

---

## Dépannage

- `Usage: gwgc [OPTION] base` imprimé et sortie code 1: l’argument `base` est manquant.
- Sortie code 2 avec message de verrou: impossible de prendre le verrou; vérifier qu’aucun autre processus n’utilise la base.

---

## Bonnes pratiques

- Lancer `--dry-run` d’abord sur de grandes bases pour estimer l’impact.
- Éviter d’exécuter `gwgc` pendant des opérations d’écriture (import, gwd actif sans verrou).
- Conserver une sauvegarde récente de la base avant un GC si possible.
