# GeneWeb Localization Audit

This document catalogs all French-language content that needs translation for proper
multi-language support. GeneWeb's template engine supports `[*keyword]` syntax for
server-side translation via `hd/lang/lexicon.txt`, but many UI strings — especially
in JavaScript files and preview images — are hardcoded in French.

## What has been fixed (2026-03-14)

### p_mod.js — Module selector panel

**Problem:** Module label names (individu, fratrie, chronologie, etc.) and button
tooltips displayed French filenames as labels.

**Fix applied:** Labels are now passed from the server-side template (`menubar.txt`)
via a `data-labels` JSON attribute on `#p_mod_table`. The JS reads these at init time.
This approach is **generalizable to other languages**: to support French, German, etc.,
replace the hardcoded English strings in `menubar.txt` with `[*keyword]` lexicon
lookups once the translation keys are added to `hd/lang/lexicon.txt`.

**Current state:** English labels are hardcoded in `menubar.txt`. To make this fully
i18n-aware, add these keys to `hd/lang/lexicon.txt` and change the `data-labels`
attribute to use `[*keyword]` syntax:

| Internal key  | English      | Lexicon key needed |
|---------------|-------------|-------------------|
| individu      | Individual  | individual        |
| parents       | Parents     | parents (exists)  |
| unions        | Unions      | unions            |
| fratrie       | Siblings    | siblings (exists) |
| relations     | Relations   | relations         |
| chronologie   | Timeline    | timeline          |
| notes         | Notes       | notes             |
| sources       | Sources     | sources           |
| arbres        | Trees       | trees             |
| htrees        | H-trees     | h-trees           |
| gr_parents    | Grandparents| grandparents      |
| ligne         | Lineage     | lineage           |
| data_3col     | 3-col data  | 3-col data        |

---

## What still needs fixing

### 1. Module preview images (39 files)

**Location:** `hd/images/modules/` (also copied to app bundle and distribution)

These are screenshot images taken from a **French-language** GeneWeb instance. They
contain French section headers, labels, and data. Each image needs to be re-captured
from an English-language instance.

| Image file          | French text visible                                    |
|---------------------|-------------------------------------------------------|
| `individu_1.jpg`    | French titles and labels                              |
| `individu_2.jpg`    | French titles and labels                              |
| `individu_3.jpg`    | French titles and labels                              |
| `fratrie_1.jpg`     | "Fratrie", "Demi-frères et soeurs"                    |
| `fratrie_2.jpg`     | "Fratrie" header with photos                          |
| `fratrie_3.jpg`     | "Fratrie" complete view                               |
| `fratrie_4.jpg`     | "Fratrie" complete + photos                           |
| `chronologie_1.jpg` | "Chronologie" header                                  |
| `chronologie_2.jpg` | "Chronologie" header + events                         |
| `arbres_1.jpg`      | "Arbre d'ascendance"                                  |
| `arbres_2.jpg`      | Tree view with French labels                          |
| `arbres_3.jpg`      | Compact tree with French labels                       |
| `arbres_4.jpg`      | Descendant tree with French labels                    |
| `htrees_1.jpg`      | "Aperçu de l'arbre", French labels throughout         |
| `htrees_2.jpg`      | H-tree family view, French                            |
| `htrees_3.jpg`      | H-tree 6 gen, French                                  |
| `htrees_4.jpg`      | H-tree 8 gen, French                                  |
| `htrees_5.jpg`      | H-tree HI view, French                                |
| `parents_1.jpg`     | "Parents" (same in both languages)                    |
| `parents_2.jpg`     | Parents + photos, French labels                       |
| `parents_3.jpg`     | Parents evolved, French                               |
| `parents_4.jpg`     | Parents complete, French                              |
| `parents_5.jpg`     | Parents complete + photos, French                     |
| `unions_1.jpg`      | "Familles" header, French dates/labels                |
| `unions_2.jpg`      | Unions + photos, French                               |
| `unions_3.jpg`      | Unions evolved, French                                |
| `unions_4.jpg`      | Unions complete, French                               |
| `unions_5.jpg`      | Unions complete + photos, French                      |
| `relations_1.jpg`   | "Relations" with French labels                        |
| `relations_2.jpg`   | Relations complete, French                            |
| `notes_1.jpg`       | "Notes individuelles", "Notes concernant les unions"  |
| `notes_2.jpg`       | Notes complete, French                                |
| `sources_1.jpg`     | "Sources" with French field labels                    |
| `sources_2.jpg`     | Sources complete, French                              |
| `gr_parents_1.jpg`  | "Grands-parents paternels, oncles et tantes"          |
| `gr_parents_2.jpg`  | Grandparents three-col, French                        |
| `ligne_1.jpg`       | Lineage view (appears mostly empty/neutral)           |
| `data_3col_1.jpg`   | 3-column layout, French section headers throughout    |
| `zz_1.jpg`          | Default template, all French headers                  |
| `menubar_1.jpg`     | Menubar icons (language-neutral)                      |

**To re-capture:** Run GeneWeb with `lang=en`, navigate to a person with rich data
(e.g., the demo "Jean Dupont" or equivalent), set each p_mod combination, and take
browser screenshots at approximately the same dimensions as the originals.

### 2. fanchart.txt — Fan chart template (23 French strings)

**Location:** `hd/etc/fanchart.txt`

Hardcoded French in button titles, section headings, and placeholders:

| Line | French text                               | English replacement              |
|------|-------------------------------------------|----------------------------------|
| 103  | `title="Masquer"`                         | `title="[*hide]"`                |
| 134  | `title="Aide et raccourcis"`              | `title="[*help] & shortcuts"`   |
| 162  | `title="Supprimer une génération"`        | `title="Remove a generation"`   |
| 165  | `title="Ajouter une génération"`          | `title="Add a generation"`      |
| 168  | `Développer/Réduire les implexes`         | `Expand/Collapse implexes`      |
| 175  | `<div>Ouverture</div>`                    | `<div>Opening</div>`            |
| 177  | `title="Vue en demi-disque (180°)"`       | `title="Half-disk view (180°)"` |
| 179  | `title="Vue complète (359°)"`             | `title="Full view (359°)"`      |
| 184  | `<div>Mode</div>`                         | `<div>Mode</div>` (OK)          |
| 186  | `title="Mode circulaire..."`              | `title="Circular mode..."`      |
| 196  | `<option>Grande</option>`                 | `<option>Large</option>`        |
| 197  | `<option>Lisible</option>`                | `<option>Readable</option>`     |
| 205  | `title="Âges décès et durées mariage"`    | `title="Death ages & marriage"` |
| 209  | `title="Colorisation des lieux..."`       | `title="Place colorization..."` |
| 214  | `title="Trier par ordre alphabétique"`    | `title="Sort alphabetically"`   |
| 231  | `title="Afficher le panneau"`             | `title="Show panel"`            |
| 236  | `Lieux & Événements`                      | `Places & Events`               |
| 239  | `title="Masquer"`                         | `title="[*hide]"`               |
| 245  | `placeholder="Rechercher…"`               | `placeholder="Search…"`         |
| 246  | `title="Effacer"`                         | `title="Clear"`                 |
| 249  | `title="Tri"`                             | `title="Sort"`                  |
| 252  | `title="Événements"`                      | `title="Events"`                |
| 260  | `title="Totaux"`                          | `title="Totals"`                |

### 3. fanchart.js — Fan chart JavaScript (6 French strings)

**Location:** `hd/etc/js/fanchart.js`

| Line | French text                                            | English replacement                          |
|------|--------------------------------------------------------|----------------------------------------------|
| 3706 | `'Aucune personne dans cette tranche d'âge'`           | `'No person in this age range'`              |
| 3707 | `'Aucun mariage dans cette tranche de durée'`          | `'No marriage in this duration range'`       |
| 4671 | `"Afficher la génération suivante (données en mémoire)"` | `"Show next generation (data in memory)"`  |
| 4673 | `"Charger la génération suivante"`                     | `"Load next generation"`                     |
| 4675 | `"Aucun parent dans la génération suivante"`           | `"No parent in next generation"`             |
| 5165 | `'générations' : 'génération'`                         | `'generations' : 'generation'`               |

### 4. checkdata.js — Data validation (3 French strings)

**Location:** `hd/etc/js/checkdata.js`

| Line | French text                                  | English replacement                    |
|------|----------------------------------------------|----------------------------------------|
| 337  | `'Erreur: données de validation invalides'`  | `'Error: invalid validation data'`     |
| 439  | `'Timeout: validation non reçue'`            | `'Timeout: validation not received'`   |
| 578  | `` `Limité à ${l} résultats` ``              | `` `Limited to ${l} results` ``        |

### 5. notes_upd_album.txt — Image editor template (5 French strings)

**Location:** `hd/etc/notes_upd_album.txt` (marked with `TODO: TRANSLATE ME`)

| Line | French text                             | English replacement                |
|------|-----------------------------------------|------------------------------------|
| 5    | `Éditeur d'image cliquable`             | `Clickable image editor`           |
| 40   | `Éditeur d'image réactives`             | `Responsive image editor`          |
| 74   | `Autre` (label)                         | `Other`                            |
| 76   | `Autre chemin` (placeholder)            | `Other path`                       |
| 85   | `Détection visages`                     | `Face detection`                   |

---

## UI consistency issues

### 6. Gender indicators — inconsistent use of Mars/Venus symbols

GeneWeb uses a mix of approaches to indicate gender across its templates:

- **Mars/Venus symbols** (♂/♀) in some views (D'Aboville numbering, some tree displays)
- **Color coding** (blue/pink via CSS classes `male`/`female`) in other views
- **Gendered language** (born/née, he/she) from the lexicon in text-based views
- **No indicator at all** in some places

**Recommendation:** Standardize on Mars/Venus symbols as the primary visual indicator
across all templates, with color as secondary reinforcement. The symbols are compact,
universally understood, language-neutral, and work in both color and monochrome
contexts. This would require auditing all template files (`hd/etc/*.txt`) for gender
display and applying a consistent pattern.

---

## Recommended approach for full i18n

GeneWeb already has a mature i18n system (`[*keyword]` in templates, `hd/lang/lexicon.txt`).
The pattern established for p_mod.js can be applied to all JavaScript-generated UI:

1. **Template files (.txt):** Replace hardcoded French with `[*keyword]` syntax.
   Most translation keys already exist in the lexicon; add missing ones.

2. **JavaScript files (.js):** Pass translated strings from templates via `data-*`
   attributes on container elements. The JS reads these at init time. This ensures
   translations are always fresh (HTML is not cached) and respect the user's
   language setting.

3. **Preview images:** Must be re-captured per language, or replaced with
   language-neutral alternatives (CSS-rendered previews, SVG diagrams, etc.).

4. **After changing JS files:** Rename the minified file (e.g., increment version
   suffix) and update the reference in `js.txt` to bust browser caches. GeneWeb
   serves static JS with 1-year cache headers (`max-age=31536000`).
