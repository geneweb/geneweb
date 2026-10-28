# `places_terms` test base

A small base for the spellings to group (`m=PLACE_TERMS`, button
"Spellings to group" of the place dictionary), the places to complete
(`m=PLACE_TERMS&groups=on`), and the renaming of a place term, the merging of
spellings or the completing of places. It is the shared reference: what each page should
show is described below, so a screen can be checked (or discussed) against
it.

Build it, then open `places_terms?m=PLACE_TERMS` as wizard:

```
distribution/gw/gwc -f -bd distribution/bases -o places_terms test/places_terms.gw
```

Every renaming scenario modifies the base: rebuild it before each one.

## Content

24 persons, 5 families, 28 distinct place strings. "Uses" counts the persons
and families using a place (once per record).

| Place | Uses | Used by |
|---|---|---|
| `Coutances, Manche, France` | 1 | Jean DUPONT (birth) |
| `St-Lô, Manche, France` | 2 | Jean DUPONT (death), DUPONT × MARTIN (marriage) |
| `Saint-Lô, Manche, France` | 1 | Marie MARTIN (birth) |
| `Saint Lo, Manche, France` | 1 | Marie MARTIN (death) |
| `Saint-Martin, Manche, France` | 1 | Pierre DUPONT |
| `Saint-Martin, Calvados, France` | 1 | Anne DUPONT |
| `[Le Bourg] - Saint-Martin, Manche, France` | 1 | Louis DUPONT |
| `Paris (75)` | 1 | Jeanne DUPONT (birth) |
| `Paris, 75, Île-de-France, France` | 1 | Jeanne DUPONT (death) |
| `Paris, Paris, France` | 1 | Paul DURAND |
| `Valognes, Manche, France` | 2 | Claire LEROY (birth), DURAND × LEROY (marriage); **also the domain of Paul DURAND's title "comte"** |
| `Tessy, Manche, France` | 1 | Henri DURAND |
| `Tessy, Vire, Manche, France` | 1 | Louise DURAND |
| `Saint-Lô, Manche, Normandie, France` | 1 | Jules DURAND |
| `Normandie, France` | 1 | Marthe DURAND (birth) |
| `Paris, Lutèce, France` | 1 | Marthe DURAND (death) |
| `Granville, Manche, France` | 2 | Marc LEFEBVRE, Yves LEFEBVRE |
| `[Saint-Pair] – Granville, Manche, France` (en dash) | 1 | LEFEBVRE × LEGRAND (marriage) |
| `Granville , Manche, France` (space before the comma) | 1 | Rose LEFEBVRE |
| `Saint-Malo, Ille-et-Vilaine, France` | 2 | Yann ROBIN, Marie ROBIN |
| `St Malo, Ille-et-Vilaine, France` | 1 | ROBIN × GUERIN (marriage) |
| `Saint Malo, Ille-et-Vilaine, France` | 1 | Anne GUERIN |
| `saint-malo, Ille-et-Vilaine, France` | 1 | Malo ROBIN |
| `Avranches` | 1 | Louis HAMEL |
| `Avranches (Manche)` | 1 | HAMEL × LEROUX (marriage) |
| `Avranches, Manche` | 1 | Jeanne LEROUX |
| `Avranches, Manche, France` | 1 | Pierre HAMEL |
| `Saint-Martin` | 1 | Rose HAMEL |

The cases it covers: homonymous towns (Saint-Martin in Manche and Calvados),
spelling variants (St-Lô / Saint-Lô / Saint Lo, `Granville` / `Granville `),
suburbs with a hyphen and an en dash, a trailing parenthesised component
(`Paris (75)`), the same term at two depths (`Paris, Paris, France`), a
term at different levels (Normandie as a region and as a leaf), and a place
string also used outside places (the title domain).

## Expected dictionary

**Index of the terms** (`m=PLACE_TERMS&s=`, not linked from the pages): 26 terms, initials
`7 A C F G I L M N P S T V` (initials of the lowered form: `Île-de-France`
is under `I`, `75` under `7`).

**Terms**: contexts are written root first; "–" is the top level.

| Term | Contexts | Places | Uses | Other spellings |
|---|---|---|---|---|
| `75` | 2: – (`Paris (75)`); France › Île-de-France | 2 | 2 | |
| `Avranches` | 3: – (`Avranches`); Manche (`Avranches (Manche)`, `Avranches, Manche`); France › Manche | 4 | 4 | |
| `Calvados` | 1: France | 1 | 1 | |
| `Coutances` | 1: France › Manche | 1 | 1 | |
| `France` | 1: – | 23 | 27 | |
| `Granville` | 1: France › Manche | 2 | 3 | `Granville ` |
| `Granville ` | 1: France › Manche | 1 | 1 | `Granville` |
| `Ille-et-Vilaine` | 1: France | 4 | 5 | |
| `Le Bourg` | 1: France › Manche › Saint-Martin, *lieu-dit* | 1 | 1 | |
| `Lutèce` | 1: France | 1 | 1 | |
| `Manche` | 3: – (`Avranches (Manche)`, `Avranches, Manche`); France (13 places); France › Normandie (1) | 16 | 19 | |
| `Normandie` | 1: France | 2 | 2 | |
| `Paris` | 5: 75 (`Paris (75)`); France (middle of `Paris, Paris`); France › Lutèce; France › Paris (leaf of `Paris, Paris`); France › Île-de-France › 75 | 5 | 5 | |
| `Saint Lo` | 1: France › Manche | 1 | 1 | `Saint-Lô`, `St-Lô` |
| `Saint Malo` | 1: France › Ille-et-Vilaine | 1 | 1 | `Saint-Malo`, `St Malo`, `saint-malo` |
| `Saint-Lô` | 2: France › Manche; France › Normandie › Manche | 2 | 2 | `Saint Lo`, `St-Lô` |
| `Saint-Malo` | 1: France › Ille-et-Vilaine | 1 | 2 | `Saint Malo`, `St Malo`, `saint-malo` |
| `Saint-Martin` | 3: – (`Saint-Martin` alone); France › Calvados (1); France › Manche (2, one with the lieu-dit Le Bourg) | 4 | 4 | |
| `Saint-Pair` | 1: France › Manche › Granville, *lieu-dit* | 1 | 1 | |
| `St Malo` | 1: France › Ille-et-Vilaine | 1 | 1 | `Saint Malo`, `Saint-Malo`, `saint-malo` |
| `St-Lô` | 1: France › Manche | 1 | 2 | `Saint Lo`, `Saint-Lô` |
| `Tessy` | 2: France › Manche; France › Manche › Vire | 2 | 2 | |
| `Valognes` | 1: France › Manche | 1 | 2 | |
| `Vire` | 1: France › Manche | 1 | 1 | |
| `saint-malo` | 1: France › Ille-et-Vilaine | 1 | 1 | `Saint Malo`, `Saint-Malo`, `St Malo` |
| `Île-de-France` | 1: France | 1 | 1 | |

"Other spellings" are the terms of the same cluster (see below).

`France` and `Normandie` count `Normandie, France` and
`Saint-Lô, Manche, Normandie, France`; the title domain is not a place use.

## Expected clusters

`m=PLACE_TERMS`: 3 clusters, by decreasing total uses (then by
key). The cluster key is `Name.abbrev (Name.lower term)`: case, accents,
hyphens and spaces are ignored, "Saint" is read as "St". In a cluster, the
spellings come by decreasing uses (then alphabetically); the first one is
the spelling kept by default.

| Key | Spellings (uses) | Places | Uses |
|---|---|---|---|
| `st lo` | `Saint-Lô` (2), `St-Lô` (2), `Saint Lo` (1) | 4 | 5 |
| `st malo` | `Saint-Malo` (2), `Saint Malo` (1), `St Malo` (1), `saint-malo` (1) | 4 | 5 |
| `granville` | `Granville` (3), `Granville ` (1) | 3 | 4 |

`Saint-Martin` is not in a cluster: there is a single spelling, in two
contexts (homonyms are not spelling variants).

## Expected places to complete

`m=PLACE_TERMS&groups=on`: a place is to complete when its terms, leaf
first, are the beginning of the terms of another place with the same suburb
(`aaaa (bbbb)` and `aaaa, bbbb` have the same terms). 4 groups, by
decreasing uses (then by complete place):

| Complete place | Places to complete | Uses |
|---|---|---|
| `Avranches, Manche, France` | `Avranches`, `Avranches, Manche`, `Avranches (Manche)` | 4 |
| `Paris, 75, Île-de-France, France` | `Paris (75)` | 2 |
| `Saint-Martin, Calvados, France` | `Saint-Martin` (in 2 groups) | 2 |
| `Saint-Martin, Manche, France` | `Saint-Martin` (in 2 groups) | 2 |

Not grouped: `Saint-Lô, Manche, France` / `Saint-Lô, Manche, Normandie,
France` and `Tessy, Manche, France` / `Tessy, Vire, Manche, France` (a level
missing in the middle is not a beginning), `[Saint-Pair] – Granville, …` /
`Granville, …` (different suburb), `Granville , …` / `Granville, …`
(different spelling: a cluster of spellings).

On a group page, the complete place is chosen with a radio button (the most
used of the places having its terms), the places to replace by it with
checkboxes, checked except the places in several groups (ambiguous).

## Renaming scenarios

Each starts from a freshly built base. A renaming does what m=MOD_DATA_OK
does for each place: the records using the old place move to the new string
(created, or already existing), the old string stays in the base unused, and
each modified person, and both spouses of each modified family, get a line
"mq" in the history.

Preview: *new place* (the new string is created), *merged into an existing
place*, or *merged with another renamed place* (several places get the same
new string). "Result" is the message after *Validate*: places renamed (of
which merged), records (persons + families) updated.

| # | Term → new term | Checked | Preview | Result |
|---|---|---|---|---|
| 1 | `St-Lô` → `Saint-Lô` | all | `St-Lô, Manche, France`: merged into the existing `Saint-Lô, Manche, France` | 1 (1), 2 (Jean DUPONT, marriage DUPONT × MARTIN) |
| 2 | `Saint Lo` → `Saint-Lô` | all | merged into the existing `Saint-Lô, Manche, France` | 1 (1), 1 (Marie MARTIN) |
| 3 | `Granville ` → `Granville` | all | merged into the existing `Granville, Manche, France` | 1 (1), 1 (Rose LEFEBVRE) |
| 4 | `Coutances` → `Coutances-sur-Mer` | all | new place | 1 (0), 1 (Jean DUPONT) |
| 5 | `Valognes` → `Valognes-Cotentin` | all | new place | 1 (0), 2 (Claire LEROY, marriage DURAND × LEROY); Paul DURAND's title domain stays `Valognes, Manche, France` |
| 6 | `Paris` → `Lutèce` | contexts France, France › Lutèce, France › Paris | `Paris, Lutèce, France`: new place `Lutèce, Lutèce, France`; `Paris, Paris, France`: merged with another renamed place (same new string) | 2 (1), 2 (Marthe DURAND, Paul DURAND) |
| 7 | `Paris` → `Lutèce` | context France only (the middle `Paris` of `Paris, Paris, France`) | `Paris, Paris, France` → `Paris, Lutèce, France`: merged into the existing place | 1 (1), 1 (Paul DURAND) |
| 7b | `Paris` → `Lutèce` | context France › Paris only (the leaf `Paris` of `Paris, Paris, France`) | `Paris, Paris, France` → `Lutèce, Paris, France`: new place | 1 (0), 1 (Paul DURAND) |
| 8 | `Tessy` → `Tessy, Vire` | all | `Tessy, Manche, France`: merged into the existing `Tessy, Vire, Manche, France`; `Tessy, Vire, Manche, France` → `Tessy, Vire, Vire, Manche, France`: new place | 2 (1), 2 (Henri DURAND, now at `Tessy, Vire, Manche, France`; Louise DURAND, at `Tessy, Vire, Vire, Manche, France`) |
| 9 | `Manche` → `Manche, Normandie` | context France only (13 places) | 12 new places; `Saint-Lô, Manche, France` merged into the existing `Saint-Lô, Manche, Normandie, France` | 13 (1), 14 (11 persons, 3 marriages) |
| 10 | cluster `st malo`, keep `Saint-Malo` | all | in this order: `Saint Malo, …`, `St Malo, …`, `saint-malo, …`, each merged into the existing `Saint-Malo, Ille-et-Vilaine, France` (the places of `Saint-Malo` itself are unchanged, not listed) | 3 (3), 3 (Anne GUERIN, Malo ROBIN, marriage ROBIN × GUERIN) |
| 11 | cluster `st lo`, keep `Saint-Lô` | all | `Saint Lo, …` and `St-Lô, …` merged into the existing `Saint-Lô, Manche, France` | 2 (2), 3 (Jean DUPONT, Marie MARTIN, marriage DUPONT × MARTIN) |
| 12 | cluster `st malo`, other spelling `St-Malo` | all | all four become `St-Malo, Ille-et-Vilaine, France`: `Saint Malo, …` (first in alphabetical order) new place, `Saint-Malo, …`, `St Malo, …` and `saint-malo, …` merged with another renamed place | 4 (3), 5 (Yann, Marie and Malo ROBIN, Anne GUERIN, marriage ROBIN × GUERIN) |
| 13 | cluster `granville`, keep `Granville` | all | `Granville , Manche, France` merged into the existing `Granville, Manche, France`; `[Saint-Pair] – Granville, …` unchanged (already `Granville`) | 1 (1), 1 (Rose LEFEBVRE) |

| 14 | group `Avranches, Manche, France` | all (default) | `Avranches`, `Avranches, Manche`, `Avranches (Manche)`: each merged into the existing `Avranches, Manche, France` | 3 (3), 3 (Louis HAMEL, Jeanne LEROUX, marriage HAMEL × LEROUX) |
| 15 | group `Saint-Martin, Manche, France` | none (default: `Saint-Martin` is in 2 groups) | no modification | – |
| 15b | group `Saint-Martin, Manche, France` | `Saint-Martin` checked | merged into the existing `Saint-Martin, Manche, France` | 1 (1), 1 (Rose HAMEL) |
| 16 | group `Paris, 75, Île-de-France, France` | all | `Paris (75)` merged into the existing place | 1 (1), 1 (Jeanne DUPONT) |

After scenario 9, the term `Normandie` has a single context, France, with 14
places, and the term `Manche` two contexts: – (the two Avranches places
without France) and France › Normandie.
