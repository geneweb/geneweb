# Sources to group

`m=SOURCE_TERMS` (button "Sources to group" of the source dictionary; its
own pages, same layout as `m=PLACE_TERMS`): clusters of spellings of the **wizards** (tab `part=w`) and of
the **sources** (tab `part=s`, chosen by the beginning of their key), then
the same steps as for the places: spelling to keep, preview, validation (`m=SOURCE_TERMS_OK`, as
m=MOD_DATA_OK, history action "ms").

## Syntax

A source string is a list of signatures separated by commas:

```
wizard (source) date
wizard date (source)
```

- **wizard**: `[[w:ident]]`, or plain text when it is a *known wizard*: the
  ident of a `[[w:…]]`, or a text found at least 20 times before a `(` at
  the start of a source string (date removed, no digit, at most 4 words);
- **source**: between parentheses or not;
- **date**: `24 ii 2021`, `28 VIII 2020`, `4xii2018`, `27.ix.2019`,
  `08.04.2022`, `25/11/04`, `28 avril 07`, `xi 21`, `2020-08-28`…

A string is cut only at a comma (outside parentheses) followed by a wizard:
commas inside a source (`AD Vosges, AC Gérardmer`) are kept. A segment
without a wizard is a single source.

## Cluster keys

- wizard: lowered, without accents and punctuation, first word reduced to its
  initial: `Gilles Francez`, `G.Francez`, `[[w:G.Francez]]` → `gfrancez`;
  `PRactMadoux`, `P Ract Madoux` → `practmadoux`;
- source: letters and digits only: `AD 75`, `AD75`, `ad-75` → `ad75`;
  `état civil`, `Etat-Civil` → `etatcivil`.

## Examples (test/source_test.ml)

| Source string | Wizard | Source | Date |
|---|---|---|---|
| `[[w:P.Colin]] (AD Vosges, RP paroisse Sainte-Croix, Saint-Dié) 7 v 2013, [[w:P.Colin]] (…) 8 v 2013` | `[[w:P.Colin]]` ×2 | `AD Vosges, RP paroisse Sainte-Croix, Saint-Dié`, … | `7 v 2013`, `8 v 2013` |
| `BdeRauglaudre, RP Stainville 1717-1788 28/403, 4xii2018` | `BdeRauglaudre` | `RP Stainville 1717-1788 28/403` | `4xii2018` |
| `D.Gaultier (Le Matin du 2/10/1933) 7/8/11` | `D.Gaultier` | `Le Matin du 2/10/1933` | `7/8/11` |
| `E.Polti ( Bottin Mondain ) 20.12.2005, [[w:J-N.Pommier]] (Archives de la Corrèze) 19 iii 2012, R.de Tessieres (état civil) 7 iii 2020` | 3 signatures | `Bottin Mondain`, `Archives de la Corrèze`, `état civil` | 3 dates |
| `Nicolas Soulard 10.02.2009 (AD72 vue 103)` | `Nicolas Soulard` | `AD72 vue 103` | `10.02.2009` |
| `AD 87, f° 109/180` | – | `AD 87, f° 109/180` | – |
| `AD59 (vue 12)` | – (digit) | `AD59 (vue 12)` | – |

## Roglo (sources cache, 2025)

Analysis of `roglo_sources.cache.gz` with the same rules:

- 1 298 225 source strings, 1 545 385 signatures;
- wizard found in 95 % of the signatures (half `[[w:…]]`, half plain text),
  date in 76 %;
- 606 `[[w:…]]` idents, about 530 plain-text known wizards;
- wizards: 526 keys, **309 clusters** of several spellings, e.g.
  `Gilles Francez` (19 104) / `[[w:G.Francez]]` (16 893);
  `PRactMadoux` / `P Ract Madoux` / `P.Ract Madoux` / `[[w:P.Ract Madoux]]`;
  `Benoît Ravinet` (24 261) / `[[w:B.Ravinet]]` (47);
- sources: 726 315 keys, **35 618 clusters** (89 742 spellings), e.g.
  `état civil` / `état-civil` / `Etat-Civil`;
  `faire-part` / `Faire-part` / `faire part`;
  `AD75` / `AD 75`; `RP` / `R.P.`; `Wikipedia` / `wikipedia` / `Wikipédia`.
