# CLAUDE.md

This file provides guidance to AI coding assistants working on the
GeneWeb repository.

## Project Overview

GeneWeb is an open-source genealogy web application written in OCaml,
started in late 1997. It serves databases with millions of
individuals (Roglo.eu: 11M+ records). Version 7.1 (beta2, December
2025). Supported platforms: Linux, macOS, Windows (Cygwin/mingw64).

## Build and Test

```bash
opam install . --deps-only    # install dependencies
dune build                    # incremental build
dune runtest                  # run tests
dune fmt                      # auto-format (ocamlformat 0.28.1)
make distrib                  # build release distribution
```

Test locally after `make distrib`:
```bash
cd distribution && gw/gwd
```

## Architecture

Avoid bloating catch-all modules. `perso.ml` (~4000 LOC) and
`util.ml` are already overloaded — prefer creating focused modules.
`templ.ml` is the template engine runtime; changes there affect every
page.

## OCaml Conventions

1. Never disable compiler warnings.
2. Never remove code in `test/` — you can add tests.
3. Never use the `Obj` module.
4. **No global mutable state.** Do not use `ref` at module level.
   Pass data as function arguments. Existing global `ref` values are
   legacy debt — do not add more.
5. Do not use `List.nth`, `List.concat`, or `@` (quadratic). Use
   `Array` access or accumulator patterns.
6. When using standard library functions not available in OCaml 4.10,
   check `Geneweb_compat` for a backport before reimplementing.
7. `aux.ml` is a reserved Windows device name — do not use it as a
   filename (breaks CI on Windows/Cygwin).
8. No commented-out code in committed files.

## Template Engine

Templates are `.txt` files in `hd/etc/`. The engine is in
`lib/templ/` (parser/lexer) and `lib/templ.ml` (runtime). Custom
template engine.

### Syntax

- `%variable;` — Variable evaluation (semicolon required).
- `%if;(cond)...%elseif;...%else;...%end;` — Conditionals.
- `%foreach;iterator;...%end;` — Loops.
- `%apply;func(arg1, arg2)` — Function application (short form).
  Long form: `%apply;func%with;arg1%and;arg2%end;` — reads better
  when arguments are complex expressions.
- `%define;func(params)...%end;` — Function definition.
- `%let;name;value%in;` — Let binding.
- `%include;filename` — Include (resolved at parse time, see below).
- `[text]` / `[*text]` — Lexicon lookup (capitalized variant).
- `%sq;` — Strip following whitespace.
- `%nn;` — Strip surrounding newline.
- `%(comment%)` — Comment (not emitted).
- `%for;i;min;max;...%end;` — Numeric for loop.
- `%count;` / `%incr_count;` / `%reset_count;` — Built-in counter
  for numbering iterations.
- `%empty_sorted_list;` / `%add_in_sorted_list;value` /
  `%foreach;sorted_list_item;...%end;` — Sorted list accumulator.

Operators in conditions: `=`, `!=`, `<`, `<=`, `>`, `>=`, `and`,
`or`, `not`, `|` (integer division), `%` (modulo), `in` (substring
test: `%if;(x in y)` checks if `x` is contained in `y`).

### Escaping in Inline Script Blocks

The template parser interprets `%` and `[` **everywhere**, including
inside inline `<script>` blocks. External `.js` files are not
affected. Escape sequences: `%%` → literal `%`, `%[` → literal `[`,
`%]` → literal `]`. For complex JS, `\u0025` / `\u005b` avoid
ambiguity.

### `%include;` is Parse-Time AST Inlining

<<<<<<< HEAD
Resolved at **parse time** by the lexer. The included file's AST is
inlined (and cached). Not a runtime file load. Variables from the
parent are visible in the included file.

### Template File Resolution Order

For template `templx` on base `mybase`:
1. `bases/etc/mybase/templx/` → 2. `bases/etc/mybase/` →
3. `gw/etc/templx/` → 4. `gw/etc/`

### Frontend Stack

Bootstrap 4.6, jQuery, Chart.js 4.5.1 (UMD, not ESM), Font Awesome,
Leaflet 2.0 alpha. CSS/JS assets in `hd/etc/css/` and `hd/etc/js/`
use template wrappers (`css.txt`, `js.txt`) for conditional includes.
Regenerate `.min.js` (Terser) after editing any `.js` file.

### Best Practices

- **URL construction**: use `%url_set;key=value`, not the legacy
  `%foreach;env_binding;` loops.
- **Separation of concerns**: keep HTML, CSS, and JS disentangled.
  Isolate responsibilities into focused sub-templates via `%include;`.
- **W3C validation** helps catch structural issues.
- **Browser console**: monitor for errors; the debug icon in the
  footer shows query latency — watch for regressions.

## Localization

**Never hardcode English strings.** All user-visible text must use
the lexicon (`hd/lang/lexicon.txt`).

Format: 4 spaces before the key, then translations with lang code
flush left, `:`, space, text:
```
    died at age %s
fr: décédé à l'âge de %s
en: died at age %s
de: gestorben im Alter von %s
```

Stacked translations use `/` for variants selected by index:
```
    witness/witnesses
fr: témoin/témoins
en: witness/witnesses
```
In templates: `[witness/witnesses]0` → "witness",
`[witness/witnesses]1` → "witnesses". In OCaml:
`Util.transl_nth conf "witness/witnesses" n`. Placeholders `%s`
are filled by `Util.cftransl`.

Avoid duplicating existing entries — search the lexicon first.
Prefer a long specific entry over concatenating shorter ones: word
order differs across languages, making concatenation unreliable.
The lexicon supports declension for Slavic languages and specific
handling for German grammar.

## Calendar / Date System

Major source of bugs. Core rule: **`Dgreg(dmy, cal)` stores `dmy`
already converted to Gregorian for complete dates.** The `cal` field
is display metadata only — it tells rendering which calendar to
convert back to for display.

- Complete dates (day > 0, month > 0): `dmy` is Gregorian.
- Partial dates (day=0 or month=0): `dmy` stores the original
  calendar's values; `cal` is needed for interpretation.
- `Date.cdate_to_dmy_opt` extracts `dmy` from compressed dates.
  `Date.cdate_to_gregorian_dmy_opt` guarantees Gregorian values
  even for partial non-Gregorian dates.

Ignoring this invariant produces bugs that only manifest with
Julian/French Republican/Hebrew dates — easy to miss in testing.

## Search Architecture

The search system (introduced by DDR) uses a 4-level fallback:
`Name.crush_lower` (phonetic) → `Name.lower` (exact) →
`Name.strip_lower` → all phonetic.

Index files (`fnames.inx`/`fnames.dat`, `snames.inx`/`snames.dat`)
provide O(log n) access. **Never use `Collection.iter` for search**
— it is O(n) over the entire database.

## Wiki / Notes Syntax (TLSW)

Engine: `lib/wiki.ml` (rendering), `lib/notesLinks.ml` (parsing).

- `[[first/last]]` or `[[First Last]]` — Person link.
- `[[first/last/oc]]` — With occurrence number.
- `[[[page/text]]]` — Wiki page link.
- `[[image:path|alt|width]]` — Inline image.
- `[[image:path|left|200px|Caption]]` — Floated image with caption.

Images served via `m=IM&s=filename` (binary). Files in
`bases/src/{dbname}/images/`. HTML sanitized by `safe_html`
(`lib/util.ml`) — only whitelisted tags preserved, `on*` attributes
stripped.

## Database Iteration API

```ocaml
Driver.persons  : base -> person Collection.t
Driver.families : ?select:(family -> bool) -> base -> family Collection.t
Collection.fold : ?from:int -> ?until:int ->
  ('a -> 'b -> 'a) -> 'a -> 'b Collection.t -> 'a
Collection.iter : ('a -> unit) -> 'a Collection.t -> unit
```

Always check `Util.authorized_age conf base p` before exposing
date-based information.
=======
1. Never disable compiler warnings
2. Never remove code in `test/` directory - you can add tests
3. Never use the `Obj` module from OCaml stdlib
4. Don't use global mutable state (global references) - prefer passing data as arguments
5. Don't use `List.nth`, `List.concat`, or `@` from OCaml stdlib
6. Always run check formatting before creating commits
>>>>>>> f45da82bd (Add a type to handle paths)
