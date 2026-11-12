# CLAUDE.md

This file provides guidance to AI coding assistants working on the
GeneWeb repository.

## Project Overview

GeneWeb is an open-source genealogy web application written in OCaml,
started in late 1997. It serves databases with millions of
individuals (Roglo.eu: 11M+ records). Version 7.1 in preparation
(7.1~beta2, December 2025). Supported platforms: Linux, macOS, Windows
(Cygwin/mingw64; no `ADDR_UNIX` sockets and no gwd workers there).

## Build and Test

```bash
opam install . --deps-only    # install dependencies
dune build                    # incremental build
dune runtest                  # run tests
dune fmt                      # auto-format (ocamlformat 0.29)
make distrib                  # build release distribution
```

Test locally after `make distrib`:
```bash
cd distribution && gw/gwd
```

The dev profile turns warnings into errors: removing the last caller
of a function, or the last use of a module, breaks the build on the
now unused value (warning 32) or `open` (warning 33). Remove them in
the same commit, and grep `bin/`, `benchmark/` and `test/` for the
removed names too. Every commit must build and pass `dune fmt` on its
own.

## Architecture

Avoid bloating catch-all modules. `perso.ml` (~6300 LOC) and
`util.ml` (~3600 LOC) are already overloaded — prefer creating focused
modules, in a dedicated `lib/<x>/` subfolder rather than at the root of
`lib/`, and never two modules of the same name.
`templ.ml` is the template engine runtime; changes there affect every
page.

`plugins/` is legacy and is being phased out. Do not add new plugins,
and do not propose a plugin as the solution to a feature request.

## OCaml Conventions

1. Never disable compiler warnings.
2. Never remove a useful test in `test/` — you can add tests. When a
   function or a mode is removed or replaced, replace its tests
   (including `test/run_gw_test.sh`) in the same series.
3. Never use the `Obj` module.
4. **No global mutable state.** Do not use `ref` at module level.
   Pass data as function arguments. Existing global `ref` values are
   legacy debt — do not add more.
5. Do not use `List.nth`, `List.concat`, or `@` (quadratic). Use
   `Array` access or accumulator patterns.
6. When using standard library functions not available in OCaml 4.14,
   check `Geneweb_compat` for a backport before reimplementing.
7. `aux.ml` is a reserved Windows device name — do not use it as a
   filename (breaks CI on Windows/Cygwin).
8. No commented-out code in committed files.
9. Avoid `function` as the body of a named function: bind the
   argument and `match` on it. The arity then shows at the
   definition, the scrutinee has a name usable in annotations and in
   error messages, and adding a parameter does not require
   restructuring. `function` stays fine as an inline lambda, where
   the type comes from the context:
   `String.map (function '\\' -> '/' | c -> c) s`.
10. Ordinary variants by default. Use polymorphic variants
    (`` `Tag ``) only when a tag set is genuinely shared or extended
    across several types. A typo in an ordinary constructor is an
    immediate `Unbound constructor` on the exact line; a typo in a
    tag silently widens an inferred row type and surfaces later,
    elsewhere, as a `[< ]` / `[> ]` mismatch. Consuming polymorphic
    variants from libraries (`Yojson.Safe.t`) is fine — that is
    their API, not ours.
11. No `| _ -> ...` over a closed variant type: enumerate the
    constructors so that adding one raises a warning at every site
    that must be updated. `_` is fine for `int`, `char`, `string`.
12. `Hashtbl.replace`, not `Hashtbl.add`, unless shadowing is wanted.
    Open files with `Secure.with_open_*` or under `Fun.protect`.
13. Access control filters at display time: compute on the full data,
    then mask what the user may not see.
14. HTML emitted from OCaml: one readable `{|...|}` string with
    `Output.printf`, near 80 columns; no inline JS (`onclick`…), put a
    function in `js.txt` behind `%if;(e.m="…")`.
15. Document functions in English in the `.mli`; comment the `.ml`
    only for what the code cannot say.

## Template Engine

Templates are `.txt` files in `hd/etc/`. The engine is in
`lib/templ/` (parser/lexer) and `lib/templ.ml` (runtime). Custom
template engine.

### Variable Resolution on Person Pages

Variables are resolved through the `eval_*` chain of `perso.ml`
before reaching the generic one in `templ.ml`. A name defined in
`perso.ml` therefore shadows the generic one silently, with no
warning: the variable still returns a value, just not the expected
one. `%source;` on a person page is the person's genealogical source
field, not `Version.src`.

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
- `%(comment%)` — Comment (not emitted). Keep the
  `<!-- $Id: … -->` header as an HTML comment: it identifies the
  template in the page source.
- `%for;i;min;max;...%end;` — Numeric for loop.
- `%count;` / `%incr_count;` / `%reset_count;` — Built-in counter
  for numbering iterations.
- `%empty_sorted_list;` / `%add_in_sorted_list;value` /
  `%foreach;sorted_list_item;...%end;` — Sorted list accumulator;
  `mem_sorted_list("listb", value)` is 1 when `value` is in `listb`, else 0.
- `%apply;sorted_list_size("listc")` — Number of items of a sorted list.

`%expr(…)` takes no final `;`: a `;` after the parenthesis is
emitted as is (e.g. inside a URL). The arguments of `%apply` may hold
text, variables, translations, `%if`, `%for` and `%apply`, but not a
`%foreach` ("not impl tag"): to wrap a loop, split the macro into an
opening and a closing one (`order_open`/`order_close` in
`updateOrder.txt`).

Operators in conditions: `=`, `!=`, `<`, `<=`, `>`, `>=`, `and`,
`or`, `not`, `|` (integer division), `%` (modulo), `in` (substring
test: `%if;(x in y)` checks if `x` is contained in `y`).

### Macro Parameter Substitution Is Raw Text Replace

`%apply;func(args)` expands a `%define;`d macro by textually replacing
each parameter *name* with its argument *value*, one parameter at a
time in declaration order — it is not a scoped binding. A parameter
name that happens to occur as a substring of an already-substituted
value is rewritten too: a two-letter name like `tt` gets clobbered
inside a place value such as "Nottingham". Rule: give any macro
parameter that carries free text or place data an underscore-prefixed
name (`p_evt`, `p_sym`, `p_flg`, `p_sel`, `p_lbl`, …) — underscores
never occur in place strings, so cross-substitution cannot happen.
(See the `nbmds` / `evt` / `tags` macros in `hd/etc/list.txt`.)

### Escaping in Inline Script Blocks

The template parser interprets `%` and `[` **everywhere**, including
inside inline `<script>` blocks. External `.js` files are not
affected. Escape sequences: `%%` → literal `%`, `%[` → literal `[`,
`%]` → literal `]`. For complex JS, `\u0025` / `\u005b` avoid
ambiguity. Never interpolate a translation into a JS string delimited
by straight quotes: an apostrophe in some language breaks the whole
script. Pass it through a `data-` attribute instead.

### `%include;` is Parse-Time AST Inlining

Resolved at **parse time** by the lexer. The included file's AST is
inlined (and cached). Not a runtime file load. Variables from the
parent are visible in the included file.

### Template File Resolution Order

For template `templx` on base `mybase`:
1. `bases/etc/mybase/templx/` → 2. `bases/etc/mybase/` →
3. `gw/etc/templx/` → 4. `gw/etc/`

### Frontend Stack

Bootstrap 5.3.8 (no jQuery), Tom Select, DataTables, Chart.js 4.5.1
(UMD, not ESM), Font Awesome. CSS/JS assets in `hd/etc/css/` and
`hd/etc/js/` use template wrappers (`css.txt`, `js.txt`) for
conditional includes; page JS belongs there, not inline in
`perso.txt`. GeneWeb's own scripts are served as sources, never
minified: gwd sends their precompressed `.br`/`.gz` variants. Only
third-party libraries are shipped as `.min.js`.
Prefer template + CSS over JS that computes or toggles classes.

### Best Practices

- **URL construction**: use `%url_set;key=value`, not the legacy
  `%foreach;env_binding;` loops. Separate parameters with `&`, not
  `;`. Every mode taking a person must accept both the index (`i=`)
  and the key (`p=`, `n=`, `oc=`); do not convert keys to indexes,
  which do not survive a rebuild of the base.
- **Styles**: a class in `css.css` rather than `style="…"`; vertical
  spacing with `mt-*`, not `mb-*`; a literal U+00A0, not `&nbsp;`.
- **Roglo**: `%if;roglo;` (base_env `roglo=yes`) only for what is
  really specific to Roglo; an improvement for all bases is not
  guarded. Grey out (`disabled`) an unwanted option rather than hide
  it.
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
handling for German grammar; entries with arguments must allow for
it. No HTML markup in the lexicon beyond a little inline text: the
markup stays in the template, the variants in stacked entries. Remove
the entries a change leaves unused, and translate new ones into as
many languages as possible. Messages to users avoid internal jargon
(executable names, format magics); technical detail goes to the log.

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
`Name.strip_lower` → all phonetic. Variants are generated at query
time only: never canonicalize stored data. Distinct apostrophes
(U+0027, U+2019, U+02BC, U+02BB) are distinct in keys.

The index of the dictionaries (`base.inx`/`base.dat`, written by
`Outbase.output`, module `BaseIndex`) gives each of the 12
dictionaries of `Dict` (first names, surnames, places, sources,
titles, estates…) sorted by key, with the persons and families using
each entry:
- `Driver.dict_size`, `dict_groups`, `dict_entries`, `dict_records`,
  `dict_count`, `dict_select`, and `Driver.persons_of_surname` /
  `persons_of_first_name` / `spi_count`.
- Keys are compared byte-wise: surnames and estates without their
  particle, places without their suburb, other strings as is. It is no
  case- or accent-insensitive index: `Driver.base_strings_of_*` still
  read `names.inx` (crushed names in hash buckets, which hold
  unrelated names: filter them before loading their persons).
- Corrected by the committed patches when read, kept per process
  until the files change, reset after `commit_patches`; pending
  patches are ignored.
- A base of an earlier format has no index and is read-only: guard
  with `Driver.base_format base = Driver.current_base_format` (or use
  `dict_select`, which falls back to all records); gwd shows such a
  base's welcome page instead of the pages built on the index.

**Never use `Collection.iter` over all persons to find those using a
string** — it is O(n) over the entire database; read their records
from the index.

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

## Database Format and Patches

- Format `GnWb0026` (`head -c8 base`). `gwc -reindex <base>` writes
  the index alone and upgrades `GnWb0025` in place; older formats
  are read-only until exported with `gwu` and rebuilt with `gwc`.
  A format change needs a visible warning in `CHANGES.md`.
- `patch_name` (`lib/db/database.ml`) writes into the name table of
  the committed patches before any commit: never share the patches
  read by a request with other requests.
- `Driver.insert_person` reads the ascendants of the person: in tests
  use `Driver.insert_person_with_union_and_ascendants`.
- `My_gzip.gzip_string` writes the time in the gzip header: the same
  content gives a different `.gz` hash at each run.

## Commits

English, imperative, prefixed by the component (`search: …`),
the fix first, then the rationale. A deprecated option or a
user-visible change goes into `CHANGES.md`; never edit the entries of
past releases. Correct a series in progress by amending the commits
concerned, each staying buildable, rather than adding fix-up commits.
