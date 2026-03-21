# CLAUDE.md

This file provides guidance to AI coding assistants working on the
GeneWeb repository. It documents architecture, conventions, and common
pitfalls — not personal workflows or opinions.

## Project Overview

GeneWeb is an open-source genealogy web application written in OCaml,
started in late 1997 by Daniel de Rauglaudre (DDR) at INRIA. It
serves databases with millions of individuals (Roglo.eu: 11M+ records
with 300 wizards/hundred of thousands of gwplus databases on Geneanet).
Version 7.1 (beta2, December 2025).

Brief history: DDR (1997–2007) v1-5, Geneanet/Maufred v6 (2010–2014),
community volunteers (2014–2019) v7a, Geneanet repository takeover
(2019–2024) v7b, community access restored late 2024 v7b2. The Geneanet
fork ("gnt" branch) has diverged; database format compatibility must be
preserved.

## Build Commands

```bash
# Install dependencies
opam install . --deps-only

# Configure (required before first build)
ocaml ./configure.ml
# Optional: --sosa-zarith for arbitrary-precision Sosa numbers
# (slightly faster for large databases per benchmarks)
ocaml ./configure.ml --sosa-zarith

# Build
make build              # full build
make gwd                # gwd + gwc only (fastest iteration)
make distrib            # release distribution with all binaries

# Test
make test               # full test suite
make ci                 # CI mode (sets GENEWEB_CI=on)
make bench              # benchmarks

# Code quality
make fmt                # auto-format (ocamlformat 0.28.1, pinned)
dune build @fmt         # check formatting without modifying
```

Run locally after `make distrib`:
```bash
cd distribution && gw/gwd -bd bases -p 2317
```

## Architecture

### Directory Layout

- **`lib/`** — Core library (~128 modules)
  - `def/` — Data definitions (`person`, `family`, genealogical types)
  - `db/` — Database abstraction layer (Driver API, Collections)
  - `core/` — Core algorithms (consanguinity)
  - `sosa/` — Sosa numbering (legacy or Zarith-based)
  - `search/` — Search functionality (DDR phonetic, n-gram)
  - `templ/` — Template engine (parser, lexer, AST)
  - `wserver/` — Built-in HTTP server
  - `util/` — Shared utilities (dates, calendar, HTML escaping)
  - `json_export/` — JSON export for web APIs
- **`bin/`** — 17 executables
  - `gwd/` — Main web server daemon
  - `gwc/` — Database compiler
  - `ged2gwb/` / `gwb2ged/` — GEDCOM import/export
  - `consang/` — Consanguinity calculator
  - `fixbase/` — Database repair
- **`plugins/`** — Plugin system (export, forum, jingoo, xhtml, etc.)
- **`rpc/`** — RPC package (Lwt-based, httpun, js_of_ocaml client)
- **`hd/`** — Web UI resources
  - `etc/` — HTML templates (`.txt` files)
  - `etc/css/` — CSS files, served via `css.txt` template wrapper
  - `etc/js/` — JS files, served via `js.txt` template wrapper
  - `lang/lexicon.txt` — Localization strings
- **`test/`** — Tests (Alcotest unit, QCheck property-based, cram)

### Module Roles (avoid misplacing code)

- `perso.ml` — Person page rendering. Already very large (~4000 LOC).
  Do not add unrelated functionality here.
- `util.ml` — Shared utilities. Already a catch-all. Prefer creating
  focused modules (e.g., `dateDisplay.ml`, `placeDisplay.ml`) over
  adding more functions here.
- `templ.ml` — Template engine runtime. Changes here affect every page.

(Non-exhaustive — many other modules exist. Read surrounding code
before deciding where to place new functionality.)

## Contributing

1. Discuss changes via issue or email before submitting.
2. Run `make fmt` before every commit.
3. Keep PRs focused — one concern per PR.
4. Rejection triggers: unformatted code, copy-paste duplication,
   unnecessary computation, commented-out code, oversized PRs,
   hardcoded English strings.
5. Squash related commits. Write clear English commit messages
   documenting root cause, symptoms, and fix.

## OCaml Conventions

### Requirements
- Supported platforms: Linux, macOS, and Windows (Cygwin/mingw64).
- OCaml >= 4.10. Tested with 4.10.2, 4.14.x, and 5.x.
- Dune >= 3.6. Packages managed with opam.
- Code must pass `ocamlformat` 0.28.1 — PRs rejected otherwise.

### Rules
1. Never disable compiler warnings.
2. Never remove code in `test/` — you can add tests.
3. Never use the `Obj` module.
4. **No global mutable state.** Do not use `ref` at module level.
   Pass data as function arguments. Global references make code
   unreadable, non-parallelizable, and hard to evolve. If you see an
   existing global `ref`, that is legacy debt — do not add more.
5. Do not use `List.nth`, `List.concat`, or `@` (quadratic). Use
   `Array` access or accumulator patterns.
6. `Driver.Iper` / `Driver.Ifam` are abstract types. Use
   `Driver.Iper.hash` for int conversion, not `Obj.magic`.
7. `Array.find_map` requires OCaml >= 4.13. For 4.10 compatibility
   use `Mutil.array_find_map`.
8. `aux.ml` is a reserved Windows device name — do not use it as a
   filename (causes CI failures on Windows/Cygwin).
9. Format: 80-column width for `.ml`/`.mli` files. Templates (`.txt`)
   may extend to 120-150 columns.
10. No trailing spaces. No commented-out code.
11. Short English comments for `.mli` signatures. No comments in
    `.ml` implementations unless essential.

## Localization

**Never hardcode English strings** in templates or OCaml source.
All user-visible text must use the lexicon (`hd/lang/lexicon.txt`).

Lexicon format: 4 spaces before the key line, then each translation
on its own line with the language code flush left, `:`, space,
translation:
```
    died at age %s
fr: décédé à l'âge de %s
en: died at age %s
de: gestorben im Alter von %s
```

Stacked translations use `/` to separate variants (selected by index):
```
    witness/witnesses
fr: témoin/témoins
en: witness/witnesses
```
In templates: `[witness/witnesses]0` gives "witness",
`[witness/witnesses]1` gives "witnesses".

In templates: `[key]` or `[*key]` (capitalized). In OCaml:
`Util.transl conf "key"` or `Util.transl_nth conf "key/variant" n`.
`%s` placeholders are filled positionally by `Util.cftransl`.

Guidelines:
- Avoid duplicating existing entries. Search the lexicon before adding.
- Prefer creating a long specific entry for a particular need over
  concatenating shorter existing entries — word order and grammar
  differ across languages, making concatenation unreliable.
- The lexicon supports a declension system for Slavic languages
  (case-dependent noun forms) and specific handling for German
  grammar particularities. Translations for these languages may
  carry additional variants.

Hardcoded English strings block upstream acceptance and create
localization debt across 20+ supported languages.

## Template Engine

### Overview

Templates are `.txt` files in `hd/etc/`. The engine is in
`lib/templ/` (parser/lexer) and `lib/templ.ml` (runtime). Templates
use a custom syntax — not Jinja, not Mustache.

### Key Syntax

- `%variable;` — Variable evaluation (semicolon required).
- `%if;(condition)...%elseif;...%else;...%end;` — Conditionals.
- `%foreach;iterator;...%end;` — Loops.
- `%apply;func(arg1, arg2)` — Function application (short form).
  Equivalent long form: `%apply;func%with;arg1%and;arg2%end;` — the
  long form reads better when arguments are complex expressions.
- `%define;func(params)...%end;` — Function definition.
- `%let;name;value%in;` — Let binding.
- `%include;filename` — Include (resolved at parse time, see below).
- `[text]` — Lexicon lookup (localized string).
- `[*text]` — Lexicon lookup, first letter capitalized.
- `%sq;` — Strip following whitespace.
- `%nn;` — Strip surrounding newline.
- `%(comment%)` — Comment (not emitted).
- `%for;i;min;max;...%end;` — Numeric for loop (rare but functional).
- `%count;` / `%incr_count;` / `%reset_count;` — Built-in counter,
  widely used in template algorithms for numbering iterations.
- `%empty_sorted_list;` / `%add_in_sorted_list;value` /
  `%foreach;sorted_list_item;...%end;` — Accumulate and iterate a
  sorted list within templates.

### Operators in Conditions

Standard: `=`, `!=`, `<`, `<=`, `>`, `>=`, `and`, `or`, `not`.
Special: `|` (integer division), `%` (modulo — note this is also
the template escape character, context-dependent), `in` (substring
test: `%if;("x" in "y")` checks if `x` is contained in `y`).

### `%include;` is Parse-Time AST Inlining

`%include;filename` is resolved at **parse time** by the lexer. The
included file's AST is inlined into the parent's AST (and cached).
It is **not** a runtime file load. Consequences:

- Variables defined in the parent are visible in the included file.
- The included file shares the parent's evaluation environment.
- Circular includes will loop at parse time.

### Critical: `%` and `[` Escaping in Inline Script Blocks

The template parser interprets `%` and `[` **everywhere**, including
inside inline `<script>` blocks within templates. External `.js` files
are not affected. Escape sequences:

- `%%` emits a literal `%`.
- `%[` emits a literal `[`.
- `%]` emits a literal `]`.

In practice, for complex JS expressions, `\u0025` (Unicode escape for
`%`) and `\u005b` (for `[`) avoid ambiguity. Alternatively, use sed
post-processing with placeholder tokens.

This is the #1 source of template bugs for new contributors.

### Frontend Stack

- Bootstrap 4.6 (CSS + JS)
- jQuery (latest)
- Chart.js 4.5.1 (UMD build required — not ESM)
- Font Awesome (icons)
- Leaflet 2.0 alpha (maps)

CSS and JS assets live in `hd/etc/css/` and `hd/etc/js/` respectively.
Each has a template wrapper (`css.txt`, `js.txt`) so the template
engine's conditional syntax can be used inside them (e.g., to
conditionally include styles or scripts based on configuration).

After modifying any `.js` file, regenerate the `.min.js` copy
(Terser).

### Template File Resolution Order

For a request with template `templx` on base `mybase`:
1. `bases/etc/mybase/templx/`
2. `bases/etc/mybase/`
3. `gw/etc/templx/`
4. `gw/etc/`

The `template=` base config variable controls which templates are
allowed (`*` = all).

### Template Best Practices

- **URL construction**: do not use `%foreach;env_binding;` loops to
  build URLs — this is a legacy pattern. Use `%url_set;key=value`
  which is more concise and readable.
- **Separation of concerns**: keep HTML, CSS, and JS as disentangled
  as possible. Avoid catch-all template files (the template
  equivalent of `util.ml`/`perso.ml`). Isolate responsibilities into
  focused sub-templates via `%include;`.
- **W3C validation**: run templates through the W3C HTML validator
  when possible. Not mandatory but helps catch structural issues.
- **Browser console**: monitor the developer console for errors and
  warnings. The debug icon (timer/hourglass) in GeneWeb's footer
  shows query latency — watch for regressions when adding new
  template logic.

## Calendar / Date System

This is a major source of bugs. The core rule:

**`Dgreg(dmy, cal)` stores `dmy` already converted to Gregorian for
complete dates.** The `cal` field is display metadata only — it tells
the rendering layer which calendar to convert back to for display.

- When constructing a `Dgreg` with a complete date (day > 0,
  month > 0), always use the Gregorian-converted `dmy` values.
- For partial dates (day=0 or month=0), SDN conversion is impossible.
  The `dmy` fields store the original calendar's values, and `cal`
  is needed for correct interpretation.
- Use `Date.cdate_to_dmy_opt` to extract `dmy` from compressed dates.
  Use `Date.cdate_to_gregorian_dmy_opt` when you need guaranteed
  Gregorian values even for partial non-Gregorian dates.
- `Calendar.gregorian_of_julian`, `Calendar.gregorian_of_french`,
  `Calendar.gregorian_of_hebrew` convert to Gregorian. The reverse
  functions convert back for display.

Ignoring this invariant produces calendar bugs that only manifest
with Julian/French Republican/Hebrew dates — easy to miss in testing.

## Search Architecture

The search system (introduced by DDR — Daniel de Rauglaudre) uses
a 4-level fallback: `Name.crush_lower` (phonetic) →
`Name.lower` (exact) → `Name.strip_lower` → all phonetic.

Index files (`fnames.inx`/`fnames.dat`, `snames.inx`/`snames.dat`)
provide O(log n) access. **Never use `Collection.iter` for search** —
it is O(n) over the entire database. Always go through the index.

Firstname aliases are stored in `names.inx`. Surname aliases are a
work in progress.

## Wiki / Notes Syntax (TLSW)

The wiki markup engine is in `lib/wiki.ml` (rendering) and
`lib/notesLinks.ml` (link parsing).

Key syntaxes:
- `[[first/last]]` or `[[First Last]]` — Person link.
- `[[first/last/oc]]` — Person link with occurrence number.
- `[[[page/text]]]` — Wiki page link.
- `[[image:path/alt/width]]` — Inline image.
- `{{image.jpg|left|200px|Caption}}` — Floated image with caption.

Images are served via `m=IM&s=filename` (binary), not `m=SRC`.
Image files live in `bases/src/{dbname}/images/`.

HTML in notes is sanitized by `safe_html` (`lib/util.ml`). Only tags
listed in `default_safe_html_allowed_tags` are preserved; others are
replaced by empty comments. All `on*` event attributes are stripped.

## Database Iteration API

```ocaml
Driver.persons  : base -> person Collection.t
Driver.families : ?select:(family -> bool) -> base -> family Collection.t
Collection.fold : ?from:int -> ?until:int ->
  ('a -> 'b -> 'a) -> 'a -> 'b Collection.t -> 'a
Collection.iter : ('a -> unit) -> 'a Collection.t -> unit
Driver.nb_of_persons      : base -> int
Driver.nb_of_families     : base -> int
Driver.nb_of_real_persons : base -> int
```

Privacy: always check `Util.authorized_age conf base p` before
exposing date-based information. Counts may include everyone; dated
details must respect privacy settings.

## Common Pitfalls

1. **Hardcoding English** instead of using the lexicon.
2. **Adding code to `util.ml` or `perso.ml`** when a focused module
   would be more appropriate.
3. **Template variable shadowing** between `perso.ml` `eval_var` and
   `templ.ml` built-in variables — check both before adding a new
   variable name.
4. **Forgetting `.min.js`** after editing JavaScript files.
5. **Using `%` or `[` literally in inline `<script>` blocks** inside
   templates — escape with `%%`, `%[`, `%]` (see Escaping section).
6. **Ignoring the calendar invariant** (`Dgreg` stores Gregorian).
7. **O(n) search via `Collection.iter`** instead of index-based
   lookup.
8. **Global mutable state** — never introduce new `ref` at module
   scope.
9. **`aux.ml` as filename** — reserved on Windows, breaks CI.
10. **Using `%foreach;env_binding;` to build URLs** — use
    `%url_set;` instead.
11. **Concatenating lexicon entries** to form sentences — word order
    varies across languages; create a dedicated entry instead.