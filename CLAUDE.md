# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GeneWeb is an open-source genealogy software written in OCaml with a web interface. It handles databases with millions of individuals (used by Roglo.eu with 11M+ records). Version 7.1 (beta).

## Build Commands

```bash
# Install dependencies
opam install . --deps-only

# Configure (run before first build)
ocaml ./configure.ml                              # basic
ocaml ./configure.ml --sosa-zarith --gwd-caching  # recommended (zarith + Unix caching)

# Build
make build              # full build
make build-geneweb      # libraries and binaries only
make gwd                # just gwd and gwc (fastest iteration)
make distrib            # release distribution with all binaries

# Test
make test               # full test suite
make ci                 # CI mode (skips known failures, sets GENEWEB_CI=on)
make bench              # benchmarks

# Code quality
make fmt                # auto-format with ocamlformat (0.28.1)
dune build @fmt         # check formatting without modifying

# Other
make doc                # generate API docs with odoc
make clean              # clean build artifacts
dune utop               # interactive REPL (requires make build first)
```

## Architecture

The codebase is organized in layers:

- **`lib/`** — Core library (~128 modules)
  - `def/` — Data definitions (person, family, genealogical relationships)
  - `db/` — Database abstraction layer
  - `core/` — Core algorithms (consanguinity computation)
  - `sosa/` — SOSA numbering system (legacy or zarith-based)
  - `search/` — Search functionality
  - `templ/` — Template engine (Jingoo-based)
  - `wserver/` — Built-in web server
  - `util/` — Shared utilities
  - `json_export/` — JSON export for web APIs
  - `ancient/` — Optional memory-efficient storage for large databases

- **`bin/`** — Executables (17 tools)
  - `gwd/` — Main web server daemon (ports 2316-2317)
  - `gwc/` — Genealogical data compiler
  - `ged2gwb/` / `gwb2ged/` — GEDCOM import/export
  - `consang/` — Consanguinity calculator
  - `fixbase/` — Database repair
  - `setup/` — Setup utility (gwsetup)

- **`plugins/`** — Plugin system (9 plugins: cgl, export, fixbase, forum, gwxjg, jingoo, lib_show, no_index, xhtml)

- **`rpc/`** — RPC support package (lwt-based async, httpun web server, js_of_ocaml client)

- **`hd/`** — HTML templates and static resources

- **`test/`** — Tests using alcotest (unit), qcheck (property-based), and cram tests

## Wiki/Notes Syntax (TLSW)

The wiki markup engine is in `lib/wiki.ml` (rendering) and `lib/notesLinks.ml` (link parsing). The `wiki_link` ADT has five variants: `WLpage`, `WLperson`, `WLwizard`, `WLimage`, `WLnone`.

Key syntaxes:
- `[[first/last]]` or `[[First Last]]` — person link (natural name syntax supported)
- `[[first/last/oc]]` or `[[First Last (oc)]]` — person link with occurrence
- `[[[page/text]]]` — wiki page link
- `[[w:wizard/name]]` — wizard link
- `[[image:path/alt/width]]` — inline image
- `{{image.jpg|left|200px|Image caption}}` — floated image with caption (align: `left`/`right`, width in px, generates `<figure>`)

Rendering pipeline for person notes: `get_note_or_source` → `html_of_tlsw` → `source_note_with_env` (calls `wiki_aux` → `html_of_tlsw` → `syntax_links` → `safe_html`).

Images are served via `m=IM&s=filename` (binary), not `m=SRC&v=` (which wraps in HTML). Image files live in `bases/src/{dbname}/images/`.

HTML tags must be in `default_safe_html_allowed_tags` (`lib/util.ml`) or `safe_html` strips them.

## macOS App Bundle

- `create_bundle.sh` — creates self-contained `geneweb.app` with dylibs bundled in `Contents/Frameworks/`, rpaths fixed, ad-hoc signed
- `create_dmg.sh` — creates distributable DMG from the app bundle
- User config at `~/.geneweb/config` sets `BASES_DIR`, `GWD_PORT`, `GENEWEB_LANG`
- The launcher (`Contents/MacOS/GeneWeb`) must stay alive via `wait` (not exit after spawning) or macOS shows "not responding"
- `CFBundleExecutable` in `Info.plist` must match the actual filename case in `Contents/MacOS/`

## Key Technical Details

- **OCaml >= 4.10**, built with **Dune >= 3.6**, packages managed with **OPAM**
- **camlp5** (>= 8.03) used for preprocessing
- Version info generated at build time from `lib/version.txt` via `generate_version.sh`
- CI matrix: Ubuntu/macOS/Windows × OCaml 4.10.2/4.14.2/5.4.0
- Code must pass `ocamlformat` (version 0.28.1, pinned) — PRs will be rejected otherwise
- `DUNE_PROFILE=dev` is used in CI

## PR Guidelines

- Discuss changes via issue/email before submitting
- Format with `make fmt` before submitting
- Rejection triggers: unformatted code, unnecessary computations, copy-paste programming, commented-out code, oversized PRs


## Nicholas Spies notes to CLAUDE

Make sure to update README.md OR other appropriate geneweb file(s) with the latest syntax that our changes have introduced, such as for inline links [[...]] and for images in the Notes field {{...}} and boldface of text display of ancestors and descendants.

When requested to run geneweb, always do so using the SPIES database. Give me instructions how I can run geneweb myself, always with the spies database. Always use the standard port Geneweb uses (2317).

Make sure that naming conventions follow those in the main geneweb github site and correct my geneweb fork files to conform; Pay attention to upper and lower case.

The /Applications/geneweb.app should be kept up to date with the latest changes.

Always record a complete transcript of each session in separate files by date; tell me where they are to be found

### Coding principles

- **Never hardcode English strings** in templates or source code. Always use the GeneWeb lexicon (`hd/lang/lexicon.txt`) for user-visible text. Hardcoded strings create localization debt and will block upstream PRs. If the needed lexicon entry doesn't exist, either propose adding it to the lexicon or defer the change.

### Build and test workflow

1. **Edit source** in `/Users/nspies/geneweb/` (git repo)
2. **Build runtime** with `make distrib` — this populates `distribution/` with binaries, templates, plugins, etc.
3. **Test locally** by running:
   ```bash
   cd distribution && gw/gwd -bd bases -p 2317
   ```
   Then open `http://localhost:2317/spies` in a browser.
4. The SPIES database lives at `bases/spies.gwb` in the repo root. A symlink at `distribution/bases/spies.gwb` points there so `make distrib` does not need to copy it.


