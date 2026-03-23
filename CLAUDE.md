# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GeneWeb is an open-source genealogy software written in OCaml with a web interface. It handles millions of individuals in production deployments (e.g., Roglo.eu with 11M+ individuals).

## Build Commands

```bash
# Configure (required first step - detects OS, generates Makefile.config)
ocaml ./configure.ml

# Build everything
make build

# Build and create distribution
make distrib

# Build only gwd/gwc executables (faster for development)
make gwd

# Format code (ocamlformat 0.28.1)
make fmt

# Generate API documentation
make doc
```

## Testing

```bash
# Run all tests
make test

# Run tests in CI mode (skips known failures)
make ci
```

Test framework: Alcotest and QCheck. Tests are in `/test/`.

## Architecture

### Main Directories

- **`/lib/`** - Core libraries (~53,600 LOC)
  - `core/` - Core genealogy logic (consanguinity calculations, etc.)
  - `db/` - Database layer (storage, AVL trees, collections)
  - `def/` - Data definitions (person, family structures)
  - `util/` - Utilities (hashing, formatting)
  - `templ/` - Custom template engine
  - `wserver/` - Web server layer
  - `plugin/` - Plugin system

- **`/bin/`** - 17 executables
  - `gwd/` - Main web server daemon
  - `gwc/` - Database creation from gw and gwo files
  - `gwb2ged/`, `ged2gwb/` - GEDCOM converters
  - `fixbase/` - Database repair utility

- **`/plugins/`** - 11 plugin modules (export, forum, jingoo, xhtml, etc.)

- **`/hd/`** - Web UI resources (templates, CSS, JS, i18n)

- **`/rpc/`** - RPC support (geneweb-rpc package with Lwt, httpun)

## OCaml Version

Requires OCaml 4.10+. Tested with 4.10.2, 4.14.3, and 5.4.1.

## Code Style

- Run `make fmt` before committing (uses ocamlformat 0.28.1)

## Contributing Rules

1. Never disable compiler warnings
2. Never remove code in `test/` directory - you can add tests
3. Never use the `Obj` module from OCaml stdlib
4. Don't use global mutable state (global references) - prefer passing data as arguments
5. Don't use `List.nth`, `List.concat`, or `@` from OCaml stdlib
6. Always run check formatting before creating commits
