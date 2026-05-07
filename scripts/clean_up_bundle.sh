#!/usr/bin/env bash
set -euo pipefail

# Clean up an OCaml installation directory after `dune install`
# Removes development artifacts and keeps only what’s needed
# for a binary distribution (executables, .cmxs, docs, etc.)

QUIET=""

# Parse options
if [ "${1:-}" = "--quiet" ]; then
  QUIET=true
  shift
fi

if [ -n "$QUIET" ]; then
  FIND_DELETE=(-delete)
else
  FIND_DELETE=(-print -delete)
fi

if [ $# -ne 1 ]; then
  echo "Usage: $0 [--quiet] <install-dir>" >&2
  exit 1
fi

INSTALL_DIR=$1

if [ ! -d "$INSTALL_DIR" ]; then
  echo "Error: directory '$INSTALL_DIR' does not exist" >&2
  exit 1
fi

if [ -n "$QUIET" ]; then
  echo "Cleaning up install directory: $INSTALL_DIR"
fi

# Patterns of files to remove
TO_REMOVE=(
  "*.ml"
  "*.mli"
  "*.cmi"
  "*.cmo"
  "*.cmx"
  "*.cmxa"
  "*.cma"
  "*.a"
  "*.cmt"
  "*.cmti"
  "dune-package"
  "opam"
  "*.opam"
  "*.mld"
)

# --- Remove Unwanted files ---
for pattern in "${TO_REMOVE[@]}"; do
  find "$INSTALL_DIR" -type f -name "$pattern" "${FIND_DELETE[@]}"
done

# --- Remove empty directories (after cleanup) ---
find "$INSTALL_DIR" -type d -empty "${FIND_DELETE[@]}"

if [ -n "$QUIET" ]; then
  echo "Cleanup complete!"
fi
