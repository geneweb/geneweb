#!/usr/bin/env bash
set -euo pipefail

HOST=$(echo "${RUNNER_OS:-host}" | tr '[:upper:]' '[:lower:]')
ARCH=$(echo "${RUNNER_ARCH:-arch}" | tr '[:upper:]' '[:lower:]')
LINKING_MODE="${LINKING_MODE:-dynamic}"

# FIXME: The current implementation of oui doesn't bundle plugin dependencies
# that must be loaded at runtime.

# ocamlfind outputs CRLF on Windows by design. See issue
# https://github.com/ocaml/ocamlfind/issues/70
ocamlfind_query () {
  ocamlfind query $1 | tr -d "\r\n"
}

# The ppx_deriving library is used by forum plugin.
bundle_ppx_deriving () {
  local src="$(ocamlfind_query ppx_deriving)"
  local dst="$1/lib/ppx_deriving"
  install -d "$dst/runtime"
  install "$src/META" "$dst"
  install "$src/runtime/ppx_deriving_runtime.cmxs" "$dst/runtime/"
}

# The jingoo library is used by gwxjg plugin.
bundle_jingoo () {
  local src="$(ocamlfind_query jingoo)"
  local dst="$1/lib/jingoo"
  install -d "$dst"
  install "$src/META" "$dst"
  install "$src/jingoo.cmxs" "$dst"
}

mkdir -p _build
BUNDLE_DIR=$(mktemp -d -p _build)
trap 'rm -Rf "$BUNDLE_DIR"' EXIT
echo "$BUNDLE_DIR"

dune clean
dune build --release @install @runtest
dune install --relocatable --prefix "$BUNDLE_DIR"
bundle_ppx_deriving "$BUNDLE_DIR"
bundle_jingoo "$BUNDLE_DIR"
./scripts/clean_up_bundle.sh "$BUNDLE_DIR"
oui lint geneweb.json "$BUNDLE_DIR"
oui build geneweb.json "$BUNDLE_DIR"

# FIXME: oui doesn't allow to rename the artifact through the CLI
PATTERN=geneweb-*.*.*.*.*
for file in $PATTERN; do
  ext="${file##*.}"
  name="geneweb-$HOST-$ARCH-$TAG-$LINKING_MODE.$ext"
  mv "$file" "$name"
  echo "$name generated"
done
