#!/usr/bin/env bash
set -eu

FILE="$1"

patch_file () {
  if [ -n "$2" ]; then
    escaped="$(echo "$2" | sed -e 's/[\/&]/\\&/g')"
    sed -e "s/let $1 = \".*\"/let $1 = \"$escaped\"/"
  else
    cat
  fi
}

VERSION=$(git describe --always --dirty --abbrev=7 || true)
SOURCE=$(git remote get-url origin | sed -n 's|^.*github.com.\([^/]\+/[^/.]\+\)\(.git\)\?|\1|p' || true)
BRANCH=$(git symbolic-ref --quiet --short HEAD || git branch -r --contains HEAD | head -n1 | tr -d ' ' || true)
COMMIT_ID=$(git rev-parse --short HEAD || true)
COMMIT_DATE=$(git show -s --date=short --pretty=format:'%cd' || true)
COMPIL_DATE=$(date +'%Y-%m-%d')

cat "$FILE" \
  | patch_file "ver" "$VERSION" \
  | patch_file "src" "$SOURCE" \
  | patch_file "branch" "$BRANCH" "$FILE" \
  | patch_file "commit_id" "$COMMIT_ID" "$FILE" \
  | patch_file "commit_date" "$COMMIT_DATE" "$FILE" \
  | patch_file "compil_date" "$COMPIL_DATE" "$FILE"
