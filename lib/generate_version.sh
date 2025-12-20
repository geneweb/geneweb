#!/usr/bin/env bash
set -eu
FILE="$1"

patch_file () {
  if [ -n "${2:-}" ]; then
    escaped="$(echo "$2" | sed -e 's/[\/&]/\\&/g')"
    sed -e "s/let $1 = \".*\"/let $1 = \"$escaped\"/"
  else
    sed -e "s/let $1 = \"%.*%\"/let $1 = \"\"/"
  fi
}

COMMIT_VER=$(git describe --always --dirty --abbrev=7 2>/dev/null || echo "")
SOURCE=$(git remote get-url origin 2>/dev/null | sed -n 's|^.*github.com[:/]\([^/]\+/[^/.]\+\)\(\.git\)\?$|\1|p' || echo "")
BRANCH=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || git branch -r --contains HEAD 2>/dev/null | head -n1 | tr -d ' ' || echo "")
COMMIT_ID=$(git rev-parse --short HEAD 2>/dev/null || echo "")
COMMIT_DATE=$(git show -s --date=short --pretty=format:'%cd' 2>/dev/null || echo "")
COMPIL_DATE=$(date +'%Y-%m-%d')

cat "$FILE" \
  | patch_file "src" "$SOURCE" \
  | patch_file "branch" "$BRANCH" \
  | patch_file "commit_ver" "$COMMIT_VER" \
  | patch_file "commit_id" "$COMMIT_ID" \
  | patch_file "commit_date" "$COMMIT_DATE" \
  | patch_file "compil_date" "$COMPIL_DATE"