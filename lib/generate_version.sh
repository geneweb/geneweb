#!/usr/bin/env bash
set -eu
FILE="$1"

patch_file () {
  if [ -n "$2" ]; then
    escaped="$(echo "$2" | sed -e 's/[\/&]/\\&/g')"
    sed -e "s/let $1 = \"[^\"]*\"/let $1 = \"$escaped\"/"
  else
    cat
  fi
}

VERSION=$(git describe --always --dirty --abbrev=7 || true)
BRANCH=$(git symbolic-ref --quiet --short HEAD || git branch -r --contains HEAD | head -n1 | tr -d ' ' || true)
SOURCE=$(git remote get-url origin | sed 's|^.*github\.com/||; s|\.git$||' || true)
COMMIT_ID=$(git rev-parse --short HEAD || true)
COMMIT_DATE=$(git show -s --date=short --pretty=format:'%cd' || true)
COMPIL_DATE=$(date +'%Y-%m-%d')

cat "$FILE" \
  | patch_file "branch" "$BRANCH" \
  | patch_file "src" "$SOURCE" \
  | patch_file "commit_id" "$COMMIT_ID" \
  | patch_file "commit_date" "$COMMIT_DATE" \
  | patch_file "compil_date" "$COMPIL_DATE"
