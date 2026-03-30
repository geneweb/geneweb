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

VER_SHORT=$(sed -n 's/^let ver = "\(.*\)"$/\1/p' "$FILE")
VER=$(git describe --always --dirty --abbrev=7 2>/dev/null | sed 's/^v//' || echo "")
# Find the remote for the current tracking branch; fall back to "origin".
_tracking_remote=$(git rev-parse --abbrev-ref --symbolic-full-name @{upstream} 2>/dev/null \
  | sed -nE 's|^([^/]+)/.*|\1|p') || true
_remote=${_tracking_remote:-origin}
SOURCE=$(git remote get-url "$_remote" 2>/dev/null \
  | sed -nE 's|^.*github.com[:/]([^/]+/[^/.]+)(.git)?$|\1|p' || echo "")
BRANCH=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || git branch -r --contains HEAD 2>/dev/null | head -n1 | tr -d ' ' || echo "")
COMMIT_ID=$(git rev-parse --short HEAD 2>/dev/null || echo "")
COMMIT_DATE=$(git show -s --date=short --pretty=format:'%cd' 2>/dev/null || echo "")
COMPIL_DATE=$(date +'%Y-%m-%d')

cat "$FILE" \
  | patch_file "ver_short" "$VER_SHORT" \
  | patch_file "ver" "$VER" \
  | patch_file "src" "$SOURCE" \
  | patch_file "branch" "$BRANCH" \
  | patch_file "commit_id" "$COMMIT_ID" \
  | patch_file "commit_date" "$COMMIT_DATE" \
  | patch_file "compil_date" "$COMPIL_DATE"