#!/bin/sh
set -eu

SOURCE=$(git remote get-url origin | sed -n 's|^.*github.com.\([^/]\+/[^/.]\+\)\(.git\)\?|\1|p')
BRANCH=$(git symbolic-ref --quiet --short HEAD || git branch -r --contains HEAD | head -n1 | tr -d ' ')
COMMIT_ID=$(git rev-parse --short HEAD)
COMMIT_DATE=$(git show -s --date=short --pretty=format:'%cd')
COMPIL_DATE=$(date +'%Y-%m-%d')

cat <<EOF
(* Copyright (c) 1998-2007 INRIA *)

let ver = "7.1-beta"

let available_languages =
  [
    "af";
    "ar";
    "bg";
    "br";
    "ca";
    "co";
    "cs";
    "da";
    "de";
    "en";
    "eo";
    "es";
    "et";
    "fi";
    "fr";
    "he";
    "is";
    "it";
    "lt";
    "lv";
    "nl";
    "no";
    "oc";
    "pl";
    "pt";
    "pt-br";
    "ro";
    "ru";
    "sk";
    "sl";
    "sv";
    "tr";
    "zh";
  ]

let branch = "$BRANCH\n"
let src = "$SOURCE\n"
let commit_id = "$COMMIT_ID\n"
let commit_date = "$COMMIT_DATE\n"
let compil_date = "$COMPIL_DATE\n"
EOF
