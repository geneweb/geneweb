#!/bin/bash -e

usage() {
  PROG_NAME=${0##*/}
  echo "Usage: $PROG_NAME [-I incdir] <target>" >&2
  exit 1
}

INCDIRS=
TARGET=
while [ -n "$1" ]; do
  case $1 in
  -h|--help) usage;;
  -I) shift
      [ -n "$1" ] || usage
      INCDIRS+="$1 ";;
  *) [ -z "$TARGET" ] || usage
     TARGET="$1";;
  esac
  shift
done

[ -n "$TARGET" ] || usage

DEPEND=".depend"

# Collect all dependencies as an associative array
declare -A DEPS

treat_line() {
  PREFIX="$1"
  LINE="$2"
  [ -n "$LINE" ] || return 0
  IFS=':' read -ra words <<< "$LINE"
  if [ "${#words[@]}" -gt 2 ]; then
    echo "Bad-formatted line: '$LINE'" >&2
    exit 1
  fi
  if [ "${#words[@]}" -eq 2 ]; then
    rhs=(${words[1]})
    prefixed_rhs="${rhs[@]/#/$PREFIX}"
    for lhs in ${words[0]}; do
      DEPS[$PREFIX$lhs]+="$prefixed_rhs "
    done
  fi
}

read_depend() {
  PREFIX="$1"
  LINE=
  while read line; do
    if [[ $line =~ ^\# ]]; then continue; fi
    if [[ $line =~ \\$ ]]; then
      LINE+="${line:0:-1}"
    else
      LINE+="$line"
      treat_line "$PREFIX" "$LINE"
      LINE=
    fi
  done < "$PREFIX$DEPEND"
  treat_line "$PREFIX" "$LINE"
}

read_depend ""

for d in $INCDIRS; do
  read_depend "$d/"
done

# Compute the transitive closure of dependencies from $TARGET
# + avoid infinite loops in case of cyclic dependencies
# + do not add duplicates

declare -A INRES
declare -A INSTACK
RES=

add() {
  if [ -z "$1" ]; then
    echo "Empty target name" >&2
    exit 1
  fi
  if [[ ${INSTACK[$1]} != y ]]; then
    INSTACK[$1]=y
    for dep in ${DEPS[$1]}; do
      add "$dep"
    done
    if [[ ${INRES[$1]} != y ]]; then
      INRES[$1]=y
      RES="$RES $1"
    fi
  fi
}

INRES[$TARGET]=y
add "$TARGET"

echo "$RES"
