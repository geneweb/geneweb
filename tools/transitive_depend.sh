#!/bin/bash -e

usage() {
  PROG_NAME=${0##*/}
  echo "Usage: $PROG_NAME [-simp] [-I incdir] <target>" >&2
  echo "Usage 2: $PROG_NAME -graph [-simp] [-noext] [-filter <pattern>] [-I incdir] [<target>]" >&2
  exit 1
}

INCDIRS=
TARGET=
GRAPH=false
SIMP=false
NOEXT=false
FILTER=
while [ -n "$1" ]; do
  case $1 in
  -h|--help) usage;;
  -graph) GRAPH=true;;
  -simp) SIMP=true;;
  -noext) NOEXT=true;;
  -filter)
    shift
    [ -n "$1" ] && [ -z "$FILTER" ] || usage
    FILTER="$1";;
  -I) shift
      [ -n "$1" ] || usage
      INCDIRS+="${1%/} ";;
  *) [ -z "$TARGET" ] || usage
     TARGET="$1";;
  esac
  shift
done

$GRAPH || [ -n "$TARGET" ] || usage

DEPEND=".depend"

# Collect all dependencies as an associative array
declare -A DEPS

simppath() {
# Simplify path by removing subdir/..
# It's an option because it's pretty slow
  local pc c b res=
  IFS='/' read -ra pc <<< "$1"
  for c in "${pc[@]}"; do
    [ -n "$c" ] && [ "$c" != "." ] || continue
    if [ -n "$res" ] && [ "$c" = ".." ]; then
      b=$(basename $res)
      if [ "$b" != ".." ]; then
        res=$(dirname $res)
        [ "$res" != "." ] || res=
        continue
      fi
    fi
    res="${res}${res:+/}$c"
  done
  [[ ! "$1" =~ ^\/ ]] || res="/${res}"
  echo "$res"
}

treat_line() {
  local PREFIX="$1"
  local LINE="$2"
  [ -n "$LINE" ] || return 0
  local words
  IFS=':' read -ra words <<< "$LINE"
  if [ "${#words[@]}" -gt 2 ]; then
    echo "Bad-formatted line: '$LINE'" >&2
    exit 1
  fi
  if [ "${#words[@]}" -eq 2 ]; then
    local rhs lhs prefixed_rhs=
    if $SIMP || [ -n "$FILTER" ]; then
      for rhs in ${words[1]}; do
        [ -z "$FILTER" ] || [[ $rhs == $FILTER ]] || continue
        rhs="$PREFIX$rhs"
        $SIMP && rhs=$(simppath "$rhs")
        $NOEXT && rhs="${rhs%.*}"
        prefixed_rhs+="$rhs "
      done
      prefixed_rhs="${prefixed_rhs% }"
    else
      rhs=(${words[1]})
      $NOEXT && rhs=("$(rhs[@]%.*)")
      prefixed_rhs="${rhs[@]/#/$PREFIX}"
    fi
    [ -n "$prefixed_rhs" ] || return 0
    for lhs in ${words[0]}; do
      [ -z "$FILTER" ] || [[ $lhs == $FILTER ]] || continue
      lhs="$PREFIX$lhs"
      $SIMP && lhs=$(simppath "$lhs")
      $NOEXT && lhs="${lhs%.*}"
      DEPS[$lhs]+="$prefixed_rhs "
    done
  fi
}

read_depend() {
  local PREFIX="$1"
  local line LINE=
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

$GRAPH || read_depend ""

for d in $INCDIRS; do
  read_depend "$d/"
done

declare -A INRES
declare -A INSTACK

if $GRAPH; then

  add() {
    if [[ ${INRES[$1]} != y ]]; then
      INRES[$1]=y
      if [ -z "${DEPS[$1]}" ]; then
        printf "\"$1\"\n"
      else
        printf "\"$1\" -> {"
        printf "\"%s\" " ${DEPS[$1]}
        printf "}\n"
      fi
      local dep
      for dep in ${DEPS[$1]}; do
        add "$dep"
      done
    fi
  }

  echo "digraph {"

  if [ -n "$TARGET" ]; then
    add "$TARGET"
  else
    for target in "${!DEPS[@]}"; do
      add "$target"
    done
  fi

  echo "}"

else
  # Compute the transitive closure of dependencies from $TARGET
  # + avoid infinite loops in case of cyclic dependencies
  # + do not add duplicates

  RES=

  add() {
    if [ -z "$1" ]; then
      echo "Empty target name" >&2
      exit 1
    fi
    if [[ ${INSTACK[$1]} != y ]]; then
      INSTACK[$1]=y
      local dep
      for dep in ${DEPS[$1]}; do
        add "$dep"
      done
      if [[ ${INRES[$1]} != y ]]; then
        INRES[$1]=y
        RES+="$1 "
      fi
    fi
  }

  INRES[$TARGET]=y
  add "$TARGET"

  echo "$RES"
fi
