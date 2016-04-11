#!/bin/bash -e

usage() {
  PROG_NAME=${0##*/}
  (echo "Usage  : $PROG_NAME <options> [<targets>]"
   echo "Usage 2: $PROG_NAME -graph <options> [<targets>]"
   echo
   echo "General options:"
   echo "  -I <incdir>         consider <incdir>/.depend file"
   echo "  -filter <pattern>   filter files according to <pattern>"
   echo "  -noext              remove extensions of files"
   echo
   echo "Non-graph option:"
   echo "  -varname <varname>  if specified write '<varname> :=' before each line (%s will be replaced by the name of the target)"
  ) >&2
  exit 1
}

INCDIRS=
TARGETS=
GRAPH=false
NOEXT=false
FILTER=
VARNAME=
while [ -n "$1" ]; do
  case $1 in
  -h|--help) usage;;
  -graph) GRAPH=true;;
  -noext) NOEXT=true;;
  -filter)
    shift
    [ -n "$1" ] && [ -z "$FILTER" ] || usage
    FILTER="$1";;
  -varname)
    shift
    [ -n "$1" ] && [ -z "$VARNAME" ] || usage
    VARNAME="$1";;
  -I) shift
      [ -n "$1" ] || usage
      INCDIRS+="${1%/} ";;
  *) TARGETS+="$1 ";;
  esac
  shift
done

DEPEND=".depend"

# Collect all dependencies as an associative array
declare -A DEPS

concatpath() {
# concatpath "A/B" "../C/D" gives "A/C/D"
  local pre="$1" post="$2"
  while [ -n "$pre" ] && [[ $post =~ ^\.\.\/ ]]; do
    pre=$(dirname $pre)
    [ "$pre" != "." ] || pre=
    post=${post:3}
  done
  echo "${pre%/}${pre:+/}$post"
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
    for rhs in ${words[1]}; do
      [ -z "$FILTER" ] || [[ $rhs == $FILTER ]] || continue
      rhs=$(concatpath "$PREFIX" "$rhs")
      $NOEXT && rhs="${rhs%.*}"
      prefixed_rhs+="$rhs "
    done
    prefixed_rhs="${prefixed_rhs% }"
    [ -n "$prefixed_rhs" ] || return 0
    for lhs in ${words[0]}; do
      [ -z "$FILTER" ] || [[ $lhs == $FILTER ]] || continue
      lhs=$(concatpath "$PREFIX" "$lhs")
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

[ -n "$TARGETS" ] || TARGETS="${!DEPS[@]}"

if $GRAPH; then

  add() {
    if [[ ${INGRAPH[$1]} != y ]]; then
      INGRAPH[$1]=y
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

  declare -A INGRAPH

  echo "digraph {"

  for target in $TARGETS; do
    add "$target"
  done

  echo "}"

else
  # Compute the transitive closure of dependencies from $TARGET
  # + avoid infinite loops in case of cyclic dependencies
  # + do not add duplicates

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

  for target in $TARGETS; do
    declare -A INRES=([$target]=y) INSTACK=()
    RES=

    add "$target"

    [ -z "$VARNAME" ] || printf "$VARNAME := " "$target"
    echo "$RES"
  done
fi
