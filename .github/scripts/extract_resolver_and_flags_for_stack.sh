#!/usr/bin/env bash

set -euo pipefail

CABAL_FILE="${CABAL_FILE:?Environment variable CABAL_FILE is required}"

readarray -t STABLE_FLAGS < <(awk -f ./.github/script_helpers/extract_flags.awk "$CABAL_FILE")
readarray -t EXP_FLAGS < <(awk -v EXPERIMENTAL_ONLY=1 -f ./.github/script_helpers/extract_flags.awk "$CABAL_FILE")

echo "extracting experimental flags and setting them to true."
echo "if this fails, please verify all flags in package.yaml are set to true/false in stack.yaml"

update_flags() {
  local flags=("$@")
  for flag in "${flags[@]}"; do
    local pattern="^( *${flag})[[:space:]]*:[[:space:]]*(true|false)"
    if grep -qE "${pattern}" stack.yaml; then
      sed -i -E "s/${pattern}/\1: true/g" stack.yaml
    fi
  done
}

update_flags "${STABLE_FLAGS[@]}"
update_flags "${EXP_FLAGS[@]}"

echo "contents of stack.yaml file:"
cat stack.yaml

RESOLVER=$(grep -E '^(snapshot|resolver):' stack.yaml | awk '{ print $2 }')

printf "\nchose resolver %s" "$RESOLVER"
echo "resolver=$RESOLVER" >>"$GITHUB_OUTPUT"
