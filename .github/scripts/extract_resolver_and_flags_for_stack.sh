#!/usr/bin/env bash

set -euo pipefail

CABAL_FILE="${CABAL_FILE:?Environment variable CABAL_FILE is required}"

readarray -t EXP_FLAGS < <(awk -f ./.github/script_helpers/extract_flags.awk "$CABAL_FILE")

echo "extracting experimental flags and setting them to true."
echo "if this fails, please verify all flags in package.yaml are set to true/false in stack.yaml"
for flag in "${EXP_FLAGS[@]}"; do
  pattern="^( *${flag})[[:space:]]*:[[:space:]]*(true|false)"
  grep -qE "${pattern}" stack.yaml
  sed -i -E "s/${pattern}/\1: true/g" stack.yaml
done

echo "contents of stack.yaml file:"
cat stack.yaml

RESOLVER=$(grep -E '^(snapshot|resolver):' stack.yaml | awk '{ print $2 }')

printf "\nchose resolver %s" "$RESOLVER"
echo "resolver=$RESOLVER" >>"$GITHUB_OUTPUT"
