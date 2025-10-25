#!/usr/bin/env bash

set -euo pipefail

OLD_MATRIX="$1"
CABAL_FILE="$2"

if [[ -z "$OLD_MATRIX" || ! -f "$OLD_MATRIX" ]]; then
  exit 1
fi

if [[ -z "$CABAL_FILE" || ! -f "$CABAL_FILE" ]]; then
  echo "Cabal file missing: $CABAL_FILE"
  exit 1
fi

MATRIX_JSON=$(echo "$OLD_MATRIX" | sed -E 's/^matrix=//')

readarray -t EXP_FLAGS < <(awk -f ./.github/script_helpers/extract_flags.awk "$CABAL_FILE")

ORIGINAL=$(jq --arg label "stable" '.include[0] += {label: $label}' <<<"$MATRIX_JSON")

if [[ ${#EXP_FLAGS[@]} -eq 0 ]]; then
  UPDATED="$ORIGINAL"
  echo "flags=null" >>"$GITHUB_OUTPUT"
else
  EXP_FLAGS_STRING=$(printf ' +%s ' "${EXP_FLAGS[@]}")
  EXPERIMENTAL=$(jq --arg flags "$EXP_FLAGS_STRING" --arg label "experimental" \
    '.include[0] += {flags: $flags, label: $label}' <<<"$MATRIX_JSON")

  UPDATED=$(jq --argjson exp_include "$(jq '.include' <<<"$EXPERIMENTAL")" \
    '.include += $exp_include' <<<"$ORIGINAL")
  printf "flags=%s\n" "$EXP_FLAGS_STRING" >>"$GITHUB_OUTPUT"
fi

echo "checking output matrix:"

echo "$MATRIX"
printf "matrix=%s" "$(echo "$UPDATED" | jq -c .)" >>"$GITHUB_OUTPUT"
