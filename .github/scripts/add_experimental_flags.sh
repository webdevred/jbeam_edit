#!/usr/bin/env bash

set -euo pipefail

if [[ -z "$OLD_MATRIX" ]]; then
  exit 1
fi

if [[ -z "$CABAL_FILE" || ! -f "$CABAL_FILE" ]]; then
  echo "Cabal file missing: $CABAL_FILE"
  exit 1
fi

echo "$OLD_MATRIX" >matrix.json

MATRIX_JSON=$(sed -E 's/^matrix=//' <matrix.json)

readarray -t EXP_FLAGS < <(awk -f ./.github/script_helpers/extract_flags.awk "$CABAL_FILE")

ORIGINAL=$(jq --arg label "stable" '.include[0] += {label: $label}' <<<"$MATRIX_JSON")

FLAGS=""
if [[ ${#EXP_FLAGS[@]} -eq 0 ]]; then
  UPDATED="$ORIGINAL"
else
  EXP_FLAGS_STRING=$(printf ' +%s ' "${EXP_FLAGS[@]}")
  EXPERIMENTAL=$(jq --arg flags "$EXP_FLAGS_STRING" --arg label "experimental" \
    '.include[0] += {flags: $flags, label: $label}' <<<"$MATRIX_JSON")

  UPDATED=$(jq --argjson exp_include "$(jq '.include' <<<"$EXPERIMENTAL")" \
    '.include += $exp_include' <<<"$ORIGINAL")
fi

echo "checking output matrix:"

echo "$UPDATED"

if echo "$EXP_FLAGS_STRING" | grep -q ' +transformation '; then
  echo "transformation=true" >>"$GITHUB_OUTPUT"
else
  echo "transformation=false" >>"$GITHUB_OUTPUT"
fi

printf "matrix=%s" "$(echo "$UPDATED" | jq -c .)" >>"$GITHUB_OUTPUT"
