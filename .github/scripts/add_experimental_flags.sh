#!/usr/bin/env bash

set -euo pipefail

if [[ -z "$MATRIX" ]]; then
  echo "matrix mising"
  exit 1
fi

if [[ -z "$CABAL_FILE" || ! -f "$CABAL_FILE" ]]; then
  echo "Cabal file missing: $CABAL_FILE"
  exit 1
fi

MATRIX_JSON=$(sed -E 's/^matrix=//' <<<"$MATRIX")

readarray -t EXP_FLAGS < <(awk -f ./.github/script_helpers/extract_flags.awk "$CABAL_FILE")

ORIGINAL=$(jq --arg label "stable" '.include[0] += {label: $label}' <<<"$MATRIX_JSON")
echo "original matrix:"
echo "$ORIGINAL" | jq -c .

if [[ ${#EXP_FLAGS[@]} -eq 0 ]]; then
  UPDATED="$ORIGINAL"
else
  EXP_FLAGS_STRING=$(printf '+%s ' "${EXP_FLAGS[@]}")
  EXPERIMENTAL=$(jq --arg flags "$EXP_FLAGS_STRING" --arg label "experimental" \
    '.include[0] += {flags: $flags, label: $label}' <<<"$MATRIX_JSON")

  UPDATED=$(jq --argjson exp_include "$(jq '.include' <<<"$EXPERIMENTAL")" \
    '.include += $exp_include' <<<"$ORIGINAL")
fi

echo "updated matrix:"
echo "$UPDATED" | jq -c .

if [[ "$GITHUB_OUTPUT" != "" ]]; then
  printf "matrix=%s" "$(echo "$UPDATED" | jq -c .)" >>"$GITHUB_OUTPUT"
fi
