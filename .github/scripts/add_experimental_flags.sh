#!/usr/bin/env bash

set -euo pipefail

MATRIX_FILE="$1"
CABAL_FILE="$2"

if [[ -z "$MATRIX_FILE" || ! -f "$MATRIX_FILE" ]]; then
  echo "Usage: $0 matrix_file path/to/package.cabal"
  exit 1
fi

if [[ -z "$CABAL_FILE" || ! -f "$CABAL_FILE" ]]; then
  echo "Cabal file missing: $CABAL_FILE"
  exit 1
fi

MATRIX_JSON=$(sed -E 's/^matrix=//' <"$MATRIX_FILE")

readarray -t EXP_FLAGS < <(awk -f ./.github/script_helpers/extract_flags.awk "$CABAL_FILE")

ORIGINAL=$(jq --arg label "stable" '.include[0] += {label: $label}' <<<"$MATRIX_JSON")

if [[ ${#EXP_FLAGS[@]} -eq 0 ]]; then
  UPDATED="$ORIGINAL"
else
  EXP_FLAGS_STRING=$(printf '+%s ' "${EXP_FLAGS[@]}")
  # TODO: remove this ghc version hardcoding once we migrate the whole project to a more modern ghc version
  EXPERIMENTAL=$(jq --arg ghc "9.12.2" --arg flags "$EXP_FLAGS_STRING" --arg label "experimental" \
    '.include[0] += {ghc : $ghc, flags: $flags, label: $label}' <<<"$MATRIX_JSON")

  UPDATED=$(jq --argjson exp_include "$(jq '.include' <<<"$EXPERIMENTAL")" \
    '.include += $exp_include' <<<"$ORIGINAL")
fi

echo "$UPDATED" | jq -c .
