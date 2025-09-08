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

readarray -t EXP_FLAGS < <(awk '
  /^flag / { flag=$2; in_desc=0 }
  /^ *description:/ { in_desc=1; if ($0 ~ /\(experimental\)/) print flag; next }
  in_desc && /^[[:space:]]+/ { if ($0 ~ /\(experimental\)/) print flag; next }
  /^[^[:space:]]/ { in_desc=0 }
' "$CABAL_FILE")

ORIGINAL=$(jq --arg label "stable" '.include[0] += {label: $label}' <<<"$MATRIX_JSON")

if [[ ${#EXP_FLAGS[@]} -eq 0 ]]; then
  UPDATED="$ORIGINAL"
else
  EXP_FLAGS_STRING=$(printf '+%s ' "${EXP_FLAGS[@]}")
  EXPERIMENTAL=$(jq --arg flags "$EXP_FLAGS_STRING" --arg label "experimental" \
    '.include[0] += {flags: $flags, label: $label}' <<<"$MATRIX_JSON")

  UPDATED=$(jq --argjson exp_include "$(jq '.include' <<<"$EXPERIMENTAL")" \
    '.include += $exp_include' <<<"$ORIGINAL")
fi

echo "$UPDATED" | jq -c .
