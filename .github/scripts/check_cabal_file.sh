#!/usr/bin/env bash

set -euo pipefail

CABAL_FILE=$(find . -maxdepth 1 -name '*.cabal' | head -n 1)
TMP_DIR=$(mktemp -d)

HPACK_HEADER="This file has been generated from package.yaml by hpack"

cp "$CABAL_FILE" "$TMP_DIR/current.cabal"
grep -q "$HPACK_HEADER" "$TMP_DIR/current.cabal"
cabal format "$TMP_DIR/current.cabal" 2>/dev/null

git show HEAD:"$CABAL_FILE" >"$TMP_DIR/committed.cabal"

HEADER_LINE=$(("$(grep -n "$HPACK_HEADER" "$TMP_DIR/committed.cabal" | cut -d: -f1)" - 1))

awk -v header="$HPACK_HEADER" '
  BEGIN { skip = 0 }
  skip == 0 && $0 ~ header { skip = 1; next }
  skip == 1 && /^--/ { next }
  skip == 1 { skip = 2; next }
  { print }
' "$TMP_DIR/committed.cabal" >"$TMP_DIR/committed_filtered.cabal"

MAYBE_REDUNDANT_LINE=$(awk "NR==$HEADER_LINE" "$TMP_DIR/committed_filtered.cabal")
if [[ -z "$MAYBE_REDUNDANT_LINE" ]]; then
  sed -i "${HEADER_LINE}d" "$TMP_DIR/committed_filtered.cabal"
fi

if ! diff -q "$TMP_DIR/committed_filtered.cabal" "$TMP_DIR/current.cabal" >/dev/null; then
  echo "Error: .cabal file changed beyond the expected hpack comment line."
  echo "Only package.yaml should be edited. Regenerate the .cabal file with hpack and run cabal format on it."
  echo "Manual changes to the .cabal file are not allowed and must be reverted."
  diff -u "$TMP_DIR/committed_filtered.cabal" "$TMP_DIR/current.cabal" || true
  exit 1
else
  echo ".cabal file is unchanged except for the hpack comment line."
fi
