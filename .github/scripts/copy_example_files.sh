#!/usr/bin/env bash
set -euo pipefail

CABAL_FILE="${CABAL_FILE:?Environment variable CABAL_FILE is required}"
DEST_DIR_RELEASE="${RELEASE_DIR:?Environment variable RELEASE_DIR is required}"
DEST_DIR_ZIP="${ZIP_DIR:?Environment variable ZIP_DIR is required}"

[[ -f "$CABAL_FILE" ]] || {
  echo "Error: cabal file '$CABAL_FILE' does not exist"
  exit 1
}
mkdir -p "$DEST_DIR_RELEASE"
mkdir -p "$DEST_DIR_ZIP"

get_cabal_field() {
  local cabal_file="$1"
  local field="$2"
  awk -v field="$field" '
        BEGIN { in_field=0 }
        $0 ~ "^"field":" { in_field=1; next }
        in_field && /^[^[:space:]]/ { in_field=0 }
        in_field { gsub(/^[[:space:]]+/, ""); print }
    ' "$cabal_file"
}

while IFS= read -r file; do
  [[ -z "$file" ]] && continue
  mkdir -p "$(dirname "$DEST_DIR_RELEASE/$file")"
  cp -r "$file" "$DEST_DIR_RELEASE/$file"
  mkdir -p "$(dirname "$DEST_DIR_ZIP/$file")"
  cp -r "$file" "$DEST_DIR_ZIP/$file"
done < <(get_cabal_field "$CABAL_FILE" "data-files")

while IFS= read -r file; do
  [[ -z "$file" ]] && continue
  mkdir -p "$(dirname "$DEST_DIR_ZIP/$file")"
  cp -r "$file" "$DEST_DIR_ZIP/$file"
done < <(get_cabal_field "$CABAL_FILE" "extra-source-files")
