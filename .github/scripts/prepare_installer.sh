#!/usr/bin/env bash

set -euo pipefail

DIST_NEWSTYLE="${DIST_NEWSTYLE:?Environment variable DIST_NEWSTYLE is required}"
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

EXE_PATH=$(find "$DIST_NEWSTYLE/build" -type f -name "jbeam-edit.exe" | head -n 1)

if [ -z "$EXE_PATH" ]; then
  echo "Error: No exe found in '$DIST_NEWSTYLE/build'. Make sure the build step succeeded."
  exit 1
fi

cp "$EXE_PATH" "$DEST_DIR_RELEASE/jbeam-edit.exe"
echo "Copied exe to /$DEST_DIR_RELEASE/jbeam-edit.exe"

TMP_DIR=$(mktemp -d)
git show HEAD:"./examples/jbeam/fender.jbeam" >"$TMP_DIR/fender.${LABEL}.jbeam"
git show HEAD:"./examples/jbeam/suspension.jbeam" >"$TMP_DIR/suspension.${LABEL}.jbeam"

echo "fender.jbeam: $TMP_DIR/fender.${LABEL}.jbeam"
echo "suspension.jbeam: $TMP_DIR/suspension.${LABEL}.jbeam"

./dist/release/jbeam-edit -i "$TMP_DIR/fender.${LABEL}.jbeam"
./dist/release/jbeam-edit -i "$TMP_DIR/suspension.${LABEL}.jbeam"

custom_diff() {
  diff --color=always --suppress-common-lines "$1" "$2"
}

if [[ -n $LABEL ]] && [[ "$LABEL" == "experimental" ]]; then
  custom_diff "$TMP_DIR/fender.experimental.jbeam" ./examples/transformed_jbeam/fender-cfg-default.jbeam
  custom_diff "$TMP_DIR/suspension.experimental.jbeam" ./examples/transformed_jbeam/suspension-cfg-default.jbeam
else
  custom_diff "$TMP_DIR/fender.stable.jbeam" ./examples/formatted_jbeam/fender-minimal-jbfl.jbeam
  custom_diff "$TMP_DIR/suspension.stable.jbeam" ./examples/formatted_jbeam/suspension-minimal-jbfl.jbeam
fi
