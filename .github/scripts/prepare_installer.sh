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
  awk -v field="$field" -f ./.github/script_helpers/get_cabal_field.awk "$cabal_file"
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
du -h "$EXE_PATH"
echo "Copied exe to /$DEST_DIR_RELEASE/jbeam-edit.exe"

LSP_EXE_PATH=$(find "$DIST_NEWSTYLE/build" -type f -name "jbeam-lsp-server.exe" | head -n 1)

if [ -z "$LSP_EXE_PATH" ]; then
  echo "Error: No LSP exe found in '$DIST_NEWSTYLE/build', skipping."
else
  du -h "$LSP_EXE_PATH"
  cp "$LSP_EXE_PATH" "$DEST_DIR_RELEASE/jbeam-lsp-server.exe"
  echo "Copied exe to /$DEST_DIR_RELEASE/jbeam-lsp-server.exe"
fi

cp ./examples/jbeam-edit.yaml ./.jbeam-edit.yaml

JBEAM_DIR="./examples/jbeam"

custom_diff() {
  diff --color=always --suppress-common-lines "$1" "$2"
}

mapfile -t JBEAM_FILES < <(find "$JBEAM_DIR" -maxdepth 1 -name "*.jbeam" -printf "%f\n")

for f in "${JBEAM_FILES[@]}"; do
  TMP_DIR=$(mktemp -d)
  trap 'rm -rf "$TMP_DIR"' EXIT

  if [[ "${LABEL:-}" == "experimental" ]]; then
    if [[ "$f" == "frame.jbeam" ]]; then
      git show HEAD:"$JBEAM_DIR/frame.jbeam" >"$TMP_DIR/frame.jbeam"
      git show HEAD:"$JBEAM_DIR/fender.jbeam" >"$TMP_DIR/fender.jbeam"

      ./dist/release/jbeam-edit -i -t "$TMP_DIR/frame.jbeam"

      expected_fender="./examples/transformed_jbeam/fender-after-frame-cfg-example.jbeam"
      echo "Checking fender.jbeam after frame transformation"
      custom_diff "$TMP_DIR/fender.jbeam" "$expected_fender"
    else
      git show HEAD:"$JBEAM_DIR/$f" >"$TMP_DIR/$f"
      ./dist/release/jbeam-edit -i -t "$TMP_DIR/$f"
    fi

    expected="./examples/transformed_jbeam/$(basename "$f" .jbeam)-cfg-example.jbeam"
    echo "Checking $f against $expected"
    custom_diff "$TMP_DIR/$f" "$expected"

  else
    git show HEAD:"$JBEAM_DIR/$f" >"$TMP_DIR/$f"
    ./dist/release/jbeam-edit -i "$TMP_DIR/$f"

    expected="./examples/formatted_jbeam/$(basename "$f" .jbeam)-minimal-jbfl.jbeam"
    echo "Checking $f against $expected"
    custom_diff "$TMP_DIR/$f" "$expected"
  fi

  rm -rf "$TMP_DIR"
done

echo "All jbeam files passed checks!"
