#!/usr/bin/env bash
set -euo pipefail

DIST_NEWSTYLE="${DIST_NEWSTYLE:?Environment variable DIST_NEWSTYLE is required}"
CABAL_FILE="${CABAL_FILE:?Environment variable CABAL_FILE is required}"
DEST_DIR_RELEASE="${RELEASE_DIR:?Environment variable RELEASE_DIR is required}"
DEST_DIR_ZIP="${ZIP_DIR:?Environment variable ZIP_DIR is required}"
LABEL="${LABEL:?Environment variable LABEL is required}"

if ! [[ -f "$CABAL_FILE" ]]; then
  echo "Error: cabal file '$CABAL_FILE' does not exist"
  exit 1
fi

find examples/formatted_jbeam examples/transformed_jbeam -type f -exec sed -i 's/$/\r/' {} +

mkdir -p "$DEST_DIR_RELEASE" "$DEST_DIR_ZIP"

get_cabal_field() {
  local cabal_file="$1"
  local field="$2"
  awk -v field="$field" -f ./.github/script_helpers/get_cabal_field.awk "$cabal_file"
}

copy_field_files() {
  local field="$1"
  local dest="$2"
  while IFS= read -r file; do
    [[ -z "$file" ]] && continue
    mkdir -p "$(dirname "$dest/$file")"
    cp -r "$file" "$dest/$file"
  done < <(get_cabal_field "$CABAL_FILE" "$field")
}

copy_field_files "data-files" "$DEST_DIR_RELEASE"
copy_field_files "data-files" "$DEST_DIR_ZIP"
copy_field_files "extra-source-files" "$DEST_DIR_ZIP"

declare -A EXES=(
  ["jbeam-edit.exe"]="$(find "$DIST_NEWSTYLE/build" -type f -name "jbeam-edit.exe" | head -n1)"
  ["jbeam-lsp-server.exe"]="$(find "$DIST_NEWSTYLE/build" -type f -name "jbeam-lsp-server.exe" | head -n1)"
)

for name in "${!EXES[@]}"; do
  src="${EXES[$name]}"
  if [[ -f "$src" ]]; then
    cp "$src" "$DEST_DIR_RELEASE/$name"
    echo "Copied $name to $DEST_DIR_RELEASE/$name"
  elif [[ "$name" == "jbeam-edit.exe" ]]; then
    echo "Error: $name not found at $src"
    exit 1
  fi
done

TMP_DIR=$(mktemp -d)

for filepath in ./examples/jbeam/*.jbeam; do
  name=$(basename "$filepath" .jbeam)
  git show HEAD:"$filepath" >"$TMP_DIR/$name.${LABEL}.jbeam"
  echo "$name.jbeam: $TMP_DIR/$name.${LABEL}.jbeam"
  ./dist/release/jbeam-edit -i "$TMP_DIR/$name.${LABEL}.jbeam"
done

custom_diff() {
  diff --color=always --suppress-common-lines "$1" "$2"
}

for tmp in "$TMP_DIR"/*.jbeam; do
  base=$(basename "$tmp")
  name="${base%%.*}"
  if [[ "$LABEL" == "experimental" ]]; then
    custom_diff "$TMP_DIR/$name.experimental.jbeam" "./examples/transformed_jbeam/$name-cfg-default.jbeam"
  else
    custom_diff "$TMP_DIR/$name.stable.jbeam" "./examples/formatted_jbeam/$name-minimal-jbfl.jbeam"
  fi
done
