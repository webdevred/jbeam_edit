#!/usr/bin/env bash

set -euo pipefail

CABAL_FILE="${CABAL_FILE:?Environment variable CABAL_FILE is required}"
DIST_DIR="dist/release"
OUT_FILE="installer/constants.inc"

GET_FIELD_AWK="./.github/script_helpers/get_cabal_field.awk"

get_field() {
  local field="$1"
  awk -v field="$field" -f "$GET_FIELD_AWK" "$CABAL_FILE" | head -n1
}

get_multi_field() {
  local field="$1"
  awk -v field="$field" -f "$GET_FIELD_AWK" "$CABAL_FILE"
}

escape() {
  local input="$1"
  printf '%s' "$input" | sed 's/["\\]/\\&/g'
}

APP_NAME=$(get_field "name")
APP_VERSION=$(get_field "version")

if [[ -z "$APP_NAME" || -z "$APP_VERSION" ]]; then
  echo "Failed to read name/version from cabal file"
  exit 1
fi

REL_DIST_DIR="..\\dist\\release"

{
  echo "; -*- mode: iss -*-"
  echo "; Auto-generated from $CABAL_FILE"

  echo
  echo "#define AppName \"$(escape "$APP_NAME")\""
  echo "#define AppVersion \"$(escape "$APP_VERSION")\""
  echo "#define ReleaseDir \"$REL_DIST_DIR\""
  echo
  echo "[Files]"

  shopt -s nullglob
  for exe_path in "$DIST_DIR"/*.exe; do
    exe_name=$(basename "$exe_path")
    if [[ "$exe_name" == "jbeam-edit.exe" ]]; then
      echo "Source: \"$REL_DIST_DIR\\$exe_name\"; DestDir: \"{app}\"; Flags: ignoreversion"
    else
      echo "Source: \"$REL_DIST_DIR\\$exe_name\"; DestDir: \"{app}\"; Flags: ignoreversion skipifsourcedoesntexist"
    fi
  done
  shopt -u nullglob

  while read -r file_path; do
    [[ -z "$file_path" ]] && continue
    dest="{app}\\$(dirname "$file_path")"
    echo "Source: \"$REL_DIST_DIR\\$file_path\"; DestDir: \"$dest\"; Flags: ignoreversion"
  done < <(get_multi_field "data-files")

} >"$OUT_FILE"

echo "Generated $OUT_FILE"
