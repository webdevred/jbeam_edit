#!/usr/bin/env bash

set -euo pipefail

RELEASE_DIR="${RELEASE_DIR:?Environment variable RELEASE_DIR is required}"
ZIP_DIR="${ZIP_DIR:?Environment variable ZIP_DIR is required}"
ZIP_FILE="${ZIP_FILE:-dist/jbeam-edit-${GITHUB_REF_NAME}.zip}"

mkdir -p "$ZIP_DIR"
mkdir -p "$(dirname "$ZIP_FILE")"

SETUP_EXE="installer/Output/setup.exe"
if [ ! -f "$SETUP_EXE" ]; then
  echo "Error: setup.exe not found in release folder '$RELEASE_DIR'. Make sure Inno Setup ran successfully."
  exit 1
fi

cp "$SETUP_EXE" "$ZIP_DIR/"

for f in README.md JBFL_DOCS.md LICENSE; do
  SRC="$RELEASE_DIR/$f"
  if [ -f "$SRC" ]; then
    cp "$SRC" "$ZIP_DIR/"
  else
    echo "Warning: $f not found in release folder, skipping."
  fi
done
