#!/usr/bin/env bash

set -euo pipefail

RELEASE_DIR="${RELEASE_DIR:?Environment variable RELEASE_DIR is required}"
ZIP_DIR="${ZIP_DIR:?Environment variable ZIP_DIR is required}"
ZIP_FILE="${ZIP_FILE:-dist/jbeam-edit-${GITHUB_REF_NAME}.zip}"

mkdir -p "$ZIP_DIR"
mkdir -p "$(dirname "$ZIP_FILE")"

SETUP_EXE="installer/Output/jbeam-edit-setup.exe"
if [ ! -f "$SETUP_EXE" ]; then
  echo "Error: setup.exe not found in release folder '$RELEASE_DIR'. Make sure Inno Setup ran successfully."
  exit 1
fi

cp "$SETUP_EXE" "$ZIP_DIR/"

7z a -tzip "dist/jbeam-edit-${GITHUB_REF_NAME}-${LABEL}.zip" ./dist/zip_temp/*
