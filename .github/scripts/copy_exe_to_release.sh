#!/usr/bin/env bash
set -euo pipefail

DIST_NEWSTYLE="${DIST_NEWSTYLE:?Environment variable DIST_NEWSTYLE is required}"
RELEASE_DIR="${RELEASE_DIR:?Environment variable RELEASE_DIR is required}"

mkdir -p "$RELEASE_DIR"

EXE_PATH=$(find "$DIST_NEWSTYLE/build" -type f -name "jbeam-edit.exe" | head -n 1)

if [ -z "$EXE_PATH" ]; then
  echo "Error: No exe found in '$DIST_NEWSTYLE/build'. Make sure the build step succeeded."
  exit 1
fi

cp "$EXE_PATH" "$RELEASE_DIR/jbeam-edit.exe"
echo "Copied exe to $RELEASE_DIR/jbeam-edit.exe"
