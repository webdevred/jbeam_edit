#!/usr/bin/env bash
#
# Checks that formatting a file keeps the line endings it came with. The
# detection is exercised through the binary rather than the library, because
# reading the file, detecting the ending and writing it back out are three
# separate steps and any one of them can drop it.
#
# Usage: bash ./.github/scripts/check_newline_preservation.sh <jbeam-edit-binary>

set -euo pipefail

BINARY=$(realpath "${1:?usage: check_newline_preservation.sh <jbeam-edit-binary>}")
SOURCE_FILE="examples/jbeam/frame.jbeam"

WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

# The binary runs from the work directory, so point it back at the repo for the
# default ruleset it ships as a data file.
export jbeam_edit_datadir="$PWD"

failed=0

carriage_returns() {
  grep -c $'\r' "$1" || true
}

check() {
  local label="$1" expected="$2" target="$WORK_DIR/$1.jbeam"

  if [[ "$label" == crlf ]]; then
    sed 's/\r$//; s/$/\r/' "$SOURCE_FILE" >"$target"
  else
    sed 's/\r$//' "$SOURCE_FILE" >"$target"
  fi

  local before after
  before=$(carriage_returns "$target")
  if [[ "$before" -ne "$expected" ]]; then
    echo "setup error: $label input has $before carriage returns, expected $expected"
    failed=1
    return
  fi

  (cd "$WORK_DIR" && "$BINARY" "$label.jbeam" >/dev/null)

  after=$(carriage_returns "$target")
  if [[ "$expected" -eq 0 && "$after" -ne 0 ]]; then
    echo "$label: line endings changed, LF input came out with $after carriage returns"
    failed=1
  elif [[ "$expected" -ne 0 && "$after" -eq 0 ]]; then
    echo "$label: line endings changed, CRLF input came out with no carriage returns"
    failed=1
  fi
}

lf_lines=$(sed 's/\r$//' "$SOURCE_FILE" | wc -l)

check lf 0
check crlf "$lf_lines"

if [[ "$failed" -ne 0 ]]; then
  echo "formatting does not preserve line endings"
  exit 1
fi

echo "line endings survive a format for both LF and CRLF input"
