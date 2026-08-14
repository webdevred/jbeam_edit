#!/usr/bin/env bash
# tools/extract-and-format-jbeam/extract-tune-examples.sh
#
# Extracts JBeam files from BeamNG vehicle zips into
# jbeam-examples/original/ and formats them into jbeam-examples/formatted/.
#
# Usage: bash tools/extract-and-format-jbeam/extract-tune-examples.sh [file-list]
#
# file-list defaults to tools/extract-and-format-jbeam/demo-files.txt
# Each line: <filename> <zip>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

source "$SCRIPT_DIR/lib/beamng.sh"

FILE_LIST="${1:-$SCRIPT_DIR/demo-files.txt}"

if [[ ! -f "$FILE_LIST" ]]; then
    echo "error: file list not found: $FILE_LIST" >&2
    exit 1
fi

VEHICLES_DIR="$(beamng_find_vehicles_dir)"
if [[ -z "$VEHICLES_DIR" ]]; then
    echo "error: could not find BeamNG vehicles directory" >&2
    exit 1
fi

ORIG_DIR="$REPO_ROOT/jbeam-examples/original"
FMT_DIR="$REPO_ROOT/jbeam-examples/formatted"

rm -rf "$ORIG_DIR" "$FMT_DIR"
mkdir -p "$ORIG_DIR" "$FMT_DIR"

extracted=0
formatted=0
errors=0

while read -r file zip; do
    [[ -z "$file" || "$file" == \#* ]] && continue

    zip_path="$VEHICLES_DIR/$zip"
    if [[ ! -f "$zip_path" ]]; then
        echo "skip: $zip not found" >&2
        continue
    fi

    inner_path="$(beamng_list_jbeam_files "$zip_path" | grep "/${file}$" | head -1)"
    if [[ -z "$inner_path" ]]; then
        echo "skip: $file not found in $zip" >&2
        continue
    fi

    beamng_extract_file "$zip_path" "$inner_path" "$ORIG_DIR/$file"
    extracted=$((extracted + 1))

    cp "$ORIG_DIR/$file" "$FMT_DIR/$file"
    if cabal run jbeam-edit --project-file=cabal.project.dev -- \
        --rules-path examples/jbfl/minimal.jbfl "$FMT_DIR/$file" 2>/dev/null; then
        rm -f "$FMT_DIR/${file%.jbeam}.bak.jbeam"
        formatted=$((formatted + 1))
    else
        echo "error: failed to format $file" >&2
        errors=$((errors + 1))
    fi
done < "$FILE_LIST"

echo "extracted: $extracted, formatted: $formatted, errors: $errors"
echo "originals: $ORIG_DIR"
echo "formatted: $FMT_DIR"
