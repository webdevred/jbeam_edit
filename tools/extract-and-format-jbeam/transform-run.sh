#!/usr/bin/env bash
# tools/extract-and-format-jbeam/transform-run.sh
#
# Extracts JBeam structural files from BeamNG vehicle zips, runs --transform
# on each, validates beam references, and prints a summary.
#
# Output in TRANSFORM_DIR:
#   <file>.orig     -- original extracted file
#   <file>          -- transformed file (or original if transformation failed)
#   <file>.diff     -- unified diff (empty if no changes)
#   <file>.err      -- stderr from transformation (only if non-empty)
#   beams.err       -- stderr from --validate-beams
#
# Usage: bash tools/extract-and-format-jbeam/transform-run.sh [options] [file-list] [filter]
#
# Options:
#   --cross-file    Also extract all other .jbeam files from each vehicle zip,
#                   so beam validation can resolve cross-file references.
#
# file-list defaults to tools/extract-and-format-jbeam/transform-check-files.txt
# filter: optional substring to match against zip names (e.g. bolide, burnside)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

source "$SCRIPT_DIR/lib/beamng.sh"

CROSS_FILE=false
FILE_LIST=""
FILTER=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --cross-file) CROSS_FILE=true; shift ;;
        -*) echo "error: unknown option: $1" >&2; exit 1 ;;
        *)
            if [[ -z "$FILE_LIST" ]]; then
                FILE_LIST="$1"
            else
                FILTER="$1"
            fi
            shift
            ;;
    esac
done

FILE_LIST="${FILE_LIST:-$SCRIPT_DIR/transform-check-files.txt}"

if [[ ! -f "$FILE_LIST" ]]; then
    echo "error: file list not found: $FILE_LIST" >&2
    exit 1
fi

VEHICLES_DIR="$(beamng_find_vehicles_dir)"
if [[ -z "$VEHICLES_DIR" ]]; then
    echo "error: could not find BeamNG vehicles directory" >&2
    exit 1
fi

TRANSFORM_DIR="$(mktemp -d /tmp/jbeam-transform-XXXXXX)"
echo "TRANSFORM_DIR=$TRANSFORM_DIR"

node_names() {
    # Extract node names from a jbeam file: first string in arrays with 4 elements
    # where elements 2-4 look like numbers. Prints one name per line.
    python3 -c "
import re, sys
text = open(sys.argv[1]).read()
text = re.sub(r'//[^\n]*', '', text)
text = re.sub(r'/\*.*?\*/', '', text, flags=re.DOTALL)
for m in re.finditer(r'\[\s*\"([^\"]+)\"\s*,\s*[-0-9.e]+\s*,\s*[-0-9.e]+\s*,\s*[-0-9.e]+', text):
    print(m.group(1))
" "$1"
}

count_renames() {
    local orig="$1" transformed="$2"
    # Count node names present in orig but not in transformed (i.e. were renamed away)
    comm -23 \
        <(node_names "$orig" | sort -u) \
        <(node_names "$transformed" | sort -u) \
        | wc -l
}

extracted=0
transformed=0
errors=0
declare -a rows

while read -r file zip; do
    [[ -z "$file" || "$file" == \#* ]] && continue
    if [[ -n "$FILTER" && "$zip" != *"$FILTER"* ]]; then
        continue
    fi

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

    beamng_extract_file "$zip_path" "$inner_path" "$TRANSFORM_DIR/$file"
    cp "$TRANSFORM_DIR/$file" "$TRANSFORM_DIR/$file.orig"
    extracted=$((extracted + 1))

    if $CROSS_FILE; then
        # Extract all other .jbeam files from the same zip (for beam validation)
        while IFS= read -r other_inner; do
            other_file="$(basename "$other_inner")"
            [[ "$other_file" == "$file" ]] && continue
            [[ -f "$TRANSFORM_DIR/$other_file" ]] && continue
            beamng_extract_file "$zip_path" "$other_inner" "$TRANSFORM_DIR/$other_file"
        done < <(beamng_list_jbeam_files "$zip_path")
    fi

    stderr_file="$TRANSFORM_DIR/$file.err"
    if cabal run jbeam-edit --project-file=cabal.project.dev -- \
        --transform "$TRANSFORM_DIR/$file" \
        > /dev/null 2> "$stderr_file"; then
        rm -f "$TRANSFORM_DIR/${file%.jbeam}.bak.jbeam"
        transformed=$((transformed + 1))
        outcome="success"
    else
        cp "$TRANSFORM_DIR/$file.orig" "$TRANSFORM_DIR/$file"
        outcome="error"
        errors=$((errors + 1))
    fi
    [[ ! -s "$stderr_file" ]] && rm -f "$stderr_file"

    diff -u "$TRANSFORM_DIR/$file.orig" "$TRANSFORM_DIR/$file" \
        > "$TRANSFORM_DIR/$file.diff" || true

    renames="$(count_renames "$TRANSFORM_DIR/$file.orig" "$TRANSFORM_DIR/$file")"

    warnings=0
    [[ -f "$stderr_file" ]] && warnings="$(wc -l < "$stderr_file")"

    rows+=("$file	$zip	$outcome	$renames	$warnings")
done < "$FILE_LIST"

# Beam validation
echo ""
echo "Running --validate-beams on $TRANSFORM_DIR ..."
beams_err="$TRANSFORM_DIR/beams.err"
(cd "$TRANSFORM_DIR" && cabal run jbeam-edit \
    --project-file="$REPO_ROOT/cabal.project.dev" -- \
    --validate-beams 2> "$beams_err") || true

unknown_verts=0
dup_beams=0
if [[ -f "$beams_err" ]]; then
    unknown_verts="$(grep -c 'unknown vertex' "$beams_err" || true)"
    dup_beams="$(grep -c 'duplicate beam' "$beams_err" || true)"
    unknown_verts="${unknown_verts:-0}"
    dup_beams="${dup_beams:-0}"
fi

echo ""
echo "--- Summary ---"
echo "extracted: $extracted, transformed: $transformed, errors: $errors"
echo ""
printf '%-40s %-20s %-10s %7s %8s\n' \
    "FILE" "ZIP" "OUTCOME" "RENAMED" "WARNINGS"
printf '%-40s %-20s %-10s %7s %8s\n' \
    "----" "---" "-------" "-------" "--------"
for row in "${rows[@]}"; do
    IFS=$'\t' read -r f z out ren warn <<< "$row"
    printf '%-40s %-20s %-10s %7s %8s\n' "$f" "$z" "$out" "$ren" "$warn"
done
echo ""
echo "Beam validation: unknown vertices=$unknown_verts, duplicate beams=$dup_beams"
echo ""
echo "Files are in: $TRANSFORM_DIR"
