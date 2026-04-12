#!/usr/bin/env bash
# tools/extract-and-format-jbeam/tune-run.sh
#
# Extracts JBeam files listed in tune-files.txt from BeamNG vehicle zips,
# formats each with jbeam-edit using minimal.jbfl, and produces diffs.
#
# Output structure in TUNE_DIR:
#   <file>.orig     -- original extracted file
#   <file>          -- formatted file (or original if formatting failed)
#   <file>.diff     -- unified diff (empty if no changes)
#   <file>.error    -- error output (only if formatting failed)
#   summary.tsv     -- one line per file: filename, zip, lines_changed, blank, indent, trailing, colon, structural, status
#
# Usage: bash tools/extract-and-format-jbeam/tune-run.sh [file-list] [filter]
#
# file-list defaults to tools/extract-and-format-jbeam/tune-files.txt
# filter: optional substring to match against zip names

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# categorize_diff <diff_file>
# Prints: blank indent trailing-comma colon-spacing structural
# Each count is the number of changed lines (both - and +) in that category.
categorize_diff() {
    local diff_file="$1"
    awk '
    /^(---|\+\+\+)/ { next }
    /^@@/ { flush(); next }
    /^-/ { minus_lines[++n_minus] = substr($0, 2); next }
    /^\+/ { plus_lines[++n_plus] = substr($0, 2); next }
    END { flush(); print blank+0, indent+0, trailing_comma+0, colon_space+0, structural+0 }

    function flush(    i, n, m, p, ms, ps, ms2, ps2, mc, pc) {
        n = (n_minus < n_plus) ? n_minus : n_plus
        for (i = 1; i <= n; i++) {
            m = minus_lines[i]; p = plus_lines[i]
            if (m ~ /^[[:space:]]*$/ && p ~ /^[[:space:]]*$/) { blank += 2; continue }
            if (m ~ /^[[:space:]]*$/ || p ~ /^[[:space:]]*$/) { blank++; structural++; continue }
            ms = m; ps = p
            gsub(/^[[:space:]]+/, "", ms); gsub(/^[[:space:]]+/, "", ps)
            if (ms == ps) { indent += 2; continue }
            ms2 = ms; ps2 = ps
            sub(/,$/, "", ms2); sub(/,$/, "", ps2)
            if (ms2 == ps2) { trailing_comma += 2; continue }
            mc = ms; pc = ps
            gsub(/[[:space:]]*:[[:space:]]*/, ":", mc)
            gsub(/[[:space:]]*:[[:space:]]*/, ":", pc)
            if (mc == pc) { colon_space += 2; continue }
            structural += 2
        }
        for (i = n+1; i <= n_minus; i++) {
            if (minus_lines[i] ~ /^[[:space:]]*$/) blank++; else structural++
        }
        for (i = n+1; i <= n_plus; i++) {
            if (plus_lines[i] ~ /^[[:space:]]*$/) blank++; else structural++
        }
        delete minus_lines; delete plus_lines; n_minus=0; n_plus=0
    }
    ' "$diff_file"
}

source "$SCRIPT_DIR/lib/beamng.sh"

FILE_LIST="${1:-$SCRIPT_DIR/tune-files.txt}"
FILTER="${2:-}"

if [[ ! -f "$FILE_LIST" ]]; then
    echo "error: file list not found: $FILE_LIST" >&2
    exit 1
fi

VEHICLES_DIR="$(beamng_find_vehicles_dir)"
if [[ -z "$VEHICLES_DIR" ]]; then
    echo "error: could not find BeamNG vehicles directory" >&2
    exit 1
fi

TUNE_DIR="$(mktemp -d /tmp/jbeam-tune-XXXXXX)"
echo "TUNE_DIR=$TUNE_DIR"

extracted=0
formatted=0
errors=0

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

    # Extract
    beamng_extract_file "$zip_path" "$inner_path" "$TUNE_DIR/$file"
    cp "$TUNE_DIR/$file" "$TUNE_DIR/$file.orig"
    extracted=$((extracted + 1))

    # Format
    if cabal run jbeam-edit --project-file=cabal.project.dev -- \
        --rules-path "$REPO_ROOT/examples/jbfl/minimal.jbfl" "$TUNE_DIR/$file" \
        > "$TUNE_DIR/$file.log" 2>&1; then
        rm -f "$TUNE_DIR/${file%.jbeam}.bak.jbeam"
        formatted=$((formatted + 1))
        status="ok"
    else
        # Restore original on failure
        cp "$TUNE_DIR/$file.orig" "$TUNE_DIR/$file"
        mv "$TUNE_DIR/$file.log" "$TUNE_DIR/$file.error"
        status="error"
        errors=$((errors + 1))
    fi
    rm -f "$TUNE_DIR/$file.log"

    # Diff
    diff -u "$TUNE_DIR/$file.orig" "$TUNE_DIR/$file" > "$TUNE_DIR/$file.diff" || true
    lines_changed=$(grep -c '^[+-]' "$TUNE_DIR/$file.diff" | head -1 || echo 0)
    # Subtract header lines (--- and +++ lines)
    if [[ -s "$TUNE_DIR/$file.diff" ]]; then
        header_lines=$(grep -c '^[+-][+-][+-]' "$TUNE_DIR/$file.diff" || echo 0)
        lines_changed=$((lines_changed - header_lines))
    fi

    read -r cat_blank cat_indent cat_trailing cat_colon cat_structural \
        <<< "$(categorize_diff "$TUNE_DIR/$file.diff")"

    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$file" "$zip" "$lines_changed" \
        "$cat_blank" "$cat_indent" "$cat_trailing" "$cat_colon" "$cat_structural" \
        "$status" >> "$TUNE_DIR/summary.tsv"
done < "$FILE_LIST"

echo ""
echo "--- Summary ---"
echo "extracted: $extracted, formatted: $formatted, errors: $errors"
echo ""
if [[ -f "$TUNE_DIR/summary.tsv" ]]; then
    printf '%-40s %-20s %7s  %6s %6s %8s %6s %10s  %s\n' \
        "FILE" "ZIP" "CHANGED" "BLANK" "INDENT" "TRAILING" "COLON" "STRUCTURAL" "STATUS"
    printf '%-40s %-20s %7s  %6s %6s %8s %6s %10s  %s\n' \
        "----" "---" "-------" "-----" "------" "--------" "-----" "----------" "------"
    while IFS=$'\t' read -r f z lc bl ind tr co st status; do
        printf '%-40s %-20s %7s  %6s %6s %8s %6s %10s  %s\n' \
            "$f" "$z" "$lc" "$bl" "$ind" "$tr" "$co" "$st" "$status"
    done < "$TUNE_DIR/summary.tsv"
fi
echo ""
echo "Diffs are in: $TUNE_DIR/*.diff"
echo "Originals:    $TUNE_DIR/*.orig"
