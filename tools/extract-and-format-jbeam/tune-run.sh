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
#   summary.tsv     -- one line per file: filename, zip, lines_changed, blank, indent, trailing, spacing, structural, status
#
# Usage: bash tools/extract-and-format-jbeam/tune-run.sh [file-list] [filter] [rules]
#
# file-list defaults to tools/extract-and-format-jbeam/tune-files.txt
# filter: optional substring to match against zip names
# rules: jbfl ruleset to format with, defaults to examples/jbfl/minimal.jbfl.
#        Pass a second one to compare two rulesets over the same files.
#
# The default list is a curated handful, meant to be read by hand. To ask how
# common something is across all stock files instead, generate a list covering
# every vehicle zip and pass that:
#
#   source tools/extract-and-format-jbeam/lib/beamng.sh
#   V="$(beamng_find_vehicles_dir)"
#   for z in "$V"/*.zip; do
#       zn="$(basename "$z")"
#       beamng_list_jbeam_files "$z" | while IFS= read -r p; do
#           echo "$(basename "$p") $zn"
#       done
#   done > /tmp/corpus-files.txt
#
# That list depends on the installed game version, so it is generated when
# needed rather than committed.
#
# Output files are named <vehicle>__<basename>, the same way corpus-extract.sh
# names them, because roughly 175 base names collide across the stock vehicles
# and a flat directory would otherwise keep only the last of each.
#
# One limit still gives a wrong answer quietly. The status column is decided by
# the exit code, and jbeam-edit exits 0 even when it cannot parse the file. Every
# row therefore reads "ok" and the run reports no errors at all, whatever the
# file contained. To count files that failed to parse, look at what the tool
# wrote to stderr instead.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# categorize_diff <diff_file>
# Prints: blank indent trailing-comma inner-spacing structural
# Each count is the number of changed lines (both - and +) in that category.
categorize_diff() {
  local diff_file="$1"
  awk '
    /^(---|\+\+\+)/ { next }
    /^@@/ { flush(); next }
    /^-/ { minus_lines[++n_minus] = substr($0, 2); next }
    /^\+/ { plus_lines[++n_plus] = substr($0, 2); next }
    END { flush(); print blank+0, indent+0, trailing_comma+0, inner_space+0, structural+0 }

    function strip_ws(s) { gsub(/^[[:space:]]+/, "", s); return s }
    function strip_comma(s) { sub(/,$/, "", s); return s }
    # Spacing the formatter adds or removes inside a line, around both the
    # colon of a key and the commas of an array. The two travel together in
    # practice, so one category counts both.
    function strip_spacing(s) {
        gsub(/[[:space:]]*:[[:space:]]*/, ":", s)
        gsub(/,[[:space:]]+/, ",", s)
        return s
    }

    # A changed line is classified by the least invasive normalisation that
    # finds it an unclaimed partner on the other side of the hunk. Pairing by
    # position instead looks simpler and is wrong: the formatter adds and removes
    # lines, so the two sides fall out of step and every later pair compares
    # unrelated text, which lands the rest of the file in structural.
    function flush(    i, key, form) {
        delete claimed
        for (i = 1; i <= n_plus; i++) {
            if (plus_lines[i] ~ /^[[:space:]]*$/) { blank_plus[++n_blank_plus] = i; continue }
            by_ws[strip_ws(plus_lines[i])] = by_ws[strip_ws(plus_lines[i])] " " i
            form = strip_comma(strip_ws(plus_lines[i]))
            by_comma[form] = by_comma[form] " " i
            form = strip_spacing(strip_ws(plus_lines[i]))
            by_space[form] = by_space[form] " " i
        }
        for (i = 1; i <= n_minus; i++) {
            if (minus_lines[i] ~ /^[[:space:]]*$/) { blank++; continue }
            key = strip_ws(minus_lines[i])
            if (take(by_ws, key)) { indent += 2; continue }
            if (take(by_comma, strip_comma(key))) { trailing_comma += 2; continue }
            if (take(by_space, strip_spacing(key))) { inner_space += 2; continue }
            structural++
        }
        for (i = 1; i <= n_plus; i++)
            if (!(i in claimed) && !(plus_lines[i] ~ /^[[:space:]]*$/)) structural++
        blank += n_blank_plus
        delete minus_lines; delete plus_lines; delete by_ws; delete by_comma
        delete by_space; delete blank_plus
        n_minus = 0; n_plus = 0; n_blank_plus = 0
    }

    # Claims the first unclaimed plus line stored under key, if any.
    function take(map, key,    parts, n, i) {
        if (!(key in map)) return 0
        n = split(map[key], parts, " ")
        for (i = 1; i <= n; i++)
            if (!(parts[i] in claimed)) { claimed[parts[i]] = 1; return 1 }
        return 0
    }
    ' "$diff_file"
}

source "$SCRIPT_DIR/lib/beamng.sh"

FILE_LIST="${1:-$SCRIPT_DIR/tune-files.txt}"
FILTER="${2:-}"
RULES="${3:-$REPO_ROOT/examples/jbfl/minimal.jbfl}"

if [[ ! -f "$FILE_LIST" ]]; then
  echo "error: file list not found: $FILE_LIST" >&2
  exit 1
fi

if [[ ! -f "$RULES" ]]; then
  echo "error: ruleset not found: $RULES" >&2
  exit 1
fi

# Resolve the binary once. cabal run per file dominates the runtime, and it also
# relinks, which breaks anything else reading the binary at the same time.
if ! cabal build --project-file=cabal.project.dev exe:jbeam-edit >/dev/null; then
  echo "error: could not build exe:jbeam-edit" >&2
  exit 1
fi
JBEAM_EDIT="$(cabal list-bin --project-file=cabal.project.dev exe:jbeam-edit)"

# cabal run sets this for us; a bare binary finds no shipped ruleset without it,
# formats with no rules at all, and still reports that it loaded the one named
# by --rules-path.
export jbeam_edit_datadir="$REPO_ROOT"

VEHICLES_DIR="$(beamng_find_vehicles_dir)"
if [[ -z "$VEHICLES_DIR" ]]; then
  echo "error: could not find BeamNG vehicles directory" >&2
  exit 1
fi

TUNE_DIR="$(mktemp -d /tmp/jbeam-edit-tune-XXXXXX)"
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

  # Extract. The output name carries the vehicle, because the same base name
  # ships in several vehicles and a flat directory would keep only the last.
  vehicle="$(basename "$zip" .zip)"
  out="${vehicle}__$file"
  beamng_extract_file "$zip_path" "$inner_path" "$TUNE_DIR/$out"
  cp "$TUNE_DIR/$out" "$TUNE_DIR/$out.orig"
  extracted=$((extracted + 1))

  # Format
  if "$JBEAM_EDIT" --rules-path "$RULES" "$TUNE_DIR/$out" \
    >"$TUNE_DIR/$out.log" 2>&1; then
    rm -f "$TUNE_DIR/${out%.jbeam}.bak.jbeam"
    formatted=$((formatted + 1))
    status="ok"
  else
    # Restore original on failure
    cp "$TUNE_DIR/$out.orig" "$TUNE_DIR/$out"
    mv "$TUNE_DIR/$out.log" "$TUNE_DIR/$out.error"
    status="error"
    errors=$((errors + 1))
  fi
  rm -f "$TUNE_DIR/$out.log"

  # Diff
  diff -u "$TUNE_DIR/$out.orig" "$TUNE_DIR/$out" >"$TUNE_DIR/$out.diff" || true
  lines_changed=$(grep -c '^[+-]' "$TUNE_DIR/$out.diff" | head -1 || echo 0)
  # Subtract header lines (--- and +++ lines)
  if [[ -s "$TUNE_DIR/$out.diff" ]]; then
    header_lines=$(grep -c '^[+-][+-][+-]' "$TUNE_DIR/$out.diff" || echo 0)
    lines_changed=$((lines_changed - header_lines))
  fi

  read -r cat_blank cat_indent cat_trailing cat_spacing cat_structural \
    <<<"$(categorize_diff "$TUNE_DIR/$out.diff")"

  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$file" "$zip" "$lines_changed" \
    "$cat_blank" "$cat_indent" "$cat_trailing" "$cat_spacing" "$cat_structural" \
    "$status" >>"$TUNE_DIR/summary.tsv"
done <"$FILE_LIST"

echo ""
echo "--- Summary ---"
echo "extracted: $extracted, formatted: $formatted, errors: $errors"
echo ""
if [[ -f "$TUNE_DIR/summary.tsv" ]]; then
  printf '%-40s %-20s %7s  %6s %6s %8s %6s %10s  %s\n' \
    "FILE" "ZIP" "CHANGED" "BLANK" "INDENT" "TRAILING" "SPACING" "STRUCTURAL" "STATUS"
  printf '%-40s %-20s %7s  %6s %6s %8s %6s %10s  %s\n' \
    "----" "---" "-------" "-----" "------" "--------" "-----" "----------" "------"
  while IFS=$'\t' read -r f z lc bl ind tr co st status; do
    printf '%-40s %-20s %7s  %6s %6s %8s %6s %10s  %s\n' \
      "$f" "$z" "$lc" "$bl" "$ind" "$tr" "$co" "$st" "$status"
  done <"$TUNE_DIR/summary.tsv"
fi
echo ""
echo "Diffs are in: $TUNE_DIR/*.diff"
echo "Originals:    $TUNE_DIR/*.orig"
