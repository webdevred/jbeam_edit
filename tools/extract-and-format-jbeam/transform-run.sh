#!/usr/bin/env bash
# tools/extract-and-format-jbeam/transform-run.sh
#
# Extracts JBeam structural files from BeamNG vehicle zips, runs --transform
# on each twice, validates beam references, and prints a summary.
#
# Every file gets its own directory under TRANSFORM_DIR/<vehicle>/<file>/,
# because --transform also rewrites references in the other .jbeam files next
# to it. Each of those directories holds:
#   <file>.orig             -- original extracted file
#   <file>                  -- transformed file (or original if it failed)
#   <file>.diff             -- unified diff (empty if no changes)
#   <file>.once             -- output of the first pass, kept for comparison
#   <file>.second-pass.diff -- what a second --transform changed, if anything
#   <file>.err              -- stderr from transformation (only if non-empty)
#   beams.err               -- stderr from --validate-beams
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
    --cross-file)
      CROSS_FILE=true
      shift
      ;;
    -*)
      echo "error: unknown option: $1" >&2
      exit 1
      ;;
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

TRANSFORM_DIR="$(mktemp -d /tmp/jbeam-edit-transform-XXXXXX)"
echo "TRANSFORM_DIR=$TRANSFORM_DIR"

# The tool runs from a work directory, so point it back at the repo for the
# default ruleset it ships as a data file. Without this it falls back to no
# formatting rules at all and blames a parse error for it.
export jbeam_edit_datadir="$REPO_ROOT"

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
    <(node_names "$transformed" | sort -u) |
    wc -l
}

extracted=0
transformed=0
errors=0
unstable=0
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

  # Each file gets its own directory. --transform also rewrites vehicle
  # references in every other .jbeam next to the file, so files sharing a
  # working directory rewrite each other and the measurements come out
  # against the wrong baseline.
  work_dir="$TRANSFORM_DIR/${zip%.zip}/${file%.jbeam}"
  mkdir -p "$work_dir"

  beamng_extract_file "$zip_path" "$inner_path" "$work_dir/$file"
  cp "$work_dir/$file" "$work_dir/$file.orig"
  extracted=$((extracted + 1))

  if $CROSS_FILE; then
    # Extract all other .jbeam files from the same zip (for beam validation)
    while IFS= read -r other_inner; do
      other_file="$(basename "$other_inner")"
      [[ "$other_file" == "$file" ]] && continue
      [[ -f "$work_dir/$other_file" ]] && continue
      beamng_extract_file "$zip_path" "$other_inner" "$work_dir/$other_file"
    done < <(beamng_list_jbeam_files "$zip_path")
  fi

  stderr_file="$work_dir/$file.err"
  if cabal run jbeam-edit --project-file=cabal.project.dev -- \
    --transform "$work_dir/$file" \
    >/dev/null 2>"$stderr_file"; then
    rm -f "$work_dir/${file%.jbeam}.bak.jbeam"
    transformed=$((transformed + 1))
    outcome="success"
  else
    cp "$work_dir/$file.orig" "$work_dir/$file"
    outcome="error"
    errors=$((errors + 1))
  fi
  [[ ! -s "$stderr_file" ]] && rm -f "$stderr_file"

  diff -u "$work_dir/$file.orig" "$work_dir/$file" \
    >"$work_dir/$file.diff" || true

  renames="$(count_renames "$work_dir/$file.orig" "$work_dir/$file")"

  # Transforming an already transformed file must not change it again. One
  # pass tells you almost nothing: names that grow, comments that swap places
  # and metadata that drifts all look like success until the second run.
  fixed_point="n/a"
  if [[ "$outcome" == "success" ]]; then
    cp "$work_dir/$file" "$work_dir/$file.once"
    if cabal run jbeam-edit --project-file=cabal.project.dev -- \
      --transform "$work_dir/$file" \
      >/dev/null 2>>"$stderr_file"; then
      rm -f "$work_dir/${file%.jbeam}.bak.jbeam"
      if diff -u "$work_dir/$file.once" "$work_dir/$file" \
        >"$work_dir/$file.second-pass.diff"; then
        fixed_point="yes"
        rm -f "$work_dir/$file.second-pass.diff"
      else
        fixed_point="NO"
        unstable=$((unstable + 1))
      fi
    else
      fixed_point="error"
      unstable=$((unstable + 1))
    fi
    cp "$work_dir/$file.once" "$work_dir/$file"
  fi

  warnings=0
  [[ -f "$stderr_file" ]] && warnings="$(wc -l <"$stderr_file")"

  rows+=("$file	$zip	$outcome	$renames	$warnings	$fixed_point")
done <"$FILE_LIST"

# Beam validation, per vehicle so files from different vehicles cannot resolve
# each other's references.
echo ""
echo "Running --validate-beams per vehicle in $TRANSFORM_DIR ..."
unknown_verts=0
dup_beams=0
while IFS= read -r dir; do
  beams_err="$dir/beams.err"
  (cd "$dir" && cabal run jbeam-edit \
    --project-file="$REPO_ROOT/cabal.project.dev" -- \
    --validate-beams 2>"$beams_err") || true
  [[ -f "$beams_err" ]] || continue
  unknown_verts=$((unknown_verts + $(grep -c 'unknown vertex' "$beams_err" || true)))
  dup_beams=$((dup_beams + $(grep -c 'duplicate beam' "$beams_err" || true)))
done < <(find "$TRANSFORM_DIR" -mindepth 2 -maxdepth 2 -type d)

echo ""
echo "--- Summary ---"
echo "extracted: $extracted, transformed: $transformed, errors: $errors, not a fixed point: $unstable"
echo ""
printf '%-40s %-20s %-10s %7s %8s %11s\n' \
  "FILE" "ZIP" "OUTCOME" "RENAMED" "WARNINGS" "FIXED POINT"
printf '%-40s %-20s %-10s %7s %8s %11s\n' \
  "----" "---" "-------" "-------" "--------" "-----------"
for row in "${rows[@]}"; do
  IFS=$'\t' read -r f z out ren warn fixed <<<"$row"
  printf '%-40s %-20s %-10s %7s %8s %11s\n' "$f" "$z" "$out" "$ren" "$warn" "$fixed"
done
echo ""
echo "Beam validation: unknown vertices=$unknown_verts, duplicate beams=$dup_beams"
echo ""
echo "Files are in: $TRANSFORM_DIR"
