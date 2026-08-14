#!/usr/bin/env bash
# tools/extract-and-format-jbeam/corpus-extract.sh
#
# Extracts every .jbeam file from every BeamNG vehicle zip into one flat
# directory. Nothing is formatted, and nothing is written inside the repo.
#
# Usage: bash tools/extract-and-format-jbeam/corpus-extract.sh [out-dir]
#
# out-dir defaults to ${TMPDIR:-/tmp}/jbeam-corpus and is wiped first.
#
# Output files are named <vehicle>__<basename>, because the same basename
# turns up in several vehicles and a flat directory would otherwise keep only
# the last one.
#
# This is not extract-tune-examples.sh. That one takes a curated list, formats
# each file and writes into jbeam-examples/ for demo and tuning work. Use this
# one when the question is how common something is, where a handful of files
# gives a confidently wrong answer.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

source "$SCRIPT_DIR/lib/beamng.sh"

OUT_DIR="${1:-${TMPDIR:-/tmp}/jbeam-corpus}"

# The corpus is wiped on every run, so refuse anything that is not a private
# scratch directory. A stray argument here would otherwise delete real work.
case "$OUT_DIR" in
  "" | "/" | "$HOME" | "$HOME/")
    echo "error: refusing to wipe $OUT_DIR" >&2
    exit 1
    ;;
  "$REPO_ROOT" | "$REPO_ROOT"/*)
    echo "error: $OUT_DIR is inside the repo, pick a path outside it" >&2
    exit 1
    ;;
esac

VEHICLES_DIR="$(beamng_find_vehicles_dir)"
if [[ -z "$VEHICLES_DIR" ]]; then
  echo "error: could not find BeamNG vehicles directory" >&2
  exit 1
fi

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

vehicles=0
extracted=0
errors=0

for zip_path in "$VEHICLES_DIR"/*.zip; do
  [[ -f "$zip_path" ]] || continue
  vehicle="$(basename "$zip_path" .zip)"
  vehicles=$((vehicles + 1))

  while read -r inner; do
    [[ -n "$inner" ]] || continue
    out="$OUT_DIR/${vehicle}__$(basename "$inner")"
    if beamng_extract_file "$zip_path" "$inner" "$out" 2>/dev/null; then
      extracted=$((extracted + 1))
    else
      # unzip -p creates the target before it fails, so clean up.
      rm -f "$out"
      echo "skip: $inner in $vehicle" >&2
      errors=$((errors + 1))
    fi
  done < <(beamng_list_jbeam_files "$zip_path")
done

echo "vehicles: $vehicles, files: $extracted, errors: $errors"
echo "corpus: $OUT_DIR"
