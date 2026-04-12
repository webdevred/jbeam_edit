#!/usr/bin/env bash
# tools/extract-and-format-jbeam/tune-list-candidates.sh
#
# Lists all .jbeam files inside BeamNG vehicle zips, with their
# compressed size, as TSV: filename <TAB> zip <TAB> size_bytes
#
# Usage: bash tools/extract-and-format-jbeam/tune-list-candidates.sh [filter]
#
# filter: optional substring to match against zip names (e.g. "autobello")
#         If omitted, all zips are listed.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/lib/beamng.sh"

FILTER="${1:-}"

VEHICLES_DIR="$(beamng_find_vehicles_dir)"
if [[ -z "$VEHICLES_DIR" ]]; then
    echo "error: could not find BeamNG vehicles directory" >&2
    exit 1
fi

for zip_path in "$VEHICLES_DIR"/*.zip; do
    zip_name="$(basename "$zip_path")"
    if [[ -n "$FILTER" && "$zip_name" != *"$FILTER"* ]]; then
        continue
    fi
    # Parse unzip -l output: length and filename for .jbeam files
    unzip -l "$zip_path" | awk -v zip="$zip_name" '
        /\.jbeam$/ {
            size = $1
            # Filename is the last field
            fname = $NF
            # Strip directory prefix, keep just the basename
            n = split(fname, parts, "/")
            base = parts[n]
            print base "\t" zip "\t" size
        }
    '
done | sort -t$'\t' -k3 -n
