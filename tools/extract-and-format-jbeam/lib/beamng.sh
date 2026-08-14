#!/usr/bin/env bash
# tools/extract-and-format-jbeam/lib/beamng.sh
#
# Shared helpers for working with BeamNG vehicle zips.
# Source this file: source "$(dirname "$0")/../lib/beamng.sh"

# Finds the BeamNG vehicles directory by scanning Steam library VDFs.
# Prints the path to stdout, or empty string if not found.
beamng_find_vehicles_dir() {
    local vdf candidates
    candidates=(
        "$HOME/snap/steam/common/.local/share/Steam/steamapps/libraryfolders.vdf"
        "$HOME/.steam/steam/steamapps/libraryfolders.vdf"
        "$HOME/.local/share/Steam/steamapps/libraryfolders.vdf"
    )
    for vdf in "${candidates[@]}"; do
        [[ -f "$vdf" ]] || continue
        while IFS= read -r line; do
            if [[ "$line" =~ \"path\"[[:space:]]+\"([^\"]+)\" ]]; then
                local vehicles="${BASH_REMATCH[1]}/steamapps/common/BeamNG.drive/content/vehicles"
                [[ -d "$vehicles" ]] && echo "$vehicles" && return
            fi
        done < "$vdf"
    done
    echo ""
}

# Lists all .jbeam file paths inside a zip.
# Usage: beamng_list_jbeam_files <zip_path>
beamng_list_jbeam_files() {
    local zip="$1"
    unzip -l "$zip" | awk '/\.jbeam$/ {print $NF}'
}

# Extracts a single file from a zip to a given output path.
# Usage: beamng_extract_file <zip_path> <inner_path> <out_path>
beamng_extract_file() {
    local zip="$1" inner="$2" out="$3"
    unzip -p "$zip" "$inner" > "$out"
}
