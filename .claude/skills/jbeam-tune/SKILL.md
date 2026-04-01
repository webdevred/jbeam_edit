---
name: jbeam-tune
description: Extract JBeam files from BeamNG, format with jbeam-edit, diff and propose JBFL improvements
argument-hint: "[vehicle-filter]"
allowed-tools: Bash, Glob, Grep, Read, Write, Edit
---

# /jbeam-tune

You are running the **jbeam-tune** skill. Follow these steps exactly.

**Language**: All content written to TODO.md -- table entries, summaries, observations, proposals, priorities -- must be written in English.

**File lists**: `tools/extract-and-format-jbeam/tune-files.txt` contains the files to test (one `<filename> <zip>` per line). You may add new files to this list when you find interesting candidates. You may remove a file if it behaves well and its patterns are already covered by existing test fixtures. Never edit `tools/extract-and-format-jbeam/demo-files.txt` unless the user explicitly asks.

---

## Step 1 -- Session setup (ask the user)

Ask the user the following two questions **in a single message**. Wait for their answers before proceeding.

1. **Filter** (optional): Which vehicle(s) to look at? Examples: `autobello`, `covet`, `barstow`. Leave blank to sample across all vehicles.
2. **Proposals**: Should I suggest JBFL setting changes and new properties based on the diffs? (yes / no)

Store their answers as:
- `FILTER` -- the filter string (may be empty)
- `PROPOSALS` -- true or false

---

## Step 2 -- Find BeamNG vehicles directory

Run:
```bash
source tools/extract-and-format-jbeam/lib/beamng.sh && beamng_find_vehicles_dir
```

If the result is empty, tell the user and stop.

---

## Step 3 -- Enumerate candidate files

Run:
```bash
source tools/extract-and-format-jbeam/lib/beamng.sh
VEHICLES_DIR="<result from step 2>"
ls "$VEHICLES_DIR"/*.zip
```

For each zip matching FILTER (or all zips if FILTER is empty):
- Run `beamng_list_jbeam_files "$VEHICLES_DIR/<zip>"` to list its .jbeam files.

**Filtering -- pick files that likely have new patterns.** To decide which files are interesting:

1. Read `examples/jbfl/minimal.jbfl` to see which section names are already covered by JBFL rules.
2. Read TODO.md to see which files have already been tested.
3. Extract a small sample of candidate files and scan their section names (top-level keys that map to arrays or objects). Compare against the covered set from step 1.
4. Prioritize files that contain sections *not* covered by any JBFL pattern.

**Selection rules:**
- Aim for 5-10 files total.
- Prefer variety: different section types, different vehicles.
- Prefer smaller files (under ~30 KB) for faster iteration. Skip files over 80 KB unless they're the only representative of a pattern.
- Do not pick files whose base name already matches a file in `examples/jbeam/`.
- Prefer files from vehicles not already in TODO.md's files-tested table.

Print your selection to the user as a table: filename, zip, size, reason for selection.

---

## Step 4 -- Extract files

Create a temp directory:
```bash
TUNE_DIR="$(mktemp -d /tmp/jbeam-tune-XXXXXX)"
```

For each selected file, run:
```bash
source tools/extract-and-format-jbeam/lib/beamng.sh
beamng_extract_file "$VEHICLES_DIR/<zip>" "<inner_path>" "$TUNE_DIR/<filename>"
```

---

## Step 5 -- Format with jbeam-edit and diff

Always use the source `examples/jbfl/minimal.jbfl` as the JBFL config via `--rules-path`, not the user's installed `~/.config/jbeam_edit/rules.jbfl` (which may be stale).

For each extracted file:

1. Copy original: `cp "$TUNE_DIR/<file>" "$TUNE_DIR/<file>.orig"`
2. Run formatter:
   ```bash
   cabal run jbeam-edit --project-file=cabal.project.dev -- \
     --rules-path examples/jbfl/minimal.jbfl "$TUNE_DIR/<file>" 2>&1
   ```
   If jbeam-edit fails on a file, note the error and continue.
3. Diff:
   ```bash
   diff "$TUNE_DIR/<file>.orig" "$TUNE_DIR/<file>"
   ```

Collect all diffs.

---

## Step 6 -- Analyse diffs

For each file, classify the diff hunks into categories:

| Category      | Description                                             |
|---------------|---------------------------------------------------------|
| `whitespace`  | Only indentation / spacing changed -- expected, fine    |
| `number-format` | Number alignment / decimal padding                    |
| `newline`     | Extra or missing blank lines                            |
| `structure`   | Structural reordering or content change -- investigate  |
| `parse-error` | jbeam-edit failed to parse the file                     |

Summarise: how many lines changed per file, which categories appear.

**Coverage question**: After seeing the diffs, answer: "Does this file reveal a pattern not covered by existing test fixtures?" Note which patterns are new.

**JBFL gap check**: Read `JBFL_DOCS.md` for the full list of available JBFL properties and their semantics. For each diff hunk, check whether it could have been avoided by an existing JBFL property applied to the right path. Read `examples/jbfl/minimal.jbfl` and `examples/jbfl/complex.jbfl` and compare the paths they cover against the paths where diffs appeared. Any path with avoidable diffs that is missing from the JBFL configs is a gap. If PROPOSALS is true, fix the gaps directly in the JBFL files -- don't just propose them.

**JBFL style rule**: When editing JBFL files, put at most **two patterns per line**. If a rule block has more than two patterns, split them across lines with trailing commas.

**AST regeneration**: After editing any file under `examples/jbfl/` or `examples/jbeam/`, always regenerate AST fixtures before committing:
```bash
cabal run jbeam-edit-dump-ast --project-file=cabal.project.dev
```
Commit the regenerated AST files together with the JBFL/JBeam changes.

---

## Step 7 -- Update TODO.md

Edit `TODO.md` in the repo root **in place**. Do not append new session blocks -- replace the existing content to reflect the current state. The file has this structure:

```markdown
# jbeam-tune -- last run <date>

## Files tested

| File | Source zip | Size | Lines changed | Categories |
|------|-----------|------|---------------|------------|
| ...  | ...       | ...  | ...           | ...        |

## Observations

<Only observations that are still true. Remove anything that has been fixed.>

---

## Priorities

### 1. -- <title>
<description>

### 2. -- <title>
...

---

## Done

### <title>
<description>
```

Rules for editing:
- **Files tested table**: Add new files from this session. Keep files from prior runs. Update line counts if a file was re-tested.
- **Observations**: Keep only observations that are still present in the diffs. Remove anything that has been fixed since it was recorded.
- **Priorities**: Keep items that are still open. Move solved items to the Done section (remove the ordinal, keep the description). Re-number remaining items as 1., 2., etc. Try to keep 5 active priorities if there are enough open problems.
- **Done**: Append newly solved items. Keep existing done items as-is.

If PROPOSALS is true, also add to Observations. Only propose properties that exist in `JBFL_DOCS.md`. If the diff suggests a formatting behavior that no existing property can address, propose it as a new property.

```markdown
## Proposed JBFL changes

### Minimal config (rules.jbfl)
- **What**: ...
- **Why**: ...
- **Suggested syntax**: ...

## Proposed new JBFL properties
- **Name**: `SomeNewProperty`
- **Purpose**: ...
- **Example**: ...
```

---

## Step 8 -- Report to user

Print a concise summary to stdout:
- Files tested (list)
- Total lines changed across all files
- Top 3 most interesting observations
- If PROPOSALS is true: top proposal(s)
- Path to TODO.md
