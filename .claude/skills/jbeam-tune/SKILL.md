---
name: jbeam-tune
description: Extract JBeam files from BeamNG, format with jbeam-edit, diff and propose JBFL improvements
argument-hint: "[vehicle-filter] [--proposals]"
allowed-tools: Bash, Glob, Grep, Read, Write, Edit
---

# /jbeam-tune

You are running the **jbeam-tune** skill. Follow these steps exactly.

**Language**: All content written to TODO.md -- table entries, summaries, observations, proposals, priorities -- must be written in English.

**File lists**: `tools/extract-and-format-jbeam/tune-files.txt` contains the files to test (one `<filename> <zip>` per line). You may add new files to this list when you find interesting candidates. You may remove a file if it behaves well and its patterns are already covered by existing test fixtures. Never edit `tools/extract-and-format-jbeam/demo-files.txt` unless the user explicitly asks.

**Arguments**: Parse the skill arguments:
- Any non-flag word is the vehicle filter (e.g. `autobello`, `covet`). Empty means all vehicles.
- `--proposals` flag means PROPOSALS=true. Without it, PROPOSALS=false.

Store as:
- `FILTER` -- the filter string (may be empty)
- `PROPOSALS` -- true or false

---

## Step 1 -- List candidates (if picking new files)

If you need to find new files to add to `tune-files.txt`, run:
```bash
bash tools/extract-and-format-jbeam/tune-list-candidates.sh [filter]
```

This outputs TSV: `filename <TAB> zip <TAB> size_bytes`, sorted by size.

**Selection rules** (when picking new files to add):
- Aim for 5-10 files total in tune-files.txt.
- Prefer variety: different section types, different vehicles.
- Prefer smaller files (under ~30 KB). Skip files over 80 KB unless they're the only representative of a pattern.
- Do not pick files whose base name already matches a file in `examples/jbeam/`.
- Read `examples/jbfl/minimal.jbfl` to see which section names are covered. Prioritize files with uncovered sections.
- Read TODO.md to see which files have already been tested. Prefer files from vehicles not already there.

Print your selection as a table: filename, zip, size, reason. Then add them to `tune-files.txt`.

If `tune-files.txt` already has good coverage for the filter, skip this step and go directly to Step 2.

---

## Step 2 -- Extract, format, and diff

Run the tune script:
```bash
bash tools/extract-and-format-jbeam/tune-run.sh [file-list] [filter]
```

Defaults: file-list = `tools/extract-and-format-jbeam/tune-files.txt`, filter = empty.

The script:
1. Finds the BeamNG vehicles directory
2. Extracts each file from its zip
3. Formats with `jbeam-edit --rules-path examples/jbfl/minimal.jbfl`
4. Produces unified diffs

Output: prints a summary table and the path to `TUNE_DIR` containing `*.orig`, `*.diff`, and `summary.tsv`.

If the script fails to find BeamNG, tell the user and stop.

---

## Step 3 -- Analyse diffs

Read the diffs from `$TUNE_DIR/*.diff`. For each file, classify the diff hunks:

| Category        | Description                                          |
|-----------------|------------------------------------------------------|
| `whitespace`    | Only indentation / spacing changed -- expected, fine |
| `number-format` | Number alignment / decimal padding                   |
| `newline`       | Extra or missing blank lines                         |
| `structure`     | Structural reordering or content change -- investigate |
| `parse-error`   | jbeam-edit failed to parse the file                  |

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

## Step 4 -- Update TODO.md

Edit `TODO.md` in the repo root **in place**. Do not append new session blocks -- replace the existing content to reflect the current state. Make sure to always have points in the priorities list, if you are unable create a five point priority list clearly tell the user. Any point which has been fixed in this branch or in any open PR should be moved to DONE. The file has this structure:

```markdown
# jbeam-tune -- last run <date>

## Files tested

| File | Source zip | Size | Lines changed | Categories |
|------|-----------|------|---------------|------------|
| ...  | ...       | ...  | ...           | ...        |

## Observations

<Only observations that are still true. Remove anything that has been fixed, either in this branch or in open PRs.>

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

If PROPOSALS is true, also add to Observations.

**Default vs setting rule**: Be careful about proposing new JBFL properties for behaviors that should just be the formatter's default. But giving users a choice can be valuable -- it makes jbeam-edit and JBFL more flexible, and it lets maintainers tune formatting behavior per AST section without writing Haskell. When proposing a new property, always consider: what should the default be?

For properties that exist in `JBFL_DOCS.md`, propose JBFL rule changes directly. If the diff suggests a formatting behavior that no existing property can address, propose it as a new property.

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

## Step 5 -- Report to user

Print a concise summary to stdout:
- Files tested (list)
- Total lines changed across all files
- Top 3 most interesting observations
- If PROPOSALS is true: top proposal(s)
- Path to TODO.md
