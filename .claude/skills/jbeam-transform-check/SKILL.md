---
name: jbeam-transform-check
description: Test the transformation feature against real BeamNG structural files (frame, suspension, body). Checks whether transformation succeeds, validates beam references, and reports regressions or crashes. Only targets files with positional node data — not engine, gauges, or interior files.
argument-hint: "[vehicle-filter]"
allowed-tools: Bash, Glob, Grep, Read, Write, Edit
---

# /jbeam-transform-check

You are running the **jbeam-transform-check** skill. Follow these steps exactly.

**Language**: All content written to TRANSFORMATION-TODO.md must be written in English.

**File lists**: `tools/extract-and-format-jbeam/transform-check-files.txt` contains the structural files to test (one `<filename> <zip>` per line). You may add new files to this list when you find interesting candidates. You may remove a file if it behaves well and its patterns are already covered by existing test fixtures. Never edit `tools/extract-and-format-jbeam/demo-files.txt` unless the user explicitly asks.

**Important**: Transformation is designed for structural files only — those containing a `nodes` section with the `["id", "posX", "posY", "posZ"]` header. It is not intended for engine, gauges, interior, lights, or slots-only files. Filter aggressively.

---

## Step 1 — Parse arguments

Parse the skill arguments:
- Any non-flag word is the vehicle filter (e.g. `autobello`, `covet`, `pickup`). Empty means sample broadly (3-5 vehicles).
- `--cross-file` flag sets CROSS_FILE=true. Default: false.

Store as:
- `FILTER` -- the filter string (may be empty)
- `CROSS_FILE` -- true or false

Always use the default transformation config (built-in defaults, no `.jbeam-edit.yaml`).

---

## Step 2 — Run transform-run.sh

```bash
bash tools/extract-and-format-jbeam/transform-run.sh [--cross-file] [transform-check-files.txt] [FILTER]
```

- Pass `--cross-file` if CROSS_FILE is true.
- Pass FILTER as the second positional argument if set.
- The script prints `TRANSFORM_DIR=<path>` on the first line — capture it.
- It extracts files, runs `--transform` on each, runs `--validate-beams`, and prints a summary table.

Read the summary table and `$TRANSFORM_DIR/*.err` files for per-file warnings.

---

## Step 3 — Validate output

For each file that transformed successfully, read the diff to understand what changed:

```bash
diff "$TRANSFORM_DIR/<file>.orig" "$TRANSFORM_DIR/<file>"
```

If CROSS_FILE: check for missed references by grepping other files in `TRANSFORM_DIR` for old node names.

**Interpreting beam validation results:**

- Unknown vertices from files not in the directory (e.g. engine nodes `e1`, wheel nodes `fw1`) are expected when only structural files were extracted. Only flag unknowns where the node should exist in an extracted file.
- Duplicate beams are worth noting but may be intentional in BeamNG.

---

## Step 4 — Investigate warnings before classifying

Before classifying any file as having a problem, investigate every warning or unexpected output from stderr. Do not assume a warning indicates a bug without reading the code that produces it.

For each warning or unexpected message:
1. Find the exact line in the source that emits it.
2. Trace the full data flow backwards — what value triggers this branch? What produces that value?
3. Only after completing the trace: is this a genuine problem, expected output, or a known artifact?

**Do not report a finding about transformation code unless you have traced the full code path and can point to the specific line where the problem originates.**

## Step 4b — Classify results

For each file, assign one of these outcomes:

| Outcome                  | Meaning                                                    |
|--------------------------|------------------------------------------------------------|
| `success`                | Transformed, parses, beams valid, no missed references     |
| `success-with-warnings`  | Transformed but stderr had warnings                        |
| `parse-error`            | Input file failed to parse before transformation           |
| `transform-error`        | Transformation logic failed (wrong header, empty tree, crash) |
| `output-invalid`         | Output was written but no longer parses                    |
| `missed-references`      | Old node names remain in related files                     |
| `invalid-beams`          | Beam validation found unexpected unknown vertices          |
| `duplicate-beams`        | Beam validation found duplicate beams with identical metadata |

---

## Step 5 — Write TRANSFORMATION-TODO.md

Edit `TRANSFORMATION-TODO.md` in the repo root **in place**. Do not append new session blocks — replace the existing content to reflect the current state. The file has this structure:

```markdown
# jbeam-transform-check -- last run <date>

## Files tested

| File | Vehicle | Size | Outcome | Nodes renamed | Invalid beams | Duplicate beams | Notes |
|------|---------|------|---------|---------------|---------------|-----------------|-------|
| ...  | ...     | ...  | ...     | ...           | ...           | ...             | ...   |

## Observations

<Only observations that are still true. Remove anything that has been fixed.>

---

## Priorities

### 1. <title>
<description>

### 2. <title>
...

---

## Done

### <title>
<description>
```

Rules for editing:
- **Files tested table**: Add new files from this session. Keep files from prior runs. Update outcomes if a file was re-tested.
- **Observations**: Keep only observations that are still present. Remove anything that has been fixed since it was recorded.
- **Priorities**: Keep items that are still open. Move solved items to the Done section (remove the ordinal, keep the description). Re-number remaining items. Try to keep ~5 active priorities if there are enough open problems.
- **Done**: Append newly solved items. Keep existing done items as-is.

---

## Step 6 — Report to user

Print a concise summary:
- Files tested, breakdown by outcome
- Any crashes or unexpected failures with details
- Whether cross-file reference updating worked correctly
- Beam validation results: invalid references and duplicate beams found
- Top suggestion for improving robustness
- Path to TRANSFORMATION-TODO.md
