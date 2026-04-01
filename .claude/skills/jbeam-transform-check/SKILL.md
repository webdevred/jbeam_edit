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

## Step 1 — Session setup (ask the user)

Ask the following in a single message. Wait for answers before proceeding.

1. **Filter** (optional): Which vehicle(s) to look at? Examples: `autobello`, `covet`, `pickup`. Leave blank to sample broadly.
2. **Config**: Which transformation config to use? Options: `default` (built-in defaults) or path to a `.jbfl`-style config file. Default: `default`.
3. **Cross-file check**: Should I also look for related files (other `.jbeam` files in the same vehicle) that may contain beam references to renamed nodes? (yes / no)

Store answers as `FILTER`, `CONFIG`, `CROSS_FILE`.

---

## Step 2 — Find BeamNG vehicles directory

```bash
source tools/extract-and-format-jbeam/lib/beamng.sh && beamng_find_vehicles_dir
```

If empty, stop.

---

## Step 3 — Find candidate files

For each zip matching FILTER (or a broad sample of 3–5 vehicles if no filter):

```bash
source tools/extract-and-format-jbeam/lib/beamng.sh
beamng_list_jbeam_files "$VEHICLES_DIR/<zip>"
```

**Select only structural files** — apply this filter heuristic:

| Include | Reason |
|---------|--------|
| `*_frame*.jbeam` | Primary target — vertex trees, beams |
| `*_body*.jbeam` | Contains nodes/beams structural data |
| `*_suspension*.jbeam` | Suspension nodes and beams |
| `*_chassis*.jbeam` | Chassis structure |

| Exclude | Reason |
|---------|--------|
| `*_engine*.jbeam` | No positional nodes |
| `*_gauges*.jbeam` | Props/flexbodies only |
| `*_interior*.jbeam` | Props/flexbodies only |
| `*_lights*.jbeam`, `*_headlights*.jbeam` | Props only |
| `*_gearbox*.jbeam`, `*_transmission*.jbeam` | No positional nodes |
| `*_fueltank*.jbeam`, `*_bumper*.jbeam` | Usually very small, low signal |

**Verification step**: For each candidate, extract it and grep for `"posX"` to confirm it has a positional node header. Discard files that don't have it.

**Selection rules:**
- Aim for 4–8 files total.
- Prefer variety across vehicles.
- Prefer files under 60 KB.
- Print selection as a table: filename, zip, size, has-posX confirmed.

---

## Step 4 — Extract files

```bash
TRANSFORM_DIR="$(mktemp -d /tmp/jbeam-transform-XXXXXX)"
```

For each selected file:
```bash
source tools/extract-and-format-jbeam/lib/beamng.sh
beamng_extract_file "$VEHICLES_DIR/<zip>" "<inner_path>" "$TRANSFORM_DIR/<filename>"
cp "$TRANSFORM_DIR/<filename>" "$TRANSFORM_DIR/<filename>.orig"
```

If CROSS_FILE is true, also extract other `.jbeam` files from the same vehicle zip into the same directory, so beam references can be validated across files.

---

## Step 5 — Run transformation

For each structural file:

```bash
cabal run jbeam-edit --project-file=cabal.project.dev -- --transform "$TRANSFORM_DIR/<file>" 2>&1
```

If a config file is specified: pass it as appropriate (check CLI help with `cabal run jbeam-edit -- --help`).

Capture:
- Exit code (0 = success, non-zero = failure)
- stderr output (error messages, warnings)
- Whether the output file was written

---

## Step 6 — Validate output

For each file that transformed successfully:

1. **Parse check**: Run jbeam-edit again on the output (without `--transform`) to confirm it still parses:
   ```bash
   cabal run jbeam-edit --project-file=cabal.project.dev -- "$TRANSFORM_DIR/<file>" 2>&1
   ```

2. **Diff**:
   ```bash
   diff "$TRANSFORM_DIR/<file>.orig" "$TRANSFORM_DIR/<file>"
   ```

3. **Rename count**: Count how many node names changed (lines in diff with node id patterns).

4. **Reference check** (if CROSS_FILE): Search other files in `TRANSFORM_DIR` for old node names that should have been updated:
   ```bash
   grep -l "<old_node_name>" "$TRANSFORM_DIR"/*.jbeam
   ```
   If any old names survive in other files, that is a missed reference — flag it.

---

## Step 6b — Beam validation

After all transformations (and cross-file updates if CROSS_FILE) are complete, run beam validation on the entire `TRANSFORM_DIR`. This checks two things: that all beam node references point to defined vertices, and that there are no duplicate beams (same node pair with identical metadata).

```bash
cd "$TRANSFORM_DIR" && cabal run jbeam-edit --project-file=cabal.project.dev -- --validate-beams 2>&1
```

To validate a specific file only:
```bash
cd "$TRANSFORM_DIR" && cabal run jbeam-edit --project-file=cabal.project.dev -- --validate-beams "<file>" 2>&1
```

Capture stderr output. Beam validation reports two kinds of issues:

| Issue                   | Meaning                                                                     |
|-------------------------|-----------------------------------------------------------------------------|
| Unknown vertex          | A beam references a node not defined in any `.jbeam` file in the directory  |
| Duplicate beam          | Same node pair with identical metadata appears more than once               |

**Interpreting results:**

- Unknown vertices referencing nodes from files **not in the directory** (e.g. engine nodes `e1`, wheel nodes `fw1`) are expected when only structural files were extracted. Only flag unknown vertices where the node should exist in an extracted file.
- Duplicate beams are worth reporting but may be intentional in BeamNG (same pair under different metadata sections is normal - only truly identical metadata duplicates are flagged).
- If CROSS_FILE is true and all vehicle files were extracted, there should be very few unknown vertices.

Record per-file: number of unknown vertex warnings, number of duplicate beam warnings.

---

## Step 7 — Investigate warnings before classifying

Before classifying any file as having a problem, investigate every warning or unexpected output from stderr. Do not assume a warning indicates a bug without reading the code that produces it.

For each warning or unexpected message:
1. Find the exact line in the source that emits it.
2. Trace the full data flow backwards — what value triggers this branch? What produces that value?
3. Only after completing the trace: is this a genuine problem, expected output, or a known artifact?

**Do not report a finding about transformation code unless you have traced the full code path and can point to the specific line where the problem originates.**

## Step 7b — Classify results

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

## Step 8 — Write TRANSFORMATION-TODO.md

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

### 1st -- <title>
<description>

### 2nd -- <title>
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

## Step 9 — Report to user

Print a concise summary:
- Files tested, breakdown by outcome
- Any crashes or unexpected failures with details
- Whether cross-file reference updating worked correctly
- Beam validation results: invalid references and duplicate beams found
- Top suggestion for improving robustness
- Path to TRANSFORMATION-TODO.md
