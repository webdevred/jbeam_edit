# Node transformation

The `--transform` flag renames and reorganizes nodes in a structural JBeam file. It classifies every node as left, middle, right, or support based on its X position and beam connectivity, then assigns new sequential names and updates every beam, triangle, and flexbody reference in the file to match.

This document explains what the tool does and how to configure it.

---

## When to use it

Transformation targets structural files containing positional node data: frames, suspension arms, subframes, chassis rails. The file must have a `nodes` section with the `["id", "posX", "posY", "posZ"]` header.

It is not intended for any file where nodes have semantic names like `int_strsl`, `dshsl`, `e1`, `cam`, which is common in engine files, gauges and interior parts. Those names carry meaning that transformation does not understand and will overwrite.

---

## How to run it

```
jbeam-edit --transform yourfile.jbeam
```

This writes the result to `yourfile.jbeam` and saves the original as `yourfile.bak.jbeam`. Pass `-i` to skip the backup and overwrite in place directly.

To override what prefix names the tool generates, pass `--update-names`:

```
jbeam-edit --transform --update-names rl_f:rl_front yourfile.jbeam
```

This tells the tool that when it would generate a prefix derived from `rl_f`, use `rl_front` instead. The result would be names like `rl_frontl0`, `rl_frontl1`, ... rather than `rl_fl0`, `rl_fl1`, ...

---

## Configuration

Transformation reads `.jbeam-edit.yaml` in the working directory if present. Without it, built-in defaults are used. Create this file to override any parameter.

[`examples/jbeam-edit.yaml`](examples/jbeam-edit.yaml) is a working one to copy. It is the config the test suite transforms every example file with, so it cannot drift out of date the way a block quoted here would.

Parameter reference:

| Key                       | Default | Description                                                                      |
|---------------------------|---------|----------------------------------------------------------------------------------|
| `y-sorting-threshold`     | 0.05    | Y distance (meters) below which two nodes are treated as the same depth band     |
| `x-sorting-threshold`     | off     | X distance (meters) below which two nodes in one Y band count as the same column |
| `support-threshold`       | 96      | Minimum beam count as a percentage of group size to classify a node as support   |
| `max-support-coordinates` | 3       | Maximum number of support node candidates examined per spatial group             |
| `x-group-breakpoints`     | ±0.09   | Rules that map X coordinate to Left, Middle, or Right (see Left, Middle, Right)  |

`x-sorting-threshold` is off unless you set it, and `off` is the only word it
accepts besides a distance. There is no number that turns it off: `0` gives
every node its own column, which is the most column sorting rather than none.

### Picking the two sorting thresholds

Both settings are distances in meters, so `0.05` means 5 cm.

They also work the same way, and it is worth knowing how. The tool goes through
the nodes in order and starts a new group as soon as a node is at least the
threshold away from **the first node of the group it is currently filling**. It
does not compare each node to the one right before it. That is deliberate: a
long gentle slope would otherwise chain together into one enormous group.

**Start with `y-sorting-threshold`.** It decides how much front to back
variation still counts as the same row of nodes. When you place a row across a
panel the nodes are never at exactly the same Y, and without a threshold a
millimetre of difference would decide the order, which would also make the two
sides of the car come out differently.

The number you want is the depth of one row, not the space between rows. The
default 5 cm suits a row you placed carefully on a flat face. A curved panel
needs more, because the row follows the curve.

[`examples/regression_jbeam/y-sorting-repro.jbeam`](examples/regression_jbeam/y-sorting-repro.jbeam)
shows what that looks like. Its five frontmost left side nodes are one row as
far as the modeller is concerned, but they cover a fair bit of depth:

| Measurement                                   | Value |
|-----------------------------------------------|-------|
| Depth of the front row, -1.967 back to -1.791 | 0.176 |
| Front row's first node back to the next row's | 0.323 |
| Space between the two rows, -1.791 to -1.644  | 0.147 |
| Biggest step inside the front row             | 0.138 |

Anything above 0.176 and up to 0.323 keeps that row together, so 0.31 is a
comfortable pick and it is what the regression test uses. The default 0.05
splits the row in two. Note the trap again: the space between the rows is 0.147,
barely more than the 0.138 step inside the row, so a threshold picked from the
space between rows lands in the wrong place.

| What you see                                | What to do                                   |
|---------------------------------------------|----------------------------------------------|
| One row comes out ordered front to back     | Raise it, the row is deeper than you thought |
| Nodes at clearly different depths are mixed | Lower it                                     |

**Then `x-sorting-threshold`.** It does the same thing sideways, inside each
row. Turn it on if the file zigzags: the heights climb, drop back down and climb
again. That happens when one row covers two vertical columns of nodes, say the
nose face of a panel and the fender beside it, and the tool has nothing but
height to go on.

Here is the part that trips people up. **The number you want is the width of a
column, not the space between the columns.** Because the tool measures from the
first node of a group, the threshold has to be a bit wider than your widest
column, and no wider than the step from one column's innermost node across to
the next column's innermost node.

Those same five nodes show it. Once they are in one row, the inner column sits
at X 0.780, 0.920 and 0.953, and the outer one at 0.998 and 1.036:

| Measurement                                    | Value |
|------------------------------------------------|-------|
| Width of the inner column, 0.780 out to 0.953  | 0.173 |
| Inner column's innermost across to the outer's | 0.218 |
| Space between the two columns, 0.953 to 0.998  | 0.045 |
| Biggest step inside the inner column           | 0.140 |

So anything above 0.173 and up to 0.218 does the job, and 0.2 is the obvious
pick. Notice that the space between the columns is only 0.045, smaller than a
step inside the inner column. Set 0.045 and the tool splits the inner column in
two, leaves 0.780 on its own, and you get a third wrong order rather than the
right one. The space between the columns is the number that looks right, so it
is worth measuring the column itself instead.

To find the number for your own vehicle, transform once with the setting off,
find a spot where the heights zigzag, and read the X values of those rows. The
two columns separate by eye. Take the first and last X of the wider column,
subtract, and pick something a little above that. Transform again and the zigzag
should be gone.

If it still looks wrong:

| What you see                               | What to do                                   |
|--------------------------------------------|----------------------------------------------|
| Nothing changed at all                     | The threshold is too big, try a smaller one  |
| Runs of one or two nodes, still zigzagging | The threshold is too small, try a bigger one |
| Still zigzagging whatever you set          | Raise `y-sorting-threshold` first, see below |

That last one is worth checking early, because no X value can fix it. Columns
only exist inside a row, so if your two columns sit further apart front to back
than `y-sorting-threshold` allows, they never end up in the same row and
`x-sorting-threshold` never gets to look at them together.

Three ways this file can fail without saying much:

The file is read from the directory you run the command in, not from the directory holding the file you are transforming. Run the tool from somewhere else and you get the defaults.

`support-threshold` is required. Leave it out and the whole file fails to parse. The tool prints one line to stderr, uses every built-in default and still exits 0, so a typo in that one key throws away all your other settings.

Unknown keys are ignored, so a misspelled key looks like it worked.

---

## Before and after

This example uses `examples/jbeam/frame.jbeam`, a chassis rail with 55 nodes all originally named under the `rl_f*` and `rl*` prefixes.

**Before (excerpt):**

```jbeam
["rl_f0",  0.523,   -2.098,  0.319],
["rl_f1",  -0.417,  -2.098,  0.319],
["rl_f2",  0.053,   -2.095,  0.319],
["rl_f3",  0.523,   -2.093,  0.268],
["rl_f4",  -0.417,  -2.093,  0.268],
["rl_f5",  0.053,   -2.090,  0.269],
["rl_f14", 0.053,   -1.314,  0.382],   // support for front
["rl15",   0.790,   -0.919,  0.182],
["rl_r37", 0.651,   1.152,   0.565],
["rl_r48", 0.053,   1.710,   0.565],   // support for rear
```

**After (excerpt):**

```jbeam
// Left side

// prefix group rl_fl
["rl_fl0", 0.523,  -2.093, 0.268],
["rl_fl1", 0.523,  -2.098, 0.319],
...

// prefix group rll
["rll0",   0.790,  -0.919, 0.182],
...

// Middle side

// prefix group rl_fm
["rl_fm0", 0.053,  -2.090, 0.269],
["rl_fm1", 0.053,  -2.095, 0.319],
...

// Right side
...

// Support nodes
["rl_fsm", 0.053,  -1.314, 0.382],   // support for front
["rl_rsm", 0.053,  1.710,  0.565],   // support for rear
["rlsm",   0.053,  -0.024, 0.578],   // support
```

All beam, triangle, and flexbody rows in the same file are updated to use the new names.

---

## How classification works

### Left, Middle, Right

Each node is classified by its X coordinate using the configured breakpoints. With the defaults:

- x >= 0.09 - Left
- -0.09 < x < 0.09 - Middle
- x <= -0.09 - Right

BeamNG vehicles are modeled with positive X on the driver's left side. A threshold of ±0.09 meters excludes narrow centerline nodes from being assigned to either side.

You can adjust breakpoints in `.jbeam-edit.yaml` to match your vehicle's proportions. The breakpoints are evaluated in order; the first match wins. Only `LeftTree`, `MiddleTree`, and `RightTree` are valid values - `SupportTree` is assigned separately.

### Support nodes

Within each spatial group (Left, Middle, Right), the tool counts how many beams connect to each node, considering only beams where both endpoints are in the same file. The `max-support-coordinates` most-connected nodes in each group become candidates. A candidate is classified as support if its beam count is at least `support-threshold` percent of the group's total node count.

For example, with 50 nodes in the Left group and `support-threshold: 96`, a candidate needs at least `round(0.96 * 50) = 48` beam connections to qualify as a support node.

Beams are counted across the whole file, not just inside the group. A centreline node tying the two sides together gets all of them counted, so the figure is not capped by the group size and can go above 100 percent of it.

The threshold also scales with the group. A group of 5 needs 5 connections, a group of 50 needs 48. The same percentage is easy to reach in a small group and hard in a large one.

Support nodes are structural hubs - nodes that many beams radiate from. They are extracted from their spatial group and placed in a separate Support section at the end of the nodes list.

---

## How sorting works

Nodes within each group are sorted by three coordinates in order:

1. **Y (front to back)** - nodes at more negative Y (further forward) come first, grouped into depth bands. The group is walked in Y order and a new band starts as soon as a node sits `y-sorting-threshold` or further from the node that opened the current band (default 5 cm).
2. **Z (height)** - within the same Y band, lower nodes come first.
3. **X (side offset)** - within the same Y and Z band, nodes are sorted by X.

A band starts at the node that opened it, not at the previous node, so a long run of small steps cannot chain into one band much wider than the threshold.

With `x-sorting-threshold` set, each Y band is banded a second time before step 2, by X and by the same opener rule. Nodes in the same Y band but different columns then come out one column at a time, lower node first within each. Without it a band that spans two columns is ordered by height alone, so the output climbs one column, jumps to the other and comes back. That is what the setting is for: on a body panel the nose face and the fender beside it can sit at heights that interleave, and no Y threshold separates them because they are at the same depth.

The threshold is there because nodes you placed at one depth are rarely at exactly the same Y. Without it, a millimetre of difference would order them by Y instead of by height, and the two sides of a symmetric vehicle would come out in different orders.

---

## How naming works

The tool derives a new name for each node from its original prefix (the name with trailing digits removed) and its spatial group.

**Non-support nodes:**

- If the original prefix ends in `l`, `m`, or `r` (an existing side letter), the side letter is replaced with the new side suffix.
  - Example: `rl_r*` nodes on the left side → prefix becomes `rl_l`, producing `rl_l0`, `rl_l1`, ...
- Otherwise, the side suffix is appended directly.
  - Example: `rl_f*` nodes on the left side → prefix becomes `rl_fl`, producing `rl_fl0`, `rl_fl1`, ...

Nodes with the same derived prefix form a prefix group and share sequential indices starting at 0.

**Support nodes:**

Support nodes are first processed by `updateSupportVertexName`, which drops the trailing digits and appends the side letter of their spatial group (`l`, `m`, or `r`). The resulting name is then used as the prefix input to the naming step, which inserts an `s` to mark them as support.

- Example: `rl_f14` at x=0.053 (Middle) → intermediate name `rl_fm` → final name `rl_fsm`
- Example: `rl19` at x=0.053 (Middle) → intermediate name `rlm` → final name `rlsm`
- Example: `rl_r48` at x=0.053 (Middle) → intermediate name `rl_rm` → final name `rl_rsm`

The first support node in a prefix group gets no index, and the rest are numbered from 1: `rl_fsm`, `rl_fsm1`, `rl_fsm2`. A group with only one support node therefore ends at `rl_fsm`.

**Prefix comments:**

When a spatial group contains more than one prefix group, the tool inserts a `prefix group <name>` comment before each group. This makes it easy to see where the original prefixes mapped to.

---

## Cross-file reference updating

When transformation renames nodes, it also scans every other `.jbeam` file in the same directory and updates any references to the old names. No extra flags are needed - this happens automatically as part of `--transform`.

Use `--validate-beams` to check whether any beam in a file references an unknown node:

```
jbeam-edit --validate-beams yourdir/yourfile.jbeam
```

Without a filename argument, all `.jbeam` files in the directory are validated. The tool loads all files in the directory as the known vertex set, so cross-file references are checked correctly.

---

## Limitations

**Named nodes are not protected.** Transformation renames every node in the file without distinction, so a file that mixes structural nodes with semantically named ones (`int_strsl`, `dshsl`, `rm_*`) loses those names. Check a file for named nodes before transforming it. An `excludePrefixes` config option is planned to protect them.

**The transformation feature is experimental.** It is not included in the standard release binary. To use it, you need to build from source with the `transformation` flag enabled, or download a build that explicitly includes it.

**Multi-section files are handled.** Files with multiple `nodes` sections (common in suspension files) are all processed correctly.

**Metadata is preserved.** Per-node metadata objects (properties attached directly to a node row) are moved with the node and deduplicated against the section-level metadata above them.
