# JBeam domain notes

How this tool models a JBeam file. Written for anyone changing the
transformation, and aimed at the parts of the format that decide its behaviour.
It is not a BeamNG modding guide: the tool formats and renames, it never reasons
about physics.

## Metadata rows are sticky

A bare object inside a `nodes` array sets properties on every node that follows
it:

```jbeam
{"nodeWeight":0.78},
["nl0", 0.953, -1.967, 0.122],
["nl1", 0.920, -1.953, 0.439],
```

Both nodes weigh 0.78. A later row overrides the same key for everything after
it, so the last write wins. A row placed ahead of the first node applies to the
whole section.

This is the single fact behind most transformation bugs, because it hides them.
The transformation only ever emits the difference against the previous vertex,
so a key the model dropped is usually still standing in the output and still
applies. The file reads correctly while the model is wrong. The error surfaces
only when the key is set again to a different value further down, or when the
metadata map is used as a sort key, which it is.

## Sections and axes

`nodes`, `beams` and `triangles` are the sections the transformation
understands. Everything else passes through untouched.

A `nodes` row is `["name", posX, posY, posZ]`, and the first row is the header
`["id", "posX", "posY", "posZ"]`. X is left and right, Y is front and back with
negative pointing forward, Z is up.

Comments and trailing commas are content, not noise. The parser records them and
the formatter writes them back, so a fixture that has been tidied into valid
JSON no longer tests what it was written to test.

## How a nodes section is modelled

| step              | where                       | what it does                                    |
|-------------------|-----------------------------|-------------------------------------------------|
| split into chunks | `breakVertices`             | new chunk when the name prefix changes          |
| annotate          | `nodesToAnnotatedVertices`  | attach sticky metadata and comments per vertex  |
| pick a tree       | `determineGroup`            | Left, Middle or Right from posX via breakpoints |
| move support      | `moveSupportVertices`       | connectivity based, see below                   |
| sort              | `sortVertices`, `compareAV` | metadata, then Y band, then Z, then X           |
| rename            | `assignNames`               | prefix, side letter, index                      |

`breakVertices` splits on the **name prefix**, not on which side of the vehicle
a vertex sits. Two consequences follow, and together they decide which vertices
end up sharing a metadata block:

- A file that names both sides `rl_f` keeps them in one chunk.
- A file that alternates `nl0, nr1, nl2, nr3` gets one chunk per vertex.

That is why `examples/jbeam/frame.jbeam` cannot reproduce defects that a
gen4-style body file reproduces on the first run. Check the naming scheme before
concluding an existing example covers a case.

Support classification has two gates. `vertexConns` first keeps the
`max-support-coordinates` most connected vertices per tree type, and
`moveSupportVertices` then keeps those where
`count >= max 1 (round (support-threshold / 100 * groupSize))`. Connection counts
come from the `beams` section, so a fixture without beams gets no support
vertices at all.

Y sorting assigns an integer band: sort by Y, then walk once and increment the
band when a vertex sits `y-sorting-threshold` or further from the band's start.
Bands are compared rather than raw Y, which is what keeps the ordering
transitive.

## Configuration

`--transform` reads `.jbeam-edit.yaml` from the current working directory, not
from the directory holding the file being transformed.

| key                       | required | default |
|---------------------------|----------|---------|
| `support-threshold`       | yes      |         |
| `y-sorting-threshold`     | no       | 0.05    |
| `max-support-coordinates` | no       | 3       |
| `x-group-breakpoints`     | no       | ±0.09   |

Two failure modes worth knowing. Unknown keys are ignored without a word, so a
misspelled key looks like it took effect. And if the file fails to parse at all,
including when `support-threshold` is missing, the tool prints one line to
stderr, falls back to **every** default and still exits 0. A typo in the
required key therefore silently discards all the other settings.

## Input shapes that change behaviour

What to vary when building a fixture, or when deciding whether an existing
example covers a case. Vary one at a time.

| shape                                                   | why it matters                                                                    |
|---------------------------------------------------------|-----------------------------------------------------------------------------------|
| metadata row ahead of the first vertex                  | applies to the whole section, and only the first chunk sees it as a leading block |
| alternating vs shared name prefixes                     | decides where the chunk boundaries fall                                           |
| name ending in a letter (`rlsm`) vs a digit (`rlsm1`)   | letter-ending names get a `SupportKey`, digit-ending ones a `PrefixKey`           |
| a comment between a vertex and a following metadata row | `extractPreviousAssocCmt` only inspects the head of the accumulator               |
| Y gaps at, just under and just over the threshold       | the band boundary is `>=`, so an exact-threshold gap starts a new band            |
| already-transformed output fed back in                  | several defects need two runs to appear                                           |
| a `beams` section, or none                              | no beams means no support vertices                                                |

Regression fixtures live in `examples/regression_jbeam/`. Read that directory's
README first: they are not curated examples, and the bar for adding one is that
the test genuinely cannot be built from what is already there. Keep them to the
smallest node set that still shows the symptom, and use real coordinates from a
body file when the spacing is the thing being reproduced.
