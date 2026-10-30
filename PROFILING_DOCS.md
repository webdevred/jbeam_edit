# Profiling

Profiling needs `profiling: True` and `profiling-detail: all-functions` in `cabal.project`. Neither belongs in a commit. Take them out again when you are done.

Each of the five points below has led to a wrong conclusion at least once.

**Read allocation first.** Two runs of the same binary on the same input allocate the same number of bytes. They differ by about a hundred bytes out of several hundred megabytes. Tick counts move by several percent between the same two runs. A typical run is only a few hundred ticks, which makes a cost centre at 2% two or three ticks. Rank by `%alloc` and treat `%time` as an indication.

**Never profile against `examples/`.** Every run rewrites the file it is given, with or without `-i`. The option only skips the `.bak.jbeam` copy that is otherwise made first (`createBackupFile` in `Main.hs`). There is no read-only mode, and a run that looks like a measurement is an edit. `--transform` also rewrites every `.jbeam` beside the file you name, which means one run overwrites all three fixtures. The next run then reads something different, because an already-transformed file is shorter. Two consecutive runs are not comparable. Copy the file to a directory of its own first, as described under "Investigating a transformation bug" in `CLAUDE.md`.

**Profile the large tail too.** `examples/jbeam/frame.jbeam` is close to a typical stock file. It is a fair sample of the common case and says nothing about the expensive one. Extract the corpus with `tools/extract-and-format-jbeam/corpus-extract.sh` and measure the sizes before choosing. The median file is around 11 KB and the 90th percentile around 44 KB. The largest is half a megabyte, roughly 36 times the frame example. Anything in the rule lookup that scales worse than linearly is invisible at the median and obvious at the tail. Profile one file from each end. Copy them out of the corpus directory first, for the same reason as above.

**Give the binary its data directory and keep stderr.** The shipped ruleset is found through `getDataFileName` (`Formatting/Config.hs`). A binary run from outside the build tree therefore finds no `examples/jbfl/minimal.jbfl` and formats with no rules at all. Rule lookup is about two thirds of all allocation. The profile then measures something the tool never does: `frame.jbeam` allocated 15 MB without rules and 62 MB with them. Export `jbeam_edit_datadir=<repo>` when running the binary directly. Keep stderr, because a failed lookup announces itself there. Do not use `Loading jbfl:` as a sign. It is printed only when `--rules-path` is given, and its absence means nothing on an ordinary run.

`--rules-path` does not get you out of this. The named file is loaded and then discarded when the shipped default cannot be found. The run formats with no rules while reporting that it loaded yours. The only sign is a second stderr line, `Failed to parse default ruleset`. This applies to any comparison between rulesets, not only to profiling. Without the data directory every variant produces byte-identical output, which reads as "the rules make no difference".

**Rebuild fully when build flags change.** A partial rebuild leaves modules compiled under the old flags, and the result looks like a real difference. If allocation moves by more than a rounding error between two runs that should be identical, suspect the binary before believing the number.

## Baseline

Measured with `--transform` and the shipped ruleset loaded. The figure is `bytes allocated in the heap` from `+RTS -s`. Reproduce it before trusting a comparison against it.

The conditions decide the numbers and belong to the measurement:

- GHC 9.14.1.
- `-O1`, because that is what ships (`configure_project.sh` passes it). A `-O0` baseline describes a build no user ever gets.
- containers 0.8.
- master at `6e9c96f`.

Optimization is worth a factor of three and the compiler version four to five percent. A figure quoted without these conditions says nothing.

| Input                                       | Size      | Allocation |
|---------------------------------------------|-----------|------------|
| `examples/jbeam/frame.jbeam`, alone         | 14 236 B  | 23 MB      |
| a 150 KB stock body file, alone             | 149 896 B | 173 MB     |
| the same body file in its vehicle, 74 files | 1.4 MB    | 1 465 MB   |

The third row describes the tool as users run it. `--transform` rewrites every neighbouring file that referenced a renamed node (37 of the 74 here), and rewriting means reformatting the file in full. The formatter therefore runs 38 times for one command. `updateOtherFiles` inherits two thirds of all allocation, while the renaming inside it costs well under one percent. Anything saved in the rule lookup is saved 38 times over. Making that lookup a trie took these three rows down by about sixty percent.

Two points follow, and both have been mistaken for something else before. Allocation runs one to two thousand bytes per byte of input. The absolute figures look alarming and say nothing on their own. A tenfold larger input allocates eight times as much. The cost is slightly sublinear. There is no asymptotic problem hiding in the rule lookup, only a large constant.

The constant was profiled once at `-O0`, before the rule lookup became a trie. Three things made up about two thirds of all allocation:

- a linear sweep over every stored pattern
- the `Seq` pattern match that compared each one against the cursor
- `propertyName` rebuilding a `Text` from a string literal on every `Eq SomeKey` comparison

The first two no longer exist. Treat these shares as history and measure before acting on them.
