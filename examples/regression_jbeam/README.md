# Regression jbeam fixtures

Small `.jbeam` files that exist purely to reproduce a specific bug for a
regression test. Unlike `examples/jbeam/`, these are **not** written or
vetted by the jbeam maintainer, not curated demo material, and
not picked up by `jbeam-edit-dump-ast` (which only scans `examples/jbeam/`).
Don't treat them as examples of good jbeam, and don't add to this
directory unless a test genuinely needs a fixture that can't be built
from what's already in the project.
