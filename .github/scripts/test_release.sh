#!/usr/bin/env bash

source ./.github/scripts/copy_example_files.sh
source ./.github/scripts/copy_exe_to_release.sh

TMP_DIR=$(mktemp -d)
git show HEAD:"./examples/jbeam/fender.jbeam" > "$TMP_DIR/fender.${LABEL}.jbeam"
git show HEAD:"./examples/jbeam/suspension.jbeam" > "$TMP_DIR/suspension.${LABEL}.jbeam"

echo "fender.jbeam: $TMP_DIR/fender.${LABEL}.jbeam"
echo "suspension.jbeam: $TMP_DIR/suspension.${LABEL}.jbeam"

./dist/release/jbeam-edit -i "$TMP_DIR/fender.${LABEL}.jbeam"
./dist/release/jbeam-edit -i "$TMP_DIR/suspension.${LABEL}.jbeam"

if [[ -n $LABEL ]] && [[ "$LABEL" == "experimental" ]]; then
  diff -q "$TMP_DIR/fender.experimental.jbeam" ./examples/transformed_jbeam/fender-cfg-default.jbeam
  diff -q "$TMP_DIR/suspension.experimental.jbeam" ./examples/transformed_jbeam/suspension-cfg-default.jbeam
else
  diff -q "$TMP_DIR/fender.stable.jbeam" ./examples/formatted_jbeam/fender-minimal-jbfl.jbeam
  diff -q "$TMP_DIR/suspension.stable.jbeam" ./examples/formatted_jbeam/suspension-minimal-jbfl.jbeam
fi
