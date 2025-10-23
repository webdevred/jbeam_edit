#!/usr/bin/env bash

set -euo pipefail

cabal configure --project-file cabal.project.release -O1

printf "package jbeam-edit\n  tests: True\n" >>cabal.project.release.local
if ! [[ "$MATRIX_FLAGS" == "" ]]; then
  echo "  flags: $MATRIX_FLAGS" >>cabal.project.release.local
fi

echo "contents of cabal.project.release.local"
cat cabal.project.release.local
