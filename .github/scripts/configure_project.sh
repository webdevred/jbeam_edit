#!/usr/bin/env bash

set -euo pipefail

IS_EXPERIMENTAL=0

cabal configure --project-file cabal.project.release -O1

if [[ "$LABEL" == "" ]]; then
  echo "LABEL must be set"
elif [[ "$LABEL" == "experimental" ]]; then
  IS_EXPERIMENTAL=1
fi

echo "package jbeam-edit" >>cabal.project.release.local
echo "  tests: True" >>cabal.project.release.local

if echo "$MATRIX_FLAGS" | grep -qa ' +transformation '; then
  echo "  benchmarks: True" >>cabal.project.release.local
fi

if [[ $IS_EXPERIMENTAL -eq 1 ]]; then
  echo "  flags: $MATRIX_FLAGS" >>cabal.project.release.local
fi

echo "contents of cabal.project.release.local"
cat cabal.project.release.local
