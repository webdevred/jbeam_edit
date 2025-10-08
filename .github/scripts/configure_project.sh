#!/usr/bin/env bash

set -euo pipefail

IS_TAG=0
IS_EXPERIMENTAL=0

if [[ "$GITHUB_REF" == "" ]]; then
  echo "GITHUB_REF must be set"
elif [[ "$GITHUB_REF" =~ ^refs/tags/ ]]; then
  IS_TAG=1
  cabal configure --project-file cabal.project.release -O2
else
  cabal configure --project-file cabal.project.release
fi

if [[ "$LABEL" == "" ]]; then
  echo "LABEL must be set"
elif [[ "$LABEL" == "experimental" ]]; then
  IS_EXPERIMENTAL=1
fi

if [[ $IS_EXPERIMENTAL -eq 1 ]] || [[ $IS_TAG -eq 0 ]]; then
  echo "package jbeam-edit" >>cabal.project.release.local
  if [[ $IS_EXPERIMENTAL -eq 1 ]]; then
    echo "  flags: $MATRIX_FLAGS" >>cabal.project.release.local
  fi
  if [[ $IS_TAG -eq 0 ]]; then
    echo "  tests: True" >>cabal.project.release.local
  fi
fi

echo "contents of cabal.project.release.local"
cat cabal.project.release.local
