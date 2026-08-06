#!/usr/bin/env bash

set -euo pipefail

CABAL_FILE="${CABAL_FILE:?Environment variable CABAL_FILE is required}"

GHC=$(stack ghc -- --numeric-version)

TESTED=$(awk -v field="tested-with" -f ./.github/script_helpers/get_cabal_field.awk "$CABAL_FILE" |
  grep -oE '[0-9]+\.[0-9]+\.[0-9]+')

if ! echo "$TESTED" | grep -qxF "$GHC"; then
  echo "Stack GHC $GHC is not listed in tested-with."
  echo "tested-with versions: $(echo "$TESTED" | tr '\n' ' ')"
  exit 1
fi

echo "Stack GHC $GHC matches tested-with."
