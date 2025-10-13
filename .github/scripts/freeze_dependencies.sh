#!/usr/bin/env bash

set -euo pipefail

cabal freeze --project-file "${CABAL_PROJECT:?CABAL_PROJECT missing}"
PLAN_PATH=$(find dist-newstyle -name plan.json)
JQ_QUERY='."install-plan" | map (."pkg-name" | select (. != "jbeam-edit")) | unique'
jq -c "$JQ_QUERY" <"$PLAN_PATH"
printf "number of dependencies: %d" "$(jq "${JQ_QUERY} | length" <"$PLAN_PATH")"
