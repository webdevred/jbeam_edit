#!/usr/bin/env bash

find examples/formatted_jbeam examples/transformed_jbeam -type f -exec sed -i 's/$/\r/' {} +
