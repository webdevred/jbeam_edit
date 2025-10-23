# Purpose:
# - If EXPERIMENTAL_ONLY is non-empty -> print flags that contain "(experimental)"
# - If EXPERIMENTAL_ONLY is empty     -> print flags that contain NO parentheses at all

# state variables:
# flag         - current flag name
# in_desc      - 1 when inside a description block
# experimental - 1 if any desc line contains "(experimental)"
# has_parens   - 1 if any desc line contains any "(...)"


/^flag[[:space:]]+/ {
    # finalize previous flag (if any) before starting a new one
    if (flag != "") {
        if (EXPERIMENTAL_ONLY != "" && experimental)      print flag
        else if (EXPERIMENTAL_ONLY == "" && !has_parens) print flag
    }

    # start new flag
    flag = $2
    in_desc = 0
    experimental = 0
    has_parens = 0
    next
}

/^ *description:/ {
    # enter description; check this line for parentheses
    in_desc = 1
    if ($0 ~ /\(experimental\)/) experimental = 1
    if ($0 ~ /\([^)]+\)/)        has_parens = 1
    next
}

in_desc && /^[[:space:]]+/ {
    # indented description continuation lines
    if ($0 ~ /\(experimental\)/) experimental = 1
    if ($0 ~ /\([^)]+\)/)        has_parens = 1
    next
}

/^[^[:space:]]/ {
    # non-indented line ends any description block
    in_desc = 0
}

END {
    # finalize last flag at EOF
    if (flag != "") {
        if (EXPERIMENTAL_ONLY != "" && experimental)      print flag
        else if (EXPERIMENTAL_ONLY == "" && !has_parens) print flag
    }
}
