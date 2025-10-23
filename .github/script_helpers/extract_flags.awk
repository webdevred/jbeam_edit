# Purpose:
# - If EXPERIMENTAL_ONLY is non-empty -> print flags that contain "(experimental)"
# - If EXPERIMENTAL_ONLY is empty     -> print flags that contain NO parentheses at all

# Per-flag state
#   flag         - current flag name
#   in_desc      - currently inside a description block
#   experimental - description contains "(experimental)"
#   has_parens   - description contains ANY "(...)"
#
# Policy:
#   experimental-only mode → only print flags marked experimental
#   default mode           → only print flags without ANY parentheses

# Compiled regexes for readability
BEGIN {
    exp_rx   = "\\(experimental\\)"
    paren_rx = "\\([^)]*\\)"
}

# Decide whether to print the current flag
function should_print() {
    if (EXPERIMENTAL_ONLY != "" && experimental)  return 1
    if (EXPERIMENTAL_ONLY == "" && !has_parens)   return 1
    return 0
}

# Output a completed flag if appropriate
function finalize_flag() {
    if (flag != "" && should_print()) {
        print flag
    }
}

# Start of flag block
/^flag[[:space:]]+/ {
    finalize_flag()     # finish previous flag first
    flag = $2
    in_desc = 0
    experimental = 0
    has_parens = 0
    next
}

# Description start line
/^ *description:/ {
    in_desc = 1
    if ($0 ~ exp_rx)   experimental = 1
    if ($0 ~ paren_rx) has_parens = 1
    next
}

# Description continuation (indented)
/^[[:space:]]+/ && in_desc {
    if ($0 ~ exp_rx)   experimental = 1
    if ($0 ~ paren_rx) has_parens = 1
    next
}

# Any non-indented line ends description
/^[^[:space:]]/ {
    in_desc = 0
}

# EOF, finalize last flag
END {
    finalize_flag()
}
