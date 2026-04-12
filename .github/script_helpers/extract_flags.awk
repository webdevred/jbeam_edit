function matches(line) {
    if (index(line, "(experimental)") > 0) return 1
    if (extra_keywords != "" && index(line, "(" extra_keywords ")") > 0) return 1
    return 0
}
/^flag / { flag=$2; in_desc=0 }
/^ *description:/ { in_desc=1; if (matches($0)) print flag; next }
in_desc && /^[[:space:]]+/ { if (matches($0)) print flag; next }
/^[^[:space:]]/ { in_desc=0 }
