BEGIN { in_field=0 }
$0 ~ "^" field ":" {
    in_field=1
    sub("^" field ":[[:space:]]*", "")
    if (length($0) > 0) print $0
    next
}
in_field && /^[^[:space:]]/ { in_field=0 }
in_field {
    gsub(/^[[:space:]]+/, "")
    gsub(/[[:space:]]+$/, "")
    print
}
