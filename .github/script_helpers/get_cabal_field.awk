BEGIN { in_field=0 }
$0 ~ "^"field":" { in_field=1; next }
in_field && /^[^[:space:]]/ { in_field=0 }
in_field { gsub(/^[[:space:]]+/, ""); print }
