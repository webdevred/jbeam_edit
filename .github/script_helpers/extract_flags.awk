/^flag / { flag=$2; in_desc=0 }
/^ *description:/ { in_desc=1; if ($0 ~ /\(experimental\)/) print flag; next }
in_desc && /^[[:space:]]+/ { if ($0 ~ /\(experimental\)/) print flag; next }
/^[^[:space:]]/ { in_desc=0 }
