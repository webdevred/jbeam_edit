BEGIN { skip = 0 }
skip == 0 && $0 ~ header { skip = 1; next }
skip == 1 && /^--/ { next }
skip == 1 { skip = 2; next }
{ print }
