.["install-plan"]
  | map(select(.["pkg-name"] != "jbeam-edit"))
  | map({pkg: "\(.["pkg-name"])=\(.["pkg-version"])", sortkey: (.["pkg-name"] | ascii_downcase)})
  | sort_by(.sortkey)
  | reduce .[] as $i ( []; if any(.[]; .sortkey == $i.sortkey) then . else . + [$i] end )
  | map(.pkg)
