.["install-plan"]
| map(
    select(."pkg-name" != "jbeam-edit")
    | { pkg: ."pkg-name" + "=" + ."pkg-version", sortkey: (."pkg-name" | ascii_downcase) }
  )
| sort_by(."sortkey")
| map (."pkg")

