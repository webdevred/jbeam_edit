."install-plan"
  | sort_by (."pkg-name")
  | unique_by (."pkg-name")
  | map (
        select (."pkg-name" != "jbeam-edit")
        | ."pkg-name" + "=" + ."pkg-version"
      )
