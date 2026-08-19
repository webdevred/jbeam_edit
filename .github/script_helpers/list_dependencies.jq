.["install-plan"] as $plan
| ($plan | map({(.id): .}) | add) as $byid
| ($plan | map(select(.["pkg-name"] == "jbeam-edit")) | map(.id)) as $seeds
| def getdeps($node):
    ($node.depends // [])
    + ($node["exe-depends"] // [])
    + (($node.components // {})
        | [.[] | (.depends // []) + (.["exe-depends"] // [])]
        | add // []);
  def reach(frontier; visited):
    (frontier - visited) as $new
    | if ($new | length) == 0 then visited
      else
        (visited + $new) as $visited2
        | ($new | map($byid[.]) | map(getdeps(.)) | add // []) as $next
        | reach($next; $visited2)
      end;
  reach($seeds; [])
| unique
| map($byid[.])
| map(select(.["pkg-name"] != "jbeam-edit"))
| map({pkg: "\(.["pkg-name"])=\(.["pkg-version"])", sortkey: (.["pkg-name"] | ascii_downcase)})
| sort_by(.sortkey)
| unique_by(.sortkey)
| map(.pkg)
