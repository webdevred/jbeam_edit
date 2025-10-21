."install-plan" | map (select (."pkg-name" != "jbeam-edit") | ."pkg-name" + "=" + ."pkg-version") | unique
