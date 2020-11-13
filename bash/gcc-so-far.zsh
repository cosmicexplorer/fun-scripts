#!/usr/bin/env

set -euxo pipefail



locate gcc \
  | grep -E '/gcc$' \
  | while read line; do \
  [[ -f "$line" ]] && [[ -x "$line" ]] \
  && echo "$line";
done

EOF

