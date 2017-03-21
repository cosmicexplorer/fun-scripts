#!/bin/bash

this_dir="$(dirname "${BASH_SOURCE[0]}")"
source "$this_dir/utils.zsh"
source "$this_dir/with-fifo.sh"

lines="$(env_or_default LINES 20 "^[0-9]+$")"
echo "lines='$lines'"

kanye_song="$(xmlstarlet sel -E 'utf-8' -T -t -m '//ref/@href' -v '.' -n kanye.asf | url_decode | head -n1)"

jq_infile="$this_dir/ffprobe-jq-song-length.jq"

echo "kanye_song=$kanye_song"
ffprobe -v quiet "$kanye_song" -show_entries 'root' -print_format json | \
  (with-fifo "head -n $lines"; \
   jq -f "$jq_infile" > "${WITH_FIFO_IN_QUEUES[0]}" \
      2> >(cut -c'1-80' > "${WITH_FIFO_IN_QUEUES[0]}") | \
      cat < "${WITH_FIFO_OUT_QUEUES[0]}")
