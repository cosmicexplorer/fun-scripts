# from this directory, run ./run-ffprobe-jq-song-length.sh

reduce to_entries[] as $pair ([]; . + [$pair]) | .[]
