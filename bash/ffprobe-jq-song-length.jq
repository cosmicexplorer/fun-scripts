# from this directory, run ./run-ffprobe-jq-song-length.sh

recurse(.[] | flatten)
