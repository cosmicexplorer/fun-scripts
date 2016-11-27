#!/bin/bash

set -o pipefail
set -o errexit

source 'utils.sh'

key="$1"
null_val="$2"
[ -z "$null_val" ] && null_val="NA"

# pass your valid youtube api v3 key as arg 1
if [ -z "$key" ]; then
  echo "no youtube api key provided" >&2
  exit 1
fi

api_base='https://www.googleapis.com/youtube/v3/videos?part=contentDetails'
endpoint="$api_base&key=$key"
export endpoint

handle_error_str_str() {
  handle_err
}

# in case of error, return blank and output error message
get_page() {
  url="$1"
  curl -s "$url" || \
    handle_error_str "url '$url' could not be retrieved, continuing"
}
export -f get_page

extract_duration() {
  target="$1"
  jq -er '.items[0].contentDetails.duration' 2>/dev/null || \
    handle_error_str "could not extract duration for video id '$target'"
}
export -f extract_duration

get_duration_from_api() {
  video_id="$1"
  endpt="$endpoint&id=$video_id"
  get_page "$endpt" | extract_duration "$video_id" | while read line; do
    echo "$video_id,$line"
  done
}
export -f get_duration_from_api

links_to_iso_8601() {
  sed -e 's/^.*v=//g' | \
    parallel get_duration_from_api | \
    ./iso_8601_to_duration.pl | \
    grep -Ev "^$"
}

export links="$(grep -Eo '[^[:space:]]+?youtube\.com/watch\?v=[^[:space:]&]+' -)"
export seconds="$(echo "$links" | links_to_iso_8601)"
# TODO: actually open up browser etc
echo "id,length"
echo "$seconds"
