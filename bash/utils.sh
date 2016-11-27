#!/bin/bash

set -o pipefail
set -o errexit

handle_error_impl() {
  res="$1"
  if [ "$res" -ne 0 ]; then
    "${@:2}" "$res"
  fi
  return "$res"
}
export -f handle_error_impl

handle_error_fun() {
  handle_error_impl "$?" "$@"
}
export -f handle_error_fun

handle_print_string() {
  stderr="$1"
  stdout="$2"
  [ -z "$stderr" ] || echo "$stderr" >&2
  echo -n "$stdout"
}
export -f handle_print_string

handle_error_str() {
  handle_error_impl "$?" handle_print_string "$1" "$2"
}
export -f handle_error_str
