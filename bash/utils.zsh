function print_args {
  argc="$#"
  argv="$@"
  paste -d '' <(seq -f '$%g=' 1 "$argc") <(printf "'%s'\n" $argv)
}

function handle_error_impl {
  res="$1"
  if [ "$res" -ne 0 ]; then
    "${@:2}" "$res"
  fi
  return "$res"
}

function handle_error_fun {
  handle_error_impl "$?" "$@"
}

function handle_print_string {
  stderr="$1"
  stdout="$2"
  [ -z "$stderr" ] || echo "$stderr" >&2
  echo -n "$stdout"
}

function handle_error_str {
  handle_error_impl "$?" handle_print_string "$1" "$2"
}

function env_or_default {
  varname="$1"
  default="$2"
  regex="$3"
  if [[ -v "$varname" ]]; then
    exp="${(P)varname}"
    if [[ -n "$regex" && "$exp" =~ "$regex" ]]; then
      echo "$exp"
    else
      echo "$default"
    fi
  else
    echo "$default"
  fi
}
