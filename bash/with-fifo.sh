# regexp is from from $(man chmod)
export valid_mode_regexp='^[ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=][0-7]+$'
function get-check-mode {
  if [[ -v WITH_FIFO_PERM ]]; then
    if [[ "$WITH_FIFO_PERM" =~ "$valid_mode_regexp" ]]; then
      echo "$WITH_FIFO_PERM"
    else
      cat >&2 <<EOF
The parameter \$WITH_FIFO_PERM is set to '$WITH_FIFO_PERM', which is invalid.
Valid permissions match the PCRE regexp '$valid_mode_regexp'.
EOF
      exit 1
    fi
  else
    #  'u=rw,g=,o=,' or whatever. user can read and write and nobody else can
    echo '0600'
  fi
}
export -f get-check-mode

# uses $TMPDIR for temp dir base if set
# uses $WITH_FIFO_PERM to set fifo modes if set;
# otherwise, opens fifos with mode 0600 (user can read and write, nobody else)
function with-fifo {
  declare -a cmds_to_start=( "$@" )
  declare -a inqs outqs pids cmds
  # uses $TMPDIR if set
  declare tmpdir="$(mktemp -d)"
  declare perm_bits="$(get-check-mode)"

  for cmd_index in "${!cmds_to_start[@]}"; do
    declare cur_cmd="${cmds_to_start[$cmd_index]}"
    cmds+="$cur_cmd"
    declare fifo_in_path="$tmpdir/fifo-proc-$cmd_index-in"
    declare fifo_out_path="$tmpdir/fifo-proc-$cmd_index-out"
    inqs+="$fifo_in_path"
    outqs+="$fifo_out_path"
    mkfifo -m "$perm_bits" "$fifo_in_path"
    mkfifo -m "$perm_bits" "$fifo_out_path"
    $cur_cmd <"$fifo_in_path" >"$fifo_out_path" & pids+="$!"
  done

  declare -agx WITH_FIFO_IN_QUEUES=( "$inqs" )
  declare -agx WITH_FIFO_OUT_QUEUES=( "$outqs" )
  declare -agx WITH_FIFO_PIDS=( "$pids" )
  declare -agx WITH_FIFO_CMDS=( "$cmds" )
  declare -gx WITH_FIFO_TMP_DIR="$tmpdir"

  trap "rm -rf ${tmpdir}" EXIT
}
export -f with-fifo