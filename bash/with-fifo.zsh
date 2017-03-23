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

# uses $TMPDIR for temp dir base if set
# uses $WITH_FIFO_PERM to set fifo modes if set;
# otherwise, opens fifos with mode 0600 (user can read and write, nobody else)
function with-fifo {
  typeset -a cmds_to_start=( "$@" )
  if [[ "${#cmds_to_start}" -eq 0 ]]; then
    echo "no commands given to with-fifo!" >&2
    return 1
  fi
  typeset -a inqs=() outqs=() cmds=()
  # uses $TMPDIR if set
  typeset tmpdir="$(mktemp -d)"
  typeset perm_bits="$(get-check-mode)"

  for cmd_index in {1..${#cmds_to_start}}; do
    typeset cur_cmd="${cmds_to_start[$cmd_index]}"
    cmds+="$cur_cmd"
    typeset fifo_in_path="$tmpdir/fifo-proc-$cmd_index-in"
    typeset fifo_out_path="$tmpdir/fifo-proc-$cmd_index-out"
    inqs+="$fifo_in_path"
    outqs+="$fifo_out_path"
    mkfifo -m "$perm_bits" "$fifo_in_path"
    mkfifo -m "$perm_bits" "$fifo_out_path"
  done


  typeset -agx WITH_FIFO_IN_QUEUES=( "${inqs[@]}" )
  typeset -agx WITH_FIFO_OUT_QUEUES=( "${outqs[@]}" )
  typeset -agx WITH_FIFO_CMDS=( "${cmds[@]}" )
  typeset -gx WITH_FIFO_TMP_DIR="$tmpdir"

  kill-fifo &
}

function kill-fifo {
  typeset -a pids

  for cmd_index in {1..${#WITH_FIFO_CMDS}}; do
    typeset cur_cmd="${WITH_FIFO_CMDS[$cmd_index]}"
    typeset fifo_in_path="${WITH_FIFO_IN_QUEUES[$cmd_index]}"
    typeset fifo_out_path="${WITH_FIFO_OUT_QUEUES[$cmd_index]}"
    ${=cur_cmd} <"$fifo_in_path" >"$fifo_out_path" & pids+="$!"
  done

  wait $pids
  rm -rf "${WITH_FIFO_TMP_DIR}"
}

typeset -agx WITH_FIFO_IN_QUEUES
typeset -agx WITH_FIFO_OUT_QUEUES
typeset -agx WITH_FIFO_CMDS
typeset -gx WITH_FIFO_TMP_DIR
