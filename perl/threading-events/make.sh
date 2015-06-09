#!/bin/bash

WORKING_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$WORKING_DIR"

CC="clang"
CFLAGS=""
LFLAGS="-pthread"
EXPOSED_SYMS_FILE=".tlsyms"
OUT="test"
EXPOSED_CLASSES_ACCESS_HEADER="tlclasses.h"

if [ "$1" = "clean" ]; then
  rm -f "$OUT"
  rm -f *.o
  rm -f tl_*.c
  rm -f repl_tl_*.c
  rm -f "$EXPOSED_SYMS_FILE"
  rm -f "$EXPOSED_CLASSES_ACCESS_HEADER"
else
  rm -f "$EXPOSED_SYMS_FILE"
  rm -f *.o
  rm -f repl_tl_*.c
  rm -f tl_*.c
  # rebuilds EXPOSED_SYMS_FILE and builds tl_%.c,
  # which have TL_EXPOSE_VAR removed
  for c_file in *.c; do
    ./tl.pl "$c_file" "tl_$c_file" "$EXPOSED_SYMS_FILE"
  done
  # build tlclasses.h
  ./insert_primitive_log_rw.pl "$EXPOSED_SYMS_FILE" \
                               "$EXPOSED_CLASSES_ACCESS_HEADER"
  # build repl_tl_%.c, which have all the log_read and log_write functions
  tlc_files=$(ls tl_*.c)
  for tlc_file in $tlc_files; do
    ./transform.pl "$tlc_file" "repl_$tlc_file" "$EXPOSED_SYMS_FILE"
  done
  # finally, build objects
  rplc_files=$(ls repl_tl_*.c)
  for rplc_file in $rplc_files; do
    rlpc_object="$(echo $rlpc_file | sed -e 's/^repl_tl_//g')"
    "$CC" -c $CFLAGS -o "$rplc_object" "$rplc_file"
  done
  # and link
  "$CC" $LFLAGS -o "$OUT" *.o
fi
