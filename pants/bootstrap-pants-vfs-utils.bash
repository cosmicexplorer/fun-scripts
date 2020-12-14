#!/usr/bin/env bash

set -euxo pipefail

# Default to compiling in debug mode because it's significantly less resource-intensive.
export MODE="${MODE:-debug}"
readonly _PANTS_URL_DEFAULT='https://github.com/pantsbuild/pants'
readonly PANTS_URL="${PANTS_URL:-$_PANTS_URL_DEFAULT}"

# These are the current binaries we can effectively use.
typeset -ra cargo_binaries=(
  fs_util            # For digesting files into and extracting files from the LMDB backing KV store.

  process_executor   # For converting command-line arguments into a hermetic process execution.
                     # The files the process can access are defined by a protobuf object, which is
                     # the root of some merkel tree. After the process exits, the entire execution
                     # directory is digested and inserted into the LMDB store.
)

export _PANTS_BINARY_OUTDIR

function setup_pants_binaries_build {
  mkdir -vp ./pants-utils/{pants-checkout,pants-binaries}
  pushd ./pants-utils
  export _PANTS_BINARY_OUTDIR="$(pwd)/pants-binaries"
}

function build_all_rust_binaries {
  # We are in the pants repo -- let's build fs_util and process_executor.
  pushd src/rust/engine
  for rust_bin in "${cargo_binaries[@]}"; do
    ../../../build-support/bin/native/cargo build -p "$rust_bin"
    cp -v "target/debug/${rust_bin}" "${_PANTS_BINARY_OUTDIR}"
  done
  popd
}

function checkout_pants {
  git clone "$PANTS_URL" ./pants-checkout
  pushd ./pants-checkout
}

setup_pants_binaries_build      # ./pants-utils
checkout_pants                  # ./pants-checkout
build_all_rust_binaries         # ./src/rust/engine

ls -lAvFh "$_PANTS_BINARY_OUTDIR"
