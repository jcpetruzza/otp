#!/usr/bin/env sh
set -euo pipefail

main() {
    [[ $# -ge 2 ]] || die "Not enough args"
    configure_script="$1"; shift
    sandbox_dir="$1"; shift

    configure_script_dir="${configure_script%/*}"
    configure_script_name="${configure_script##*/}"

    export ERL_TOP="$PWD/$sandbox_dir"

    cd "$configure_script_dir" || die "not found '$configure_script_dir'"

    # The configure script wants to see a "unique file" at certain location,
    # so we create one for it
    grep -q '^ac_unique_file="' "$configure_script_name" || die "no ac_unique_file_defined"
    ac_unique_file="$(grep '^ac_unique_file="' "$configure_script_name" | cut -d\" -f2)"
    mkdir -p "./${ac_unique_file%/*}"
    touch "$ac_unique_file"

    exec "./$configure_script_name" \
      --disable-option-checking \
      --cache-file=/dev/null \
      --srcdir="$PWD" \
      "$@"
}

die() {
    echo >&2 "$*"
    exit 1
}

main "$@"
