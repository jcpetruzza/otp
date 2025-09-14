#!/usr/bin/env bash
set -euo pipefail

: ${BUCK_SCRATCH_PATH:=/tmp}

main() {
    [[ $# -ge 4 ]] || die "Wrong number of arguments"
    otp_mk="$1"; shift
    vsn_mk="$1"; shift
    target_triple="$1"; shift

    WORKSPACE=$(mktemp -d -p "$BUCK_SCRATCH_PATH")

    parse "$otp_mk" "SYSTEM_VSN" "$WORKSPACE/system_vsn"
    parse "$otp_mk" "OTP_VERSION" "$WORKSPACE/otp_version"
    parse "$vsn_mk" "VSN" "$WORKSPACE/vsn"

    exec "$@" \
      "$(cat "$WORKSPACE/system_vsn")" \
      "$(cat "$WORKSPACE/otp_version")" \
      "$(cat "$WORKSPACE/vsn")" \
      "$target_triple"
}

parse() {
    mk="$1"; shift
    key="$1"; shift
    dest="$1"; shift

    pat="$(printf '^%s *= *' $key)"
    res="$(grep -m1 "$pat" "$mk" | sed -e "s/$pat//")"
    [[ -n "$res" ]] || die "$key not found in $mk"
    echo "$res" > "$dest"
}

die() {
    echo >&2 "$*"
    exit 1
}

main "$@"
