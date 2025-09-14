#!/usr/bin/env bash
set -euo pipefail

main() {
    NIFS=()
    DRIVERS=()
    while [[ $# -gt 0 && "$1" != "--" ]]; do
        flag="$1"; shift
        case "$flag" in
            -nif)
                fake_file_suffix="_nif.o"
                ;;
            -driver)
                fake_file_suffix="_drv.o"
                ;;
            -static-nif)
                fake_file_suffix=".a"
                ;;
            -static-driver)
                fake_file_suffix=".a"
                ;;
            *)
                die "Unknown option '$flag'"
        esac
        [[ $# -gt 0 ]] || die "Missing argument"
        case "$flag" in
            *-nif)
                nif_init_fun="$(get_nif_init "$1")"
                [[ -n "$nif_init_fun" ]] || die "Not a nif source '$1'"
                fake_nif_file="${nif_init_fun}${fake_file_suffix}"
                NIFS+=( "$fake_nif_file" )
                ;;

            -*driver)
                drv_name="$(get_driver_name "$1")"
                [[ -n "$drv_name" ]] || die "Not a driver source '$1'"
                fake_drv_file="${drv_name}${fake_file_suffix}"
                DRIVERS+=( "$fake_drv_file" )
                ;;
        esac
        shift
    done

    [[ $# -gt 0 ]] || die "Missing command"

    exec "$@" -nifs "${NIFS[@]}" -drivers "${DRIVERS[@]}"
}

get_nif_init() {
    nif_src="$1"; shift
    sed -n 's/^ERL_NIF_INIT( *\([^ ,]*\) *,.*/\1/p' "$nif_src" | head -n1
}

get_driver_name() {
    drv_src="$1"; shift
    sed -n 's/^struct *erl_drv_entry *\(.*\)_driver_entry *=.*/\1/p' "$drv_src" | head -n1
}

die() {
    echo >&2 "$*"
    exit 1
}

main "$@"
