load("@otp//buck2/constraints:defs.bzl", "transition_force_constraints", "transition_ignore_constraints")
load("@otp//buck2:erlang.bzl", "erlang_bootstrap_app", "erts_release", "erlang_otp_release")

# Release used for development; enough to compile and test things
erlang_otp_release(
    name = "otp-devel",
    erts = "otp//erts:erts",
    boots = [
        "otp//erts/start_scripts:start",
        "otp//erts/start_scripts:start_clean",
        "otp//erts/start_scripts:start_sasl",
        "otp//erts/start_scripts:no_dot_erlang",

        "otp//erts/start_scripts:start[script]",
    ],
    apps = [
        "otp//lib/kernel:app",
        "otp//lib/stdlib:app",
        "otp//lib/compiler:app",
        "otp//lib/asn1:app",
        "otp//lib/parsetools:app",
        "otp//lib/sasl:app",
        "otp//lib/syntax_tools:app",
        "otp//lib/public_key:app",
        "otp//lib/xmerl:app",
        "otp//lib/common_test:app",
    ],
    incoming_transition = "otp//bootstrap:use-bootstrap-2",
    visibility = ["PUBLIC"],
)
