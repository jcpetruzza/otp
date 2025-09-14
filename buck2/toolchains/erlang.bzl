load("@prelude//erlang:erlang_toolchain.bzl", "erlang_toolchain")

def system_erlang_toolchain(*, name: str):
    native.erlang_otp_binaries(
        name = "{}-binaries".format(name),
        erl = "local/erl",
        erlc = "local/erlc",
        escript = "local/escript",
        visibility=["PUBLIC"],
    )
    erlang_toolchain(
        name = name,
        otp_binaries = ":{}-binaries".format(name),
        erl_opts = "+nowarn_underscore_match",
        emu_flags = "+sbwt very_short +sbwtdcpu very_short +sbwtdio very_short",
        parse_transforms_filters = {},
        parse_transforms = [],
        visibility = ["PUBLIC"],
    )
