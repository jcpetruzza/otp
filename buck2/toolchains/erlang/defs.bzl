load(
    "@prelude//erlang:erlang_toolchain.bzl",
    "erlang_toolchain",
    ErlangToolchainUtilsInfo = "ToolchainUtillInfo",
)

_COMMON_ERL_OPTS = [
    "+nowarn_underscore_match",
]

_COMMON_EMU_FLAGS = [
    "+sbwt",
    "very_short",
    "+sbwtdcpu",
    "very_short",
    "+sbwtdio",
    "very_short",
]

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
        erl_opts = " ".join(_COMMON_ERL_OPTS),
        emu_flags = " ".join(_COMMON_EMU_FLAGS),
        parse_transforms_filters = {},
        parse_transforms = [],
        visibility = ["PUBLIC"],
    )

def local_erlang_toolchain(*,
    name: str,
    otp_release: str,
    extra_erl_opts: list[str] = [],
    toolchain_utilities: [None, str] = None
):
    native.erlang_otp_binaries(
        name = "{}-binaries".format(name),
        erl = "{}[erl]".format(otp_release),
        erlc = "{}[erlc]".format(otp_release),
        escript = "{}[escript]".format(otp_release),
    )
    erlang_toolchain(
        name = name,
        otp_binaries = ":{}-binaries".format(name),
        erl_opts = " ".join(_COMMON_ERL_OPTS + extra_erl_opts),
        emu_flags = " ".join(_COMMON_EMU_FLAGS),
        parse_transforms_filters = {},
        parse_transforms = [],
        toolchain_utilities = toolchain_utilities,
        visibility = ["PUBLIC"],
    )

def _erlang_toolchain_utilities_override_impl(ctx: AnalysisContext):
    base = ctx.attrs._base[ErlangToolchainUtilsInfo]
    override_keys = ["dependency_analyzer", "dependency_finalizer"]

    utils = {k: getattr(base, k) for k in dir(base)}

    for k in override_keys:
        override = getattr(ctx.attrs, k)
        if override != None:
            utils[k] = override

    return [
        DefaultInfo(),
        ErlangToolchainUtilsInfo(**utils)
    ]

erlang_toolchain_utilities_override = rule(
    impl = _erlang_toolchain_utilities_override_impl,
    attrs = {
        "_base": attrs.dep(providers=[ErlangToolchainUtilsInfo], default = "@prelude//erlang/toolchain:toolchain_utilities"),
        "dependency_analyzer": attrs.option(attrs.source(), default=None),
        "dependency_finalizer": attrs.option(attrs.source(), default=None),
    },
)
