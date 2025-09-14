def _alloc_types_impl(ctx: AnalysisContext):
    src = ctx.attrs.src
    script = ctx.attrs._script[RunInfo]
    out = ctx.actions.declare_output("erl_alloc_types.h")
    cmd = cmd_args([script, "-src", src, "-dst", out.as_output()])
    cmd.add(*ctx.attrs._types)

    ctx.actions.run(
        cmd,
        env = {"LANG": "C"},
        category = "codegen",
    )
    return [
        DefaultInfo(
            default_output = out,
            sub_targets = {
                "erl_alloc_types.h": [DefaultInfo(default_output = out)],
            }
        )
    ]

def _alloc_types_list():
    def add_type_if(constraint: str, value: str):
        return select({
            constraint: [value],
            "DEFAULT": []
        })

    return (
        add_type_if("@otp//buck2/config/emu_type:asan", "asan") +
        add_type_if("@otp//buck2/config/emu_type:debug", "debug") +
        add_type_if("@otp//buck2/config/emu_type:gprof", "gprof") +
        add_type_if("@otp//buck2/config/emu_type:lcnt", "lcnt") +
        add_type_if("@otp//buck2/config/emu_type:valgrind", "valgrind") +
        ["nofrag"] +
        add_type_if("@otp//buck2/config/emu_flavor:jit", "beamasm") +
        ["unix"]
    )

alloc_types = rule(
    impl = _alloc_types_impl,
    attrs = {
        "src": attrs.source(),
        "_script": attrs.exec_dep(
            providers=[RunInfo],
            default="@otp//erts/emulator/utils:make_alloc_types"
        ),
        "_types": attrs.list(attrs.string(), default=_alloc_types_list()),
    }
)
