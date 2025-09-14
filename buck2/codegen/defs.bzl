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

def _tables_impl(ctx: AnalysisContext):
    script = ctx.attrs._script[RunInfo]

    outputs = {
        name: ctx.actions.declare_output(name)
        for name in [
            "erl_atom_table.c",
            "erl_atom_table.h",
            "erl_bif_list.h",
            "erl_bif_table.c",
            "erl_bif_table.h",
            "erl_dirty_bif_wrap.c",
            "erl_guard_bifs.c",
        ]
    }
    out_dir = cmd_args(outputs.values()[0].as_output(), parent=1)

    cmd = cmd_args(
        script, "-src", out_dir, "-include", out_dir, "-dst", "no",
        hidden = [out.as_output() for out in outputs.values()]
    )
    cmd.add("-jit", "yes" if ctx.attrs._jit else "no")
    cmd.add(ctx.attrs.atoms)
    cmd.add(ctx.attrs.bifs)
    cmd.add(ctx.attrs.dirty_bifs)

    ctx.actions.run(
        cmd,
        category = "codegen",
        env = {"LANG": "C"},
    )

    return [
        DefaultInfo(
            default_outputs = outputs.values(),
            sub_targets = {
                name: [DefaultInfo(default_output = output)]
                for name, output in outputs.items()
            },
        ),
    ]

tables = rule(
    impl = _tables_impl,
    attrs = {
        "atoms": attrs.source(),
        "bifs": attrs.source(),
        "dirty_bifs": attrs.source(),
        "_script": attrs.exec_dep(
            providers=[RunInfo],
            default="@otp//erts/emulator/utils:make_tables"
        ),
        "_jit": attrs.bool(default = select({
            "otp//buck2/config/emu_flavor:jit": True,
            "otp//buck2/config/emu_flavor:emu": False,
        })),
    }

)
