load("@otp//buck2/autotools:defs.bzl", "target_triple")

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

def _is_jit():
    return select({
        "otp//buck2/config/emu_flavor:jit": True,
        "otp//buck2/config/emu_flavor:emu": False,
    })

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
        "_jit": attrs.bool(default = _is_jit()),
    }

)

def _beam_opcodes_impl(ctx: AnalysisContext):
    script = ctx.attrs._script[RunInfo]

    output_files = [
        "beam_opcodes.h",
        "beam_opcodes.c",
    ]
    output_files.extend(ctx.attrs._flavor_outputs)

    outputs = {
        name: ctx.actions.declare_output(name)
        for name in output_files
    }
    out_dir = cmd_args(outputs.values()[0].as_output(), parent=1)

    cmd = cmd_args([script], hidden=[out.as_output() for out in outputs.values()])
    cmd.add("-wordsize", str(ctx.attrs._wordsize))
    cmd.add("-code-model", "small" if ctx.attrs.small_code_model else "unknown")
    cmd.add("-outdir", out_dir)
    cmd.add("-jit", "yes" if ctx.attrs._jit else "no")
    cmd.add("-DUSE_VM_PROBES={}".format(1 if ctx.attrs._vm_probes else 0))
    cmd.add("-emulator", ctx.attrs.srcs)

    ctx.actions.run(
        cmd,
        category = "codegen",
        env = {"LANG": "C"},
    )

    return [
        DefaultInfo(
            default_outputs = outputs.values(),
            sub_targets = {
                name: [DefaultInfo(default_output = out)]
                for name, out in outputs.items()
            },
        ),
    ]

def _beamops_flavor_outputs():
    return select({
        "otp//buck2/config/emu_flavor:emu": [
            "beam_cold.h",
            "beam_warm.h",
            "beam_hot.h",
        ],

        "otp//buck2/config/emu_flavor:jit": [
            "beamasm_emit.h",
            "beamasm_protos.h",
        ],
    })

def _wordsize():
    return select({
        "config//cpu:x86_64": 64,
        "config//cpu:x86_32": 32,
        "config//cpu:arm64": 64,
        "config//cpu:arm32": 32,
    })

def _vm_probes():
    return select({
        "otp//buck2/config/dynamic-trace:dtrace": True,
        "otp//buck2/config/dynamic-trace:lttng": True,
        "otp//buck2/config/dynamic-trace:systemtap": True,
        "DEFAULT": False,
    })

beam_opcodes = rule(
    impl = _beam_opcodes_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "small_code_model": attrs.bool(),
        "_defines": attrs.dict(attrs.string(), attrs.string(), default={}),
        "_script": attrs.exec_dep(
            providers=[RunInfo],
            default="@otp//erts/emulator/utils:beam_makeops",
        ),
        "_flavor_outputs": attrs.list(attrs.string(), default = _beamops_flavor_outputs()),
        "_wordsize": attrs.int(default = _wordsize()),
        "_jit": attrs.bool(default = _is_jit()),
        "_vm_probes": attrs.bool(default = _vm_probes()),
    },
)

def _erl_version_impl(ctx: AnalysisContext):
    output = ctx.actions.declare_output(ctx.attrs.name)

    cmd = cmd_args(
        ctx.attrs._wrapper[RunInfo],
        ctx.attrs.otp,
        ctx.attrs.vsn,
        ctx.attrs._target_triple,
        ctx.attrs._script[RunInfo],
        "-o",
        output.as_output()
    )

    ctx.actions.run(
        cmd,
        category = "codegen",
        env = {"LANG": "C"},
    )

    return [
        DefaultInfo(default_output=output)
    ]

erl_version = rule(
    impl = _erl_version_impl,
    attrs = {
        "vsn": attrs.source(),
        "otp": attrs.source(),
        "_script": attrs.exec_dep(
            providers=[RunInfo],
            default="@otp//erts/emulator/utils:make_version",
        ),
        "_wrapper": attrs.exec_dep(
            providers=[RunInfo],
            default="@otp//buck2/codegen:run-make-version",
        ),
        "_target_triple": attrs.string(default = target_triple()),
    },
)

def beam_asm_global(*, name: str, src: str):
    native.genrule(
        name = name,
        cmd = "perl ${SRCS} > ${OUT}",
        env = {"LANG": "C"},
        srcs = [src],
        out = name,
    )

def preloaded(*, name: str, srcs: str):
    native.genrule(
        name = name,
        cmd = "$(exe otp//erts/emulator/utils:make_preload) -old $(location {})/* > $OUT".format(srcs),
        env = {"LANG": "C"},
        out = name,
    )

def _yielding_c_fun_impl(ctx: AnalysisContext):
    ycf = ctx.attrs._ycf[RunInfo]
    src = ctx.attrs.src
    args = ctx.attrs.args

    out = ctx.actions.declare_output(ctx.attrs.name)

    cmd = cmd_args([ycf, args, "-output_file_name", out.as_output(), src])
    ctx.actions.run(
        cmd,
        category = "codegen",
    )

    return [DefaultInfo(default_output=out)]

yielding_c_fun = rule(
    impl = _yielding_c_fun_impl,
    attrs = {
        "src": attrs.source(),
        "args": attrs.list(attrs.string()),
        "_ycf": attrs.exec_dep(
            providers=[RunInfo],
            default="otp//erts:yielding_c_fun",
        ),
    },
)
