load("@prelude//paths.bzl", "paths")
load("@prelude//toolchains:cxx.bzl", "CxxToolsInfo")
load("@toolchains//cxx.bzl", "cxx_tools_info")

def target_triple():
    return select({
        "config//cpu:x86_64": select({
            "config//os:linux": "x86_64-pc-linux-gnu",
            "config//os:macos": "x86_64-apple-darwin",
            "config//os:windows": select({
                "config//abi:msvc": "x86_64-pc-windows-msvc",
            }),
        }),
        "config//cpu:arm64": select({
            "config//os:linux": "aarch64-pc-linux-gnu",
            "config//os:macos": "arm64-apple-darwin",
            "config//os:windows": select({
                "config//abi:msvc": "aarch64-pc-windows-msvc",
            }),
        }),
    })

def _configure_impl(ctx: AnalysisContext):
    package = ctx.attrs._package_name
    srcs = ctx.attrs.srcs
    cxx_tools_info = ctx.attrs._cxx_tools_info[CxxToolsInfo]

    # 'configure' doesn't receive inputs as args, so to make this rule hermetic,
    # we need to ensure that no other files exist in the working directory.
    # we use a sandbox dir named as a function of the inputs
    sandbox_prefix = sha256(
        '\n'.join(
            sorted([
                "{}:{}".format(k, a.short_path())
                for k, as_ in srcs.items()
                for a in as_
            ])
        )
    )

    inputs = {}
    for dir, dir_srcs in srcs.items():
        for src in dir_srcs:
            if src.extension() == ".in":
                inputs.setdefault(dir, []).append(src)

    if not inputs:
        fail("No .in files in srcs attr")

    raw_outputs = {}
    for dir, dir_srcs in inputs.items():
        for src in dir_srcs:
            wanted = paths.replace_extension(src.short_path(), "")
            # This is what the call to `configure` gives us
            raw_output = ctx.actions.declare_output(paths.join(
                sandbox_prefix,
                dir,
                paths.dirname(wanted,),
                ctx.attrs._target_triple,
                paths.basename(wanted,)
            ))
            raw_outputs[paths.join(dir, wanted)] = raw_output

    script = ctx.attrs.script
    patched_script = ctx.actions.declare_output(paths.join(sandbox_prefix, package, script.basename()))
    relativized_input_paths = [
        _relativize(paths.join(dir, input.short_path()), package)
        for dir, dir_inputs in inputs.items()
        for input in dir_inputs
    ]
    ctx.actions.run(
        [
            ctx.attrs._patch_configure[RunInfo],
            "--input", script,
            "--output", patched_script.as_output()
        ] + relativized_input_paths,
        category = "patch"
    )

    # We can't control where the configure script writes the output
    # except by running it on the directory that should have this output.
    # So prepare a copy of the environment
    linked_srcs = [
        _symlink_file(ctx, src, prefix=paths.join(sandbox_prefix, dir))
        for dir, srcs in srcs.items()
        for src in srcs
    ]

    cmd = cmd_args([ctx.attrs._run_configure[RunInfo], patched_script, _sandbox_dir(raw_outputs)],
        hidden=linked_srcs + [output.as_output() for output in raw_outputs.values()]
    )
    cmd.add("--build", ctx.attrs._target_triple)
    cmd.add("--host", ctx.attrs._target_triple)

    dynamic_trace = ctx.attrs._dynamic_trace
    if dynamic_trace:
        cmd.add(cmd_args(dynamic_trace, format="--with-dynamic-trace={}"))

    cmd.add([
        cmd_args(cxx_tools_info.archiver, format="AR={}"),
        cmd_args(cxx_tools_info.compiler, format="CC={}"),
        cmd_args(cxx_tools_info.cxx_compiler, format="CXX={}"),
        cmd_args(cxx_tools_info.linker, format="LD={}"),
    ])

    ctx.actions.run(cmd, category = "configure")

    outputs = {
        wanted: ctx.actions.symlink_file(wanted, raw_output)
        for wanted, raw_output in raw_outputs.items()
    }

    return [
        DefaultInfo(
            default_outputs=outputs.values(),
            sub_targets = {
                k: [DefaultInfo(default_output=v)]
                for k,v in outputs.items()
            }
        ),
    ]

def _sandbox_dir(raw_outputs: dict[str, Artifact]) -> cmd_args:
    output_path, output_artifact = raw_outputs.items()[0]
    parts = output_path.split("/")
    return cmd_args(output_artifact, parent=len(parts) + 1, ignore_artifacts=True)

def _symlink_file(ctx: AnalysisContext, src: Artifact, *, prefix: str | None = None):
    dest = src.short_path()
    if prefix:
        dest = paths.join(prefix, dest)
    return ctx.actions.symlink_file(dest, src)

def _relativize(path: str, start: str) -> str:
    path_parts = paths.normalize(path).split("/")
    start_parts = paths.normalize(start).split("/")

    i = 0
    limit = min(len(path_parts), len(start_parts))
    for _ in range(0, limit):
        if path_parts[i] != start_parts[i]:
            break
        i += 1

    result_parts = [".."] * (len(start_parts) - i) + path_parts[i:]
    return "/".join(result_parts)

_configure = rule(
    impl = _configure_impl,
    attrs = {
        "script": attrs.source(),
        "srcs": attrs.dict(
            attrs.string(),
            attrs.list(attrs.source()),
        ),
        "_patch_configure": attrs.dep(
            providers=[RunInfo],
            default = "otp//buck2/autotools:patch-configure",
        ),
        "_run_configure": attrs.dep(
            providers=[RunInfo],
            default = "otp//buck2/autotools:run-configure",
        ),
        "_package_name": attrs.string(),
        "_target_triple": attrs.string(default = target_triple()),
        "_dynamic_trace": attrs.option(attrs.string(), default = select({
            "otp//buck2/config/dynamic-trace:dtrace": "dtrace",
            "otp//buck2/config/dynamic-trace:lttng": "lttng",
            "otp//buck2/config/dynamic-trace:systemtap": "systemtap",
            "DEFAULT": None,
        })),
        "_cxx_tools_info": attrs.exec_dep(providers = [CxxToolsInfo], default = cxx_tools_info()),
    }
)

def configure(*, name, script, srcs, outs=None):
    _configure(
        name = name,
        script = script,
        srcs = srcs,
        _package_name = package_name()
    )
