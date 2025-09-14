load("@prelude//paths.bzl", "paths")

def _configure_impl(ctx: AnalysisContext):
    # 'configure' doesn't receive inputs as args, so to make this rule hermetic,
    # we need to ensure that no other files exist in the working directory.
    # we use a sandbox dir named as a function of the inputs
    sandbox_prefix = sha256(
        '\n'.join(
            sorted([a.short_path() for a in ctx.attrs.srcs]) +
            sorted([
                "{}:{}".format(k, a.short_path())
                for k, as_ in ctx.attrs._common_srcs.items()
                for a in as_
            ])
        )
    )
    package_name = ctx.attrs._package_name

    if not ctx.attrs.srcs:
        fail("No .in files in srcs attr")

    raw_outputs = {}
    for src in ctx.attrs.srcs:
        wanted, ext = paths.split_extension(src.short_path())
        if ext != ".in":
            fail("{} is not a .in file".format(src.short_path()))
        else:
            # This is what the call to `configure` gives us
            raw_output = ctx.actions.declare_output(paths.join(
                sandbox_prefix,
                package_name,
                paths.dirname(wanted,),
                ctx.attrs._target_triple,
                paths.basename(wanted,)
            ))
            raw_outputs[wanted] = raw_output

    script = ctx.attrs.script
    patched_script = ctx.actions.declare_output(paths.join(sandbox_prefix, package_name, script.basename()))
    ctx.actions.run(
        [
            ctx.attrs._patch_configure[RunInfo],
            "--input", script,
            "--output", patched_script.as_output()
        ] + [src.short_path() for src in ctx.attrs.srcs],
        category = "patch"
    )

    # We can't control where the configure script writes the output
    # except by running it on the directory that should have this output.
    # So prepare a copy of the environment
    package_prefix = paths.join(sandbox_prefix, package_name)
    srcs = [_symlink_file(ctx, src, prefix=package_prefix) for src in ctx.attrs.srcs]
    common_srcs = [
        _symlink_file(ctx, src, prefix=paths.join(sandbox_prefix, prefix))
        for prefix, srcs in ctx.attrs._common_srcs.items()
        for src in srcs
    ]

    cmd = cmd_args([ctx.attrs._run_configure[RunInfo], patched_script, _sandbox_dir(raw_outputs)],
        hidden=[
            srcs,
            common_srcs,
        ] + [ output.as_output() for output in raw_outputs.values()]
    )
    cmd.add("--build", ctx.attrs._target_triple)
    cmd.add("--host", ctx.attrs._target_triple)

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

_configure = rule(
    impl = _configure_impl,
    attrs = {
        "script": attrs.source(),
        "srcs": attrs.list(attrs.source()),
        "_patch_configure": attrs.dep(providers=[RunInfo]),
        "_run_configure": attrs.dep(providers=[RunInfo]),
        "_package_name": attrs.string(),
        "_common_srcs": attrs.dict(attrs.string(), attrs.list(attrs.source())),
        "_target_triple": attrs.string(),
    }
)

def configure(*, name, script, srcs, outs=None):
    _configure(
        name = name,
        script = script,
        srcs = srcs,
        _patch_configure = "otp//buck2/autotools:patch-configure",
        _run_configure = "otp//buck2/autotools:run-configure",
        _package_name = package_name(),
        _common_srcs = {
            "make/autoconf": [
                "otp//make/autoconf:config.sub",
            ],
        },
        _target_triple = select({
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
        }),
    )
