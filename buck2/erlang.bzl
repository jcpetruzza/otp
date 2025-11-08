load("@prelude//:artifacts.bzl", "ArtifactGroupInfo")
load("@prelude//erlang:erlang_info.bzl", "ErlangAppInfo", "ErlangToolchainInfo")
load("@prelude//paths.bzl", "paths")
load("@otp//buck2/constants.bzl", "VSN")

ErtsReleaseInfo = provider(
    fields = {
        "erl": provider_field(Artifact),
        "bins": provider_field(list[Artifact]),
        "output": provider_field(Artifact),
    }
)

def _indexed_by_basename(artifacts: list[Artifact], *, prefix: [None, str] = None) -> dict[str, Artifact]:
    if prefix:
        def mkname(a: Artifact) -> str:
            return paths.join(prefix, a.basename())
    else:
        def mkname(a: Artifact) -> str:
            return a.basename()

    return {mkname(a): a for a in artifacts}

def _erts_release_impl(ctx: AnalysisContext):
    erl = ctx.attrs.erl
    beam = ctx.attrs.beam
    bins = ctx.attrs.bins
    internal_bins = ctx.attrs.internal_bins
    version = ctx.attrs.version
    release_includes = ctx.attrs._release_includes[ArtifactGroupInfo].artifacts

    files = {"bin/beam.smp": beam}
    files.update(_indexed_by_basename([erl], prefix = "bin"))
    files.update(_indexed_by_basename(bins, prefix = "bin"))
    files.update(_indexed_by_basename(internal_bins, prefix = "bin"))
    files.update(_indexed_by_basename(release_includes, prefix = "include"))

    erts_dir = ctx.actions.copied_dir("erts-{}".format(version), files)
    erts_bin_dir = erts_dir.project("bin")

    return [
        DefaultInfo(default_output = erts_dir),
        ErtsReleaseInfo(
            erl = erts_bin_dir.project(erl.basename()),
            bins = [erts_bin_dir.project(bin.basename()) for bin in bins],
            output = erts_dir,
        )
    ]

erts_release = rule(
    impl = _erts_release_impl,
    attrs = {
        "erl": attrs.source(),
        "beam": attrs.source(),
        "bins": attrs.list(attrs.source()),
        "internal_bins": attrs.list(attrs.source()),
        "version": attrs.string(default=VSN),
        "_release_includes": attrs.dep(
            providers=[ArtifactGroupInfo],
            default="otp//erts/emulator:release-includes",
        ),
    }
)

ErlangBootstrapAppInfo = provider(
    fields = {
        "name": provider_field(str),
        "app_file": provider_field(Artifact),
        "beams": provider_field(list[Artifact]),
        "include": provider_field(list[Artifact], default=[]),
    }
)

def _erlang_bootstrap_app_impl(ctx: AnalysisContext):
    app_name = ctx.attrs.app_name
    if app_name == None:
        app_name = ctx.attrs.name

    app_file = ctx.attrs.app_file
    beams = ctx.attrs.beams
    include = ctx.attrs.include

    if app_file.extension != ".app":
        fail("Wrong extension for app_file {}".format(app_file))

    non_beam_files = [a for a in beams if a.extension != ".beam"]
    if non_beam_files:
        fail("Unexecpted file in 'beams': {}".format(non_beam_files[0].short_path()))

    extra_includes = [a for a in include if a.extension != ".hrl"]
    if extra_includes:
        fail("Unexecpted file in 'include': {}".format(extra_includes[0].short_path()))

    return [
        DefaultInfo(),
        ErlangBootstrapAppInfo(
            name = app_name,
            beams = beams,
            include = include,
            app_file = app_file,
        )
    ]

erlang_bootstrap_app = rule(
    impl = _erlang_bootstrap_app_impl,
    attrs = {
        "app_name": attrs.option(attrs.string(), default=None),
        "app_file": attrs.source(),
        "beams": attrs.list(attrs.source()),
        "include": attrs.list(attrs.source(), default=[]),
    },
)

def _get_app_name(app: Dependency) -> str:
    if ErlangAppInfo in app:
        return app[ErlangAppInfo].name
    elif ErlangBootstrapAppInfo in app:
        return app[ErlangBootstrapAppInfo].name
    else:
        fail("Dependency not an app: {}".format(app))

ErlangOtpReleaseInfo = provider(
    fields = {
        "apps": provider_field(list[Dependency]),
    }
)

def _erlang_otp_release_impl(ctx: AnalysisContext):
    erts = ctx.attrs.erts[ErtsReleaseInfo]
    boots =  ctx.attrs.boots
    bootstrapping = ctx.attrs.bootstrapping

    erts_dir = ctx.actions.copy_dir(erts.output.basename(), erts.output)
    boot_dir = ctx.actions.copied_dir("boot", _indexed_by_basename(boots))

    def erts_dir_bin_version(bin: Artifact) -> Artifact:
        return erts_dir.project("bin").project(bin.basename())

    bin_files = {"erl": erts_dir_bin_version(erts.erl)}
    bin_files.update({bin.basename(): erts_dir_bin_version(bin) for bin in erts.bins})
    bin_files.update({boot.basename(): boot_dir.project(boot.basename()) for boot in boots})
    bin_dir = ctx.actions.symlinked_dir("bin", bin_files)

    if ctx.attrs.extends:
        base_release = ctx.attrs.extends[ErlangOtpReleaseInfo]
        apps = {_get_app_name(app): app for app in base_release.apps}
    else:
        apps = {}

    for app in ctx.attrs.apps:
        apps[_get_app_name(app)] = app

    lib_files = {}
    for app in apps.values():
        if ErlangBootstrapAppInfo in app:
            if not bootstrapping:
                fail("Prebuilt app used in non-bootstrapping release")

            app = app[ErlangBootstrapAppInfo]
            lib_files.update(_indexed_by_basename(
                app.beams + [app.app_file],
                prefix = paths.join(app.name, "ebin"),
            ))

            if app.include:
                lib_files.update(_indexed_by_basename(
                    app.include,
                    prefix = paths.join(app.name, "include"),
                ))
        elif ErlangAppInfo in app:
            app = app[ErlangAppInfo]
            app_name = "{}-{}".format(app.name, app.version) if not bootstrapping and app.version else app.name
            lib_files[app_name] = app.app_folder
        else:
            fail("Unsupported dep {} of type {}".format(app, type(app)))

    lib_dir = ctx.actions.symlinked_dir("lib", lib_files)
    usr_dir = ctx.actions.symlinked_dir("usr", {
        "include": erts.output.project("include")
    })

    release = [bin_dir, lib_dir, erts_dir, usr_dir]
    otp_dir = ctx.actions.symlinked_dir("otp", _indexed_by_basename(release))

    def project_bin(name: str) -> Artifact:
       return bin_dir.project(name).with_associated_artifacts(release)

    def make_bin_sub_target(exe: Artifact):
        return [
            DefaultInfo(default_output = exe),
            RunInfo(args = cmd_args(exe)),
        ]

    sub_targets = {
        "erl": make_bin_sub_target(project_bin("erl")),
    }
    for bin in erts.bins:
        bin_name = bin.basename()
        sub_targets[bin_name] = make_bin_sub_target(project_bin(bin_name))

    return [
        DefaultInfo(
            default_output = otp_dir,
            sub_targets = sub_targets,
        ),
        ErlangOtpReleaseInfo(apps = apps.values()),
        RunInfo(args = cmd_args(project_bin("erl"))),
    ]

erlang_otp_release = rule(
    impl = _erlang_otp_release_impl,
    attrs = {
        "erts": attrs.transition_dep(
            providers=[ErtsReleaseInfo],
            cfg="otp//buck2/constraints:ignore-erlang-toolchain",
        ),
        "extends": attrs.option(attrs.dep(providers=[ErlangOtpReleaseInfo]), default=None),
        "apps": attrs.list(attrs.dep()),
        "boots": attrs.list(attrs.source(), default=[]),
        "bootstrapping": attrs.bool(default=False),
    },
    supports_incoming_transition = True,
)

def _erlang_asn1_srcs_impl(ctx: AnalysisContext):
    toolchain = ctx.attrs._toolchain[ErlangToolchainInfo]
    erl_opts = toolchain.erl_opts + ["+noobj"] + ctx.attrs.erl_opts

    def _run_erlc(src: Artifact, hrl: Artifact, erl: Artifact):
        out_dir = cmd_args(erl.as_output(), parent = 1)

        erl_cmd = cmd_args(
            toolchain.erlc_trampoline,
            toolchain.otp_binaries.erlc,
            erl_opts,
            "-o",
            out_dir,
            src,
            hidden = [hrl.as_output()],
        )
        ctx.actions.run(erl_cmd, category = "erlc_asn1", identifier = src.basename)

    outputs = {}

    for src in ctx.attrs.srcs:
        if src.basename.endswith(".asn1"):
            module_name = src.basename[:-5]
        elif src.basename.endswith(".set.asn"):
            module_name = src.basename[:-8]
        else:
            fail("Not an .asn1 or .set.asn file: '{}'".format(src))

        hrl = ctx.actions.declare_output(module_name + ".hrl")
        erl = ctx.actions.declare_output(module_name + ".erl")

        _run_erlc(src, hrl, erl)
        outputs[hrl.basename] = hrl
        outputs[erl.basename] = erl

    return [
        DefaultInfo(
            default_outputs = outputs.values(),
            sub_targets = {
                k: [DefaultInfo(default_output = o)]
                for k, o in outputs.items()
            }
        ),
    ]

erlang_asn1_srcs = rule(
    impl = _erlang_asn1_srcs_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "erl_opts": attrs.list(attrs.string(), default=[]),
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
    }
)

def _resources_impl(ctx: AnalysisContext):
    outputs = []
    for f in ctx.attrs.srcs:
        path = f.short_path
        if paths.starts_with(path, "priv"):
            target = paths.relativize(path, "priv/")
        else:
            target = path
        outputs.append(ctx.actions.symlink_file(target, f))

    return [DefaultInfo(default_outputs = outputs)]

resources = rule(
    impl = _resources_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
    },
)

def _boot_script_impl(ctx: AnalysisContext):
    src = ctx.attrs.src
    versions = ctx.attrs.versions

    toolchain = ctx.attrs._toolchain[ErlangToolchainInfo]
    erl_opts = ctx.attrs.erl_opts

    if src.extension() == ".rel":
        rel_file = src
    elif src.basename().endswith(".rel.src"):
        rel_file = ctx.actions.declare_output(paths.replace_extension(src.basename(), ""))

        sed_cmd = cmd_args(
            "sed",
            cmd_args(["s;%{}%;{};".format(k, v) for k,v in versions.items()], prepend = "-e"),
            cmd_args(rel_file.as_output(), format = "w {}", prepend = "-e"),
            src,
        )

        ctx.actions.run(sed_cmd, category = "sed")
    else:
        fail("Unsupported file type '{}'".format(src))

    boot_file = ctx.actions.declare_output(ctx.attrs.name + ".boot")
    script_file = ctx.actions.declare_output(ctx.attrs.name + ".script")

    for output, identifier in [(boot_file, "boot)"), (script_file, "script")]:
        cmd = cmd_args(
            toolchain.erlc_trampoline,
            toolchain.otp_binaries.erlc,
            erl_opts,
            "-o",
            output.as_output(),
            rel_file,
        )
        ctx.actions.run(cmd, category = "erlc", identifier = identifier)

    return [
        DefaultInfo(
            default_output = boot_file,
            sub_targets = {
                "script": [DefaultInfo(default_output = script_file)],
            }
        )
    ]

boot_script = rule(
    impl = _boot_script_impl,
    attrs = {
        "src": attrs.source(),
        "erl_opts": attrs.list(attrs.string(), default=[]),
        "versions": attrs.dict(attrs.string(), attrs.string(), default={}),
        "_toolchain": attrs.toolchain_dep(default = "toolchains//:erlang-default"),
    },
)
