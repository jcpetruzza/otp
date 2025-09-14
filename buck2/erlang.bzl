load("@prelude//erlang:erlang_info.bzl", "ErlangAppInfo")
load("@prelude//paths.bzl", "paths")

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

    files = {"bin/beam.smp": beam}
    files.update(_indexed_by_basename([erl], prefix = "bin"))
    files.update(_indexed_by_basename(bins, prefix = "bin"))
    files.update(_indexed_by_basename(internal_bins, prefix = "bin"))

    # Ideally we'd use "erts-VSN" for name, but not easily available atm
    erts_dir = ctx.actions.copied_dir("erts-buck2", files)
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
