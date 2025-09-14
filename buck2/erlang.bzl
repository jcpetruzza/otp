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
