def _perl_binary_impl(ctx: AnalysisContext):
    src = ctx.attrs.src
    return [
        DefaultInfo(),
        RunInfo(args = cmd_args(["perl", src])),
    ]

perl_binary = rule(
    impl = _perl_binary_impl,
    attrs = {
        "src": attrs.source(),
    }
)
