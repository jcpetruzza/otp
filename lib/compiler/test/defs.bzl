load("@prelude//paths.bzl", "paths")

_COVER = "cover"
_DIALYZER = "dialyzer"
_INLINE = "inline"
_NO_BOOL_OPT = "no_bool_opt"
_NO_COPT = "no_copt"
_NO_COPT_SSA = "no_copt_ssa"
_NO_MODULE_OPT = "no_module_opt"
_NO_OPT = "no_opt"
_NO_SSA_OPT = "no_ssa_opt"
_NO_TYPE_OPT = "no_type_opt"
_POST_OPT = "post_opt"
_R26 = "r26"

def _makefile_variants(config: dict[str, list[str]]) -> list[str]:
    """
    Makefile creates variants of suites in the test/directory, so we
    need to be able to exclude them
    """
    return [
        "{}_{}_SUITE.erl".format(suite[:-len("_SUITE")], variant)
        for suite, variants in config.items()
        for variant in variants
    ]

def _suites_for(variant: str, config: dict[str, list[str]]) -> dict[str, str]:
    result = {}
    for suite, variants in config.items():
        for v in variants:
            if v == variant:
                if not suite.endswith("_SUITE"):
                    fail("Invalid suite name", suite)
                base = suite[:-len("_SUITE")]
                variant_suite = "{}_{}_SUITE".format(base, variant)
                result[variant_suite] = suite
    return result

def _erl_opts(variant: str) -> list[str]:
    DISABLE_SSA_OPT = [
        "+no_bool_opt",
        "+no_share_opt",
        "+no_bsm_opt",
        "+no_fun_opt",
        "+no_ssa_opt",
        "+no_recv_opt",
    ]

    if variant == _COVER:
        return ["+line_coverage"]

    if variant == _DIALYZER:
        return ["+dialyzer"]

    if variant == _INLINE:
        return ["+inline"]

    if variant == _NO_BOOL_OPT:
        return ["+no_bool_opt"]

    if variant == _NO_COPT:
        return ["+no_copt"]

    if variant == _NO_COPT_SSA:
        return ["+no_copt", "+no_ssa_opt"]

    if variant == _NO_MODULE_OPT:
        return ["+no_module_opt"]

    if variant == _NO_OPT:
        return ["+no_copt"] + DISABLE_SSA_OPT + ["+no_postopt"]

    if variant == _NO_SSA_OPT:
        return DISABLE_SSA_OPT

    if variant == _NO_TYPE_OPT:
        return ["+no_type_opt"]

    if variant == _POST_OPT:
        return ["+no_copt"] + DISABLE_SSA_OPT

    if variant == _R26:
        return ["+r26"]

    fail("Unknown variant '%s'".format(variant))

def _gen_variant(*, name: str, src: str):
    src_mod = paths.replace_extension(src, "")
    out_mod = paths.replace_extension(name, "")
    native.genrule(
        name = name,
        cmd = """
        sed -e "s;-module(${SRC_MOD});-module(${OUT_MOD});" < ${SRCS} > ${OUT}
        """,
        env = {"SRC_MOD": src_mod, "OUT_MOD": out_mod},
        srcs = [src],
        out = name,
    )

def _variant_group_name(variant: str) -> str:
    return variant + "-tests"

test_variants = struct(
    common = [_COVER, _NO_COPT, _NO_COPT_SSA, _NO_MODULE_OPT, _NO_OPT, _NO_SSA_OPT, _NO_TYPE_OPT, _POST_OPT],

    cover = _COVER,
    dialyzer = _DIALYZER,
    inline = _INLINE,
    no_bool_opt = _NO_BOOL_OPT,
    no_copt = _NO_COPT,
    no_copt_ssa = _NO_COPT_SSA,
    no_module_opt = _NO_MODULE_OPT,
    no_opt = _NO_OPT,
    no_ssa_opt = _NO_SSA_OPT,
    no_type_opt = _NO_TYPE_OPT,
    post_opt = _POST_OPT,
    r26 = _R26,

    all = [
        _COVER,
        _DIALYZER,
        _INLINE,
        _NO_BOOL_OPT,
        _NO_COPT,
        _NO_COPT_SSA,
        _NO_MODULE_OPT,
        _NO_OPT,
        _NO_SSA_OPT,
        _NO_TYPE_OPT,
        _POST_OPT,
        _R26,
    ],

    makefile_variants = _makefile_variants,
    suites_for = _suites_for,
    erl_opts = _erl_opts,
    gen_variant = _gen_variant,
    variant_group_name = _variant_group_name,
)

def resources_for(suite_file: str) -> list[str]:
    suite_name = paths.replace_extension(paths.basename(suite_file), "")
    resource_dir_name = "{}_data".format(suite_name)

    suite_dir = paths.dirname(suite_file)
    resource_dir_path = paths.join(suite_dir, resource_dir_name)

    if not glob([paths.join(resource_dir_path, "*")]):
        return []

    # HACK: The name of this resource comes from the internals of the
    # erlang_tests() macro in the prelude
    resource_target = ":{}-{}".format(resource_dir_name, suite_file)
    return [resource_target]
