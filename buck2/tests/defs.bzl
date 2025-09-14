load("@prelude//paths.bzl", "paths")

def otp_tests(*, name: str, suites: list[str], deps: list[str]  = [], **kwargs):
    COMMON_DEPS = [
        "@otp//buck2/tests:test-support",
    ]
    COMMON_CT_HOOKS = [
        "{set_path_cth, [], -65535}",
    ]

    extra_ct_hooks = kwargs.pop("extra_ct_hooks", [])

    native.erlang_tests(
        suites = suites,
        deps = _append_unique(COMMON_DEPS, deps),
        extra_ct_hooks = _append_unique(COMMON_CT_HOOKS, extra_ct_hooks),
        **kwargs
    )

    native.test_suite(
        name = name,
        tests = [_target_name(suite) for suite in suites]
    )

def _target_name(suite_path: str) -> str:
    suite_filename = paths.basename(suite_path)
    suite_name, _ = paths.split_extension(suite_filename)
    return ":" + suite_name

def _append_unique(l: list[str], r: list[str]) -> list[str]:
    """Appends two lists (preserving order), removing duplicates"""
    if not l:
        return r
    if not r:
        return l
    return _unique(l + r)

def _unique(l: list[str]) -> list[str]:
    """Removes duplicates, preserving order"""
    result = []
    seen = set()
    for x in l:
        if x not in seen:
            result.append(x)
            seen.add(x)
    return result
