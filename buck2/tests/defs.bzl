load("@prelude//paths.bzl", "paths")

def otp_tests(
    *,
    name: str,
    suites: list[str],
    deps: list[str] | None = None,
    suite_deps: dict[str, list[str]] | None = None,
    **kwargs
):
    COMMON_DEPS = [
        "@otp//buck2/tests:buck2-test-support",
    ]
    COMMON_CT_HOOKS = [
        "{set_path_cth, [], -65535}",
        "{ensure_distributed_cth, [], -65534}",
    ]

    deps = _append_unique(COMMON_DEPS, deps or [])
    suite_deps = suite_deps or {}

    extra_ct_hooks = kwargs.pop("extra_ct_hooks", [])
    extra_ct_hooks = _append_unique(COMMON_CT_HOOKS, extra_ct_hooks)

    suites_no_extra_deps = [suite for suite in suites if suite not in suite_deps]
    cases = [([suite], sdeps) for suite, sdeps in suite_deps.items()]
    cases.append((suites_no_extra_deps, []))

    for case, case_deps in cases:
        native.erlang_tests(
            suites = case,
            deps = deps + case_deps,
            extra_ct_hooks = extra_ct_hooks,
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
