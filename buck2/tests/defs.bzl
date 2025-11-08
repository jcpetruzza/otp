load("@prelude//paths.bzl", "paths")

def otp_tests(
    *,
    name: str,
    suites: list[str],
    overrides: dict[str, dict[str, typing.Any]] | None = None,
    erl_opts: list[str] | None = None,
    deps: list[str] | None = None,
    env: dict[str, str] | None = None,
    extra_ct_hooks: list[str] | None = None,
    patch: str | None = None,
    **kwargs
):
    os_env = kwargs.pop("os_env", None)
    if erl_opts:
        key = "ERL_COMPILER_OPTIONS"
        if key in erl_opts:
            fail("ERL_COMPILER_OPTIONS already set")
        opts = []
        for opt in erl_opts:
            if opt.startswith("+"):
                opt = opt[1:]
            opts.append(opt)
        value = "[{}]".format(", ".join(opts))
        os_env = os_env if os_env != None else {}
        os_env[key] = value

    COMMON_DEPS = [
        "@otp//buck2/tests:buck2-test-support",
    ]
    COMMON_CT_HOOKS = [
        "{set_path_cth, [], -65535}",
        "{ensure_distributed_cth, [], -65534}",
    ]
    COMMON_ENV = {"BUCK2_ERLANG_OTP_TEST": "1"}

    kwargs["deps"] = _append_unique(COMMON_DEPS, deps or [])
    kwargs["extra_ct_hooks"] = _append_unique(COMMON_CT_HOOKS, extra_ct_hooks or [])
    kwargs["env"] =  _merge(env or {}, COMMON_ENV)

    if patch != None:
        kwargs["patch"] = patch

    overrides = overrides or {}
    suites_no_overrides = [suite for suite in suites if suite not in overrides]

    known_suites = set(suites)

    cases = [(suites_no_overrides, kwargs)]
    for suite, override in overrides.items():
        if suite not in known_suites:
            fail("Overriding unknown suite:", suite)
        cases.append(([suite], _merge(kwargs, override)))

    for case_suites, case_kwargs in cases:
        patch = case_kwargs.pop("patch", None)
        if patch != None:
            case_suites = _do_patch(case_suites, patch)

        native.erlang_tests(
            suites = case_suites,
            os_env = os_env,
            **case_kwargs
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

def _merge(l, r):
    if l == None:
        return r

    if r == None:
        return l

    if isinstance(l, list):
        if not isinstance(r, list):
            fail("list expected, got", r)

        return _append_unique(l, r)

    if isinstance(l, dict):
        if not isinstance(r, dict):
            fail("dict expected, got", r)

        result = {}
        for lk, lv in l.items():
            result[lk] = _merge(lv, r.get(lk))
        for rk, rv in r.items():
            if rk not in l:
                result[rk] = rv
        return result

    return r

def _do_patch(suites: list[str], patch_cmd: str) -> list[str]:
    result = []
    for suite in suites:
        name = paths.join("__patched__", suite)
        if name.startswith(":"):
            name = name[1:]

        native.genrule(
            name = name,
            cmd = """
            {patch_cmd} < $SRCS > $OUT
            """.format(patch_cmd=patch_cmd.strip()),
            srcs = [suite],
            out = name,
        )
        result.append(":" + name)

    return result
