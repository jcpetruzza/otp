def cxx_tools_info():
    # Using msvc_tools for windows is buck2's current default behaviour,
    # but we'd need to check what are the correct flags to use, so
    # we stick to clang
    #
    # if host_info().os.is_windows:
    #     return "prelude//toolchains/msvc:msvc_tools"

    return "prelude//toolchains/cxx/clang:path_clang_tools"


def c_flags():
    COMMON = [
        "-DHAVE_CONFIG_H",
        "-D_GNU_SOURCE",

        "-DUSE_THREADS",
        "-D_THREAD_SAFE",
        "-D_REENTRANT",
        "-DPOSIX_THREADS",
        "-D_POSIX_THREAD_SAFE_FUNCTIONS",

        "-Werror=undef",
        "-Werror=implicit",
        "-Werror=return-type",

        "-fno-common",
        "-fstrict-flex-arrays=3",
        "-fno-strict-aliasing",
        "-fno-delete-null-pointer-checks",
        "-fno-strict-overflow",
        "-fexceptions",
        "-fcf-protection=full",
        "-fstack-protector-strong",
        "-fstack-clash-protection",

        "-U_FORTIFY_SOURCE",
        "-D_FORTIFY_SOURCE=3",

        "-g",

        "-Wall",
        "-Wstrict-prototypes",
        "-Wpointer-arith",
        "-Wmissing-prototypes",
        "-Wdeclaration-after-statement",
    ]

    FLAVOR = select({
        "otp//buck2/config/emu_flavor:jit": [
            "-DBEAMASM=1"
        ],
        "otp//buck2/config/emu_flavor:emu": [],
        "DEFAULT": []
    })

    DEBUG_TYPE = select({
        "otp//buck2/config/emu_type:debug": [
            "-g",
            "-DDEBUG",
        ],
        "DEFAULT": [],
    })

    GCOV_TYPE = select({
        "otp//buck2/config/emu_type:gcov": [
            "-DERTS_GCOV",
            "-fprofile-arcs",
            "-ftest-coverage",
        ],
        "DEFAULT": [],
    })

    VALGRIND_TYPE = select({
        "otp//buck2/config/emu_type:valgrind": [
            "-DVALGRIND",
        ],
        "DEFAULT": [],
    })

    ASAN_TYPE = select({
        "otp//buck2/config/emu_type:asan": [
            "-fsanitize=address",
            "-fsanitize-recover=address",
            "-DADDRESS_SANITIZER",
        ],
        "DEFAULT": [],
    })

    GPROF_TYPE = select({
        "otp//buck2/config/emu_type:gprof": [
            "-DGPROF",
            "-pg",
        ],
        "DEFAULT": [],
    })

    LCNT_TYPE = select({
        "otp//buck2/config/emu_type:lcnt": [
            "-DERTS_ENABLE_LOCK_COUNT",
        ],
        "DEFAULT": [],
    })

    FRMPTR_TYPE = select({
        "otp//buck2/config/emu_type:frmptr": [
            "-DERTS_FRMPTR",
        ],
        "DEFAULT": [],
    })

    ICOUNT_TYPE = select({
        "otp//buck2/config/emu_type:icount": [
            "-DERTS_OPCODE_COUNTER_SUPPORT",
        ],
        "DEFAULT": [],
    })

    return (
        COMMON +
        FLAVOR +
        DEBUG_TYPE +
        GCOV_TYPE +
        VALGRIND_TYPE +
        ASAN_TYPE +
        GPROF_TYPE +
        LCNT_TYPE +
        FRMPTR_TYPE +
        ICOUNT_TYPE +
        _opt_flags() +
        _fp_flags() +
        _jump_table_flags() +
        _inline_flags()
    )

def _opt_flags():
    return select({
        "otp//buck2/config/emu_type:debug": [
            "-Og",
        ],
        "otp//buck2/config/emu_type:gcov": [
            "-O0",
        ],
        "DEFAULT": [
            "-O2",
        ],
    })

def _fp_flags():
    return select({
        "otp//buck2/config/emu_type:frmptr": ["-fomit-frame-pointer"],
        "otp//buck2/config/emu_flavor:jit": ["-fomit-frame-pointer"],
        "otp//buck2/config/emu_type:asan": ["-fomit-frame-pointer"],
        "DEFAULT": ["-fno-omit-frame-pointer"],
    })

def _jump_table_flags():
    return select({
        "otp//buck2/config/emu_type:debug": ["-DNO_JUMP_TABLE"],
        "otp//buck2/config/emu_type:gcov": ["-DNO_JUMP_TABLE"],
        "otp//buck2/config/emu_type:valgrind": ["-DNO_JUMP_TABLE"],
        "DEFAULT": [],
    })

def _inline_flags():
    return select({
        "otp//buck2/config/emu_type:gcov": [
            "-DERTS_CAN_INLINE=0",
            "-DERTS_INLINE=",
        ],
        "otp//buck2/config/emu_type:gprof": [
            "-DERTS_CAN_INLINE=0",
            "-DERTS_INLINE=",
            "-fno-inline-functions",
        ],
        "DEFAULT": [],
    })

def link_flags():

    DEBUG_FLAGS = select({
        "otp//buck2/config/emu_type:debug-win": ["-g"],
        "DEFAULT": [],
    })

    GCOV_FLAGS = select({
        "otp//buck2/config/emu_type:gcov": ["-lgcov"],
        "DEFAULT": []
    })

    ASAN_FLAGS = select({
        "otp//buck2/config/emu_type:asan": ["-fsanitize=address"],
        "DEFAULT": []
    })

    GPROF_FLAGS = select({
        "otp//buck2/config/emu_type:gprof": ["-pg"],
        "DEFAULT": []
    })

    return (
        DEBUG_FLAGS +
        GCOV_FLAGS +
        ASAN_FLAGS +
        GPROF_FLAGS
    )
