CFLAGS = [
        "-DHAVE_CONFIG_H",
    ] + select({ "config//os:linux": [
            "-D_GNU_SOURCE"
        ],
        "DEFAULT": [],
    })
