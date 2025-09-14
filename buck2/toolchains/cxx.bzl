def c_flags():
    COMMON = [
        "-DHAVE_CONFIG_H",
    ]
    OS =  select({
        "config//os:linux": [
            "-D_GNU_SOURCE",
        ],
        "DEFAULT": [],
    })
    return COMMON + OS
