load("@prelude//paths.bzl", "paths")

def flatten_includes(headers: list[str]) -> dict[str, str]:
    """
    Ignore the path of the header files and make them available by name.
    """
    return {
        paths.basename(header): header for header in headers
    }


def strip_include_prefix(prefix: str, headers: list[str]) -> dict[str, str]:
    """
    Make each header available at their path, but stripping a prefix
    """
    prefix_len = len(prefix)
    def flatten(header):
        if not header.startswith(prefix):
            fail("Expecting source prefix {} but got {}".format(prefix, header))

        return header[prefix_len:]

    return {flatten(header): header for header in headers}

def headers_union(*headers) -> dict[str,str] | Select:
    def to_items(d):
        return d.items()

    items_list = []
    for h in headers:
       items_list = items_list + select_map(h, to_items)

    return select_map(items_list, dict)
