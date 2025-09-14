-module(otp_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").

% CT callbacks
-export([all/0, suite/0]).

% Testcases
-export([test_finds_exec_resources/1]).

all() -> [
    test_finds_exec_resources
].

suite() ->
    [{ct_hooks,[ts_install_cth]}].

test_finds_exec_resources(_Config) ->
    ?assert(os:find_executable("foo.exe") /= false),
    ?assert(os:find_executable("test-exec") /= false),
    ok.
