-module(ensure_distributed_cth).

%% CTH Callbacks
-export([init/2]).

-type state() :: none.

%% Always called before any other callback function.
-spec init(Id :: term(), Opts :: proplists:proplist()) ->
    {ok, State :: state()}.
init(_Id, _Opts) ->
    case node() of
        'nonode@nohost' ->
            Node = 'test_server',
            {ok, _} = net_kernel:start(Node, #{name_domain => shortnames});
        _ ->
            ok
    end,
    State = none,
    {ok, State}.
