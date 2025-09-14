%% NO-OP hook. We handle the making of files with buck2
%% so we don't need to do anything here
-module(ts_install_cth).

%% Suite Callbacks
-export([init/2]).

-spec init(Id :: term(), Opts :: proplists:proplist()) ->
    {ok, State :: []}.
init(_Id, _Opts) ->
    {ok, []}.
