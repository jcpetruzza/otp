-module(set_path_cth).


%% CTH Callbacks
-export([init/2]).
-export([pre_init_per_suite/3]).

-include_lib("kernel/include/file.hrl").

-type state() :: none.

-type config() :: proplists:proplist().
-type reason() :: term().
-type skip_or_fail() :: {skip, reason()} | {fail, reason()}.

%% Always called before any other callback function.
-spec init(Id :: term(), Opts :: proplists:proplist()) ->
    {ok, State :: state()}.
init(_Id, _Opts) ->
    State = none,
    {ok, State}.

%% Called before init_per_suite is called.
-spec pre_init_per_suite(Suite :: atom(),
			 Config :: config(),
			 State :: state()) ->
	{config() | skip_or_fail(), NewState :: state()}.
pre_init_per_suite(_Suite, Config ,State) ->
    % put running OTP first in the PATH
    ok = add_to_PATH(filename:join([code:root_dir(), "bin"])),

    DataDir = proplists:get_value(data_dir, Config),
    ExecutableFiles0 = find_all_executables(DataDir),

    ExecutableFiles1 = maps:filtermap(
        fun
            (_FileName, [Dir]) -> {true, Dir};
            (_FileName, _Dirs) -> false
        end,
        ExecutableFiles0
    ),

    UniqueExecutableNames = maps:keys(ExecutableFiles1),
    AmbiguousExecutables = maps:without(UniqueExecutableNames, ExecutableFiles0),

    Result = case maps:to_list(AmbiguousExecutables) of
        [] ->
            DirsWithExecutables = maps:values(ExecutableFiles1),
            lists:foreach(fun add_to_PATH/1, DirsWithExecutables),
            Config;

        [{AmbiguousName, _Dirs}|_] ->
            {fail, {ambiguous_executable_in_PATH, AmbiguousName}}
    end,
    {Result, State}.

% -------------------------------------------------------------------
% Helpers
% -------------------------------------------------------------------

-spec add_to_PATH(Dir :: os:env_var_value()) -> ok.
add_to_PATH(Dir) ->
    PATH_VAR = "PATH",
    PATH0 = os:getenv(PATH_VAR),
    PATH1 = string:join([Dir, PATH0], path_separator()),
    true = os:putenv(PATH_VAR, PATH1),
    ok.

-spec path_separator() -> string().
path_separator() ->
    case os:type() of
        {unix, _} -> ":";
        {win32, _} -> ";"
    end.


-spec find_all_executables(Dir) ->  #{BaseName => [Path]} when
    Dir :: file:filename(),
    BaseName :: file:filename_all(),
    Path :: file:filename_all().
find_all_executables(Dir) ->
    filelib:fold_files(
        Dir,
        _AnyMatchRegExp = "",
        _Recursive = true,
        fun (F, Acc) ->
            case is_executable(F) of
                true ->
                    FileName = filename:basename(F),
                    DirName = filename:dirname(F),
                    Paths = maps:get(FileName, Acc, []),
                    Acc#{FileName => [DirName|Paths]};
                false ->
                    Acc
            end
        end,
        #{}
    ).

-spec is_executable(Filename :: file:filename()) -> boolean().
is_executable(Filename) ->
    case filelib:is_regular(Filename) of
        false ->
            false;
        true ->
            case os:type() of
                {unix, _} ->
                    case file:read_file_info(Filename) of
                        {ok, FileInfo} ->
                            Mode = FileInfo#file_info.mode,
                            % Check if any execute bit is set (owner, group, other)
                            (Mode band 8#100) =:= 8#100 orelse
                            (Mode band 8#010) =:= 8#010 orelse
                            (Mode band 8#001) =:= 8#001;
                        {error, _} ->
                            false
                    end;
                {win32, _} ->
                    Ext = string:lowercase(filename:extension(Filename)),
                    lists:member(Ext, [".exe", ".bat", ".cmd", ".com"])
            end
    end.
