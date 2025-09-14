%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(bootstrap_dependency_analyzer).
-moduledoc """
Like buck2's dependency_analyzer.erl, but doesn't depend on anything other than kernel
and stdlib (so no epp_dodger), so it can be used while bootstrapping the Erlang compiler.
(based on regexes, though, so less robust)
""".

-export([main/1]).

-type entry() ::
    #{type := include, file := binary()}
    | #{type := include_lib, file := binary(), app := binary()}
    | #{type := behaviour, file := binary()}
    | #{type := parse_transform, file := binary()}.

%% entry point
main([InFile]) ->
    do(InFile, stdout);
main([InFile, OutFile]) ->
    do(InFile, {file, OutFile});
main(_) ->
    usage(),
    erlang:halt(1).

-spec usage() -> ok.
usage() ->
    io:format("dependency_analyzer.escript some_file.(h|e)rl [out.term]").

-spec do(file:filename(), {file, file:filename()} | stdout) -> ok.
do(InFile, OutSpec) ->
    {ok, Handle} = file:open(InFile, [read, raw, binary]),
    Collected = collect(Handle),
    file:close(Handle),

    Entries = lists:sort(Collected),
    OutData = json:encode(Entries),
    case OutSpec of
        {file, File} ->
            ok = write_file(File, OutData);
        stdout ->
            io:format("~s~n", [OutData])
    end.

-spec collect(Handle) -> [entry()]  when
    Handle :: file:io_device().
collect(Handle) ->
    {ok, IncludeRegex} = re:compile(~"^-include\\(\"([^\"]*)\"\\)."),
    {ok, InclLibRegex} = re:compile(~"^-include_lib\\(\"([^/]*)/include/([^\"]*)\"\\)."),
    {ok, BehavioRegex} = re:compile(~"^-behaviour\\(([^)]*)\\)."),
    {ok, ParseTrRegex} = re:compile(~"^-compile\\({parse_transform,\s*([^}\s]*)\s*}\."),

    Matchers = [
        {include, IncludeRegex},
        {include_lib, InclLibRegex},
        {behaviour, BehavioRegex},
        {parse_transform, ParseTrRegex}
    ],
    collect(Handle, Matchers, #{in_doc => false, entries => []}).

-spec collect(Handle, Matchers, Accum) -> Entries when
    Handle :: file:io_device(),
    Matchers :: [{include | include_lib | behaviour | parse_transform, re:mp()}],
    Accum :: #{in_doc := boolean(), entries := Entries},
    Entries :: [entry()].
collect(Handle, Matchers, Accum) ->
    #{in_doc := InDoc, entries := Entries} = Accum,
    case file:read_line(Handle) of
        eof ->
            Entries;
        {ok, ~"-doc \"\"\"\n"} when not InDoc ->
            collect(Handle, Matchers, Accum#{in_doc => true});
        {ok,  ~"-moduledoc \"\"\"\n"} when not InDoc ->
            collect(Handle, Matchers, Accum#{in_doc => true});
        {ok, ~"\"\"\".\n"} when InDoc ->
            collect(Handle, Matchers, Accum#{in_doc => false});
        {ok, _Line} when InDoc ->
            collect(Handle, Matchers, Accum);
        {ok, Line} when not InDoc ->
            case match_line(Line, Matchers) of
                {match, Entry} ->
                    collect(Handle, Matchers, Accum#{entries => [Entry | Entries]});
                nomatch ->
                    collect(Handle, Matchers, Accum)
            end
    end.

-spec match_line(Line, Matchers) -> nomatch | {match, Entry} when
    Line :: binary(),
    Matchers :: [{include | include_lib | behaviour | parse_transform, re:mp()}],
    Entry :: entry().
match_line(_Line, []) ->
    nomatch;
match_line(Line, [{include, Regex} | Matchers]) ->
    case re:run(Line, Regex, [{capture, [1], binary}]) of
        {match, [IncludedFile]} ->
            {match, #{type => include, file => IncludedFile}};
        nomatch ->
            match_line(Line, Matchers)
    end;
match_line(Line, [{include_lib, Regex} | Matchers]) ->
    case re:run(Line, Regex, [{capture, [1,2], binary}]) of
        {match, [App, IncludedFile]} ->
            {match, #{type => include_lib, file => IncludedFile, app => App}};
        nomatch ->
            match_line(Line, Matchers)
    end;
match_line(Line, [{behaviour, Regex} | Matchers]) ->
    case re:run(Line, Regex, [{capture, [1], binary}]) of
        {match, [BehaviourMod]} ->
            BehaviourFile = <<BehaviourMod/binary, ".erl">>,
            {match, #{type => behaviour, file => BehaviourFile}};
        nomatch ->
            match_line(Line, Matchers)
    end;
match_line(Line, [{parse_transform, Regex} | Matchers]) ->
    case re:run(Line, Regex, [{capture, [1], binary}]) of
        {match, [ParseTransformMod]} ->
            ParseTransformFile = <<ParseTransformMod/binary, ".erl">>,
            {match, #{type => parse_transform, file => ParseTransformFile}};
        nomatch ->
            match_line(Line, Matchers)
    end.

-spec write_file(file:filename(), iolist()) -> ok.
write_file(File, Data) ->
    case file:open(File, [write, binary, raw]) of
        {ok, Handle} ->
            try
                % We use file:pwrite instead of file:write_file to work around
                % the latter needlessly flattening iolists (as returned by
                % json:encode/1, etc.) to a binary
                file:pwrite(Handle, 0, Data)
            after
                file:close(Handle)
            end,
            ok
    end.
