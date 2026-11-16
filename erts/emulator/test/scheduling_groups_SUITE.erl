%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(scheduling_groups_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, groups/0,
         end_per_testcase/2]).

-export([scheduler_counts/1,
         scheduler_percentages/1,
         inferred_scheduler_counts/1,
         dirty_scheduler_counts/1,
         process_scheduling_group/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [{group, command_line}].

groups() ->
    [{command_line, [],
      [scheduler_counts,
       scheduler_percentages,
       inferred_scheduler_counts,
       dirty_scheduler_counts,
       process_scheduling_group]}].

end_per_testcase(_Case, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

scheduler_counts(Config) when is_list(Config) ->
    #{default := #{schedulers_online := SchedulersOnline}} =
        get_scheduling_groups([]),
    DefaultOnline = min(8, SchedulersOnline),
    GroupOnline = min(1, SchedulersOnline),
    #{default := #{schedulers := 8,
                   schedulers_online := DefaultOnline},
      group := #{schedulers := 1,
                 schedulers_online := GroupOnline}} =
        get_scheduling_groups(["+S8", "+SG", "group", "+S1"]),

    #{default := #{schedulers := 8, schedulers_online := 6},
      group := #{schedulers := 4, schedulers_online := 2}} =
        get_scheduling_groups(["+S8:6", "+SG", "group", "+S4:2"]),

    #{default := #{schedulers := 8, schedulers_online := 2},
      group := #{schedulers := 4, schedulers_online := 4}} =
        get_scheduling_groups(["+S8", "+SG", "group", "+S4:4",
                               "+SG", "default", "+S:2"]),
    ok.

scheduler_percentages(Config) when is_list(Config) ->
    #{default := #{schedulers := Schedulers}} =
        get_scheduling_groups([]),
    GroupSchedulers = max(1, Schedulers - 4),
    #{default := #{schedulers := 2},
      group := #{schedulers := GroupSchedulers}} =
        get_scheduling_groups(["+S4", "+SP50", "+SG", "group"]),
    #{default := #{schedulers := 8, schedulers_online := 6},
      group := #{schedulers := 2, schedulers_online := 1}} =
        get_scheduling_groups(["+S8:6", "+SG", "group", "+S4:4",
                               "+SP50:25"]),
    #{default := #{schedulers := 8, schedulers_online := 6},
      group := #{schedulers := 2, schedulers_online := 1}} =
        get_scheduling_groups(["+S8:6", "+SG", "group", "+SP50:25",
                               "+S4:4"]),
    #{default := #{schedulers := 8, schedulers_online := 6},
      group := #{schedulers := 4, schedulers_online := 4}} =
        get_scheduling_groups(["+S8:6", "+SG", "group", "+S4:4",
                               "+SP25:25", "+SP100:100"]),
    ok.

inferred_scheduler_counts(Config) when is_list(Config) ->
    #{default := #{schedulers := Schedulers,
                   schedulers_online := SchedulersOnline}} =
        get_scheduling_groups([]),
    if
        Schedulers < 6; SchedulersOnline < 6 ->
            {comment, "Needs at least six schedulers online"};
        true ->
            DefaultSchedulers = SchedulersOnline - 1,
            GroupSchedulers = Schedulers - DefaultSchedulers,
            #{default :=
                  #{schedulers := DefaultSchedulers,
                    schedulers_online := DefaultSchedulers},
              group :=
                  #{schedulers := GroupSchedulers,
                    schedulers_online := 1}} =
                get_scheduling_groups(
                  ["+S" ++ integer_to_list(DefaultSchedulers),
                   "+SG", "group"]),

            SchedulersPerGroup = Schedulers div 3,
            SchedulersOnlinePerGroup = SchedulersOnline div 3,
            G2Schedulers = SchedulersPerGroup - 1,
            G2SchedulersOnline = SchedulersOnlinePerGroup - 1,
            G3Schedulers = max(1, SchedulersPerGroup * 50 div 100),
            G3SchedulersOnline =
                max(1, SchedulersOnlinePerGroup * 50 div 100),
            #{default :=
                  #{schedulers := SchedulersPerGroup,
                    schedulers_online := SchedulersOnlinePerGroup},
              g2 :=
                  #{schedulers := G2Schedulers,
                    schedulers_online := G2SchedulersOnline},
              g3 :=
                  #{schedulers := G3Schedulers,
                    schedulers_online := G3SchedulersOnline}} =
                get_scheduling_groups(["+SG", "g2", "+S-1",
                                       "+SG", "g3", "+SP50"]),
            ok
    end.

dirty_scheduler_counts(Config) when is_list(Config) ->
    #{default :=
          #{schedulers := 8,
            schedulers_online := 6,
            dirty_cpu_schedulers := 8,
            dirty_cpu_schedulers_online := 6,
            dirty_io_schedulers := 3},
      group :=
          #{schedulers := 4,
            schedulers_online := 2,
            dirty_cpu_schedulers := 4,
            dirty_cpu_schedulers_online := 2,
            dirty_io_schedulers := 4}} =
        get_scheduling_groups(["+S8:6", "+SDcpu8:6", "+SDio3",
                               "+SG", "group",
                               "+S4:2", "+SDcpu4:2", "+SDio4"]),
    #{default :=
          #{schedulers := 8,
            schedulers_online := 6,
            dirty_cpu_schedulers := 8,
            dirty_cpu_schedulers_online := 6},
      group :=
          #{schedulers := 4,
            schedulers_online := 2,
            dirty_cpu_schedulers := 2,
            dirty_cpu_schedulers_online := 1}} =
        get_scheduling_groups(["+S8:6", "+SDcpu8:6",
                               "+SG", "group",
                               "+S4:2", "+SDcpu4:2", "+SDPcpu50:50"]),
    #{default :=
          #{schedulers := 8,
            schedulers_online := 6,
            dirty_cpu_schedulers := 8,
            dirty_cpu_schedulers_online := 6},
      group :=
          #{schedulers := 4,
            schedulers_online := 2,
            dirty_cpu_schedulers := 2,
            dirty_cpu_schedulers_online := 1}} =
        get_scheduling_groups(["+S8:6", "+SDcpu8:6",
                               "+SG", "group",
                               "+S4:2", "+SDPcpu50:50", "+SDcpu4:2"]),
    ok.

process_scheduling_group(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(#{args => ["+S2:2", "+SG", "group", "+S2:2"],
                                  env => [{"ERL_FLAGS", false},
                                          {"ERL_ZFLAGS", false}]}),
    ok = erpc:call(Node, fun() -> process_scheduling_group_test() end),
    peer:stop(Peer),
    ok.

process_scheduling_group_test() ->
    #{default := #{schedulers := DefaultCount},
      group := #{schedulers := GroupCount}} =
        erlang:system_info(scheduling_groups),
    DefaultSchedulers = lists:seq(1, DefaultCount),
    GroupSchedulers = lists:seq(DefaultCount + 1,
                                DefaultCount + GroupCount),

    {scheduling_group, default} = process_info(self(), scheduling_group),
    expect_badarg(fun() -> process_flag(scheduling_group, unknown) end),
    {scheduling_group, default} = process_info(self(), scheduling_group),

    0 = process_flag(scheduler, 1),
    expect_badarg(fun() -> process_flag(scheduling_group, group) end),
    {scheduling_group, default} = process_info(self(), scheduling_group),
    1 = process_flag(scheduler, 0),

    DefaultWorkers = spawn_workers(100),
    default = process_flag(scheduling_group, group),
    true = lists:member(erlang:system_info(scheduler_id), GroupSchedulers),
    GroupWorkers = spawn_workers(100),
    {scheduling_group, default} =
        process_info(hd(DefaultWorkers), scheduling_group),
    {scheduling_group, group} =
        process_info(hd(GroupWorkers), scheduling_group),

    expect_badarg(fun() -> process_flag(scheduler, 1) end),
    Parent = self(),
    spawn_opt(fun() ->
                      Parent ! {scheduler_child,
                                process_info(self(), scheduling_group),
                                erlang:system_info(scheduler_id)}
              end,
              [link, {scheduler, 1}]),
    receive
        {scheduler_child, {scheduling_group, default}, SchedulerId} ->
            true = lists:member(SchedulerId, DefaultSchedulers)
    end,

    group = process_flag(scheduling_group, default),
    true = lists:member(erlang:system_info(scheduler_id), DefaultSchedulers),

    check_workers(DefaultWorkers, DefaultSchedulers),
    check_workers(GroupWorkers, GroupSchedulers),

    move_workers(DefaultWorkers, default, group, GroupSchedulers),
    move_workers(GroupWorkers, group, default, DefaultSchedulers),
    {scheduling_group, group} =
        process_info(hd(DefaultWorkers), scheduling_group),
    {scheduling_group, default} =
        process_info(hd(GroupWorkers), scheduling_group),

    check_workers(DefaultWorkers, GroupSchedulers),
    check_workers(GroupWorkers, DefaultSchedulers),

    lists:foreach(fun(Pid) -> Pid ! stop end, DefaultWorkers ++ GroupWorkers),
    ok.

expect_badarg(Fun) ->
    try Fun() of
        _ -> ct:fail(expected_badarg)
    catch
        error:badarg -> ok
    end.

spawn_workers(Count) ->
    [spawn_link(fun scheduling_group_worker/0) || _ <- lists:seq(1, Count)].

scheduling_group_worker() ->
    receive
        {check, Parent, Ref} ->
            Parent ! {Ref, self(), sample_scheduler_ids(100, [])},
            scheduling_group_worker();
        {move, Parent, Ref, Group} ->
            OldGroup = process_flag(scheduling_group, Group),
            Parent ! {Ref, self(), OldGroup,
                      erlang:system_info(scheduler_id)},
            scheduling_group_worker();
        stop ->
            ok
    end.

sample_scheduler_ids(0, SchedulerIds) ->
    SchedulerIds;
sample_scheduler_ids(Count, SchedulerIds) ->
    SchedulerId = erlang:system_info(scheduler_id),
    erlang:yield(),
    sample_scheduler_ids(Count - 1, [SchedulerId | SchedulerIds]).

check_workers(Workers, SchedulerIds) ->
    Ref = make_ref(),
    [Pid ! {check, self(), Ref} || Pid <- Workers],
    check_worker_replies(Workers, Ref, SchedulerIds).

check_worker_replies([], _Ref, _SchedulerIds) ->
    ok;
check_worker_replies(Workers, Ref, SchedulerIds) ->
    receive
        {Ref, Pid, ActualSchedulerIds} ->
            true = lists:member(Pid, Workers),
            true = lists:all(fun(SchedulerId) ->
                                     lists:member(SchedulerId, SchedulerIds)
                             end,
                             ActualSchedulerIds),
            check_worker_replies(lists:delete(Pid, Workers),
                                 Ref,
                                 SchedulerIds)
    end.

move_workers(Workers, OldGroup, NewGroup, SchedulerIds) ->
    Ref = make_ref(),
    [Pid ! {move, self(), Ref, NewGroup} || Pid <- Workers],
    move_worker_replies(Workers, Ref, OldGroup, SchedulerIds).

move_worker_replies([], _Ref, _OldGroup, _SchedulerIds) ->
    ok;
move_worker_replies(Workers, Ref, OldGroup, SchedulerIds) ->
    receive
        {Ref, Pid, ActualOldGroup, SchedulerId} ->
            true = lists:member(Pid, Workers),
            OldGroup = ActualOldGroup,
            true = lists:member(SchedulerId, SchedulerIds),
            move_worker_replies(lists:delete(Pid, Workers),
                                Ref,
                                OldGroup,
                                SchedulerIds)
    end.

get_scheduling_groups(Args) ->
    {ok, Peer, Node} = ?CT_PEER(#{args => Args,
                                  env => [{"ERL_FLAGS", false},
                                          {"ERL_ZFLAGS", false}]}),
    SchedulingGroups = erpc:call(Node, erlang, system_info, [scheduling_groups]),
    peer:stop(Peer),
    SchedulingGroups.
