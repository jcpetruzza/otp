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
         dirty_scheduler_counts/1]).

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
       dirty_scheduler_counts]}].

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

get_scheduling_groups(Args) ->
    {ok, Peer, Node} = ?CT_PEER(#{args => Args,
                                  env => [{"ERL_FLAGS", false},
                                          {"ERL_ZFLAGS", false}]}),
    SchedulingGroups = erpc:call(Node, erlang, system_info, [scheduling_groups]),
    peer:stop(Peer),
    SchedulingGroups.
