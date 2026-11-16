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

-export([all/0, suite/0]).

-export([system_info/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [system_info].

system_info(Config) when is_list(Config) ->
    Schedulers = erlang:system_info(schedulers),
    SchedulersOnline = erlang:system_info(schedulers_online),
    DirtyCpuSchedulers = erlang:system_info(dirty_cpu_schedulers),
    DirtyCpuSchedulersOnline = erlang:system_info(dirty_cpu_schedulers_online),
    DirtyIoSchedulers = erlang:system_info(dirty_io_schedulers),
    #{default := #{schedulers := Schedulers,
                   schedulers_online := SchedulersOnline,
                   dirty_cpu_schedulers := DirtyCpuSchedulers,
                   dirty_cpu_schedulers_online := DirtyCpuSchedulersOnline,
                   dirty_io_schedulers := DirtyIoSchedulers}} =
        erlang:system_info(scheduling_groups),
    ok.
