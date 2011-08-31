%% -------------------------------------------------------------------
%%
%% basho_metrics: fast performance metrics for Erlang.
%%
%% Copyright (c) 2007-2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_metrics_nifs).
-export([histogram_new/0,
         histogram_new/1,
         histogram_update/2,
         histogram_stats/1,
         histogram_clear/1,
         meter_new/0,
         meter_update/2,
         meter_tick/1,
         meter_stats/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).

-opaque histogram() :: binary().

-opaque meter() :: binary().

-type histogram_options() :: [{size, pos_integer()}].

-type meter_options() :: [{tick_interval, pos_integer()}].

-type histogram_stats() :: [{mean, integer()} |
                            {count, integer()} |
                            {stddev, integer()} |
                            {p50, integer()} |
                            {p95, integer()} |
                            {p95, integer()} |
                            {p99, integer()}].

-type meter_stats() :: [{one, integer()}  |
                        {five, integer()} |
                        {fifteen, integer()} |
                        {count, non_neg_integer()}].

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).


-spec histogram_new() -> {ok, histogram()}.
histogram_new() ->
    histogram_new([]).

-spec histogram_new(Options::histogram_options()) -> {ok, histogram()}.
histogram_new(_Options) ->
    ?nif_stub.

-spec histogram_update(histogram(), Sample::pos_integer()) -> ok.
histogram_update(_Ref, _Sample) ->
    ?nif_stub.

-spec histogram_stats(histogram()) -> histogram_stats().
histogram_stats(_Ref) ->
    ?nif_stub.

-spec histogram_clear(histogram()) -> ok.
histogram_clear(_Ref) ->
    ?nif_stub.

-spec meter_new() -> {ok, meter()}.
meter_new() ->
    meter_new([]).

-spec meter_new(Options::meter_options()) -> {ok, meter()}.
meter_new(_Options) ->
    ?nif_stub.

-spec meter_update(histogram(), Sample::pos_integer()) -> ok.
meter_update(_Ref, _Sample) ->
    ?nif_stub.

-spec meter_stats(meter()) -> meter_stats().
meter_stats(_Ref) ->
    ?nif_stub.

-spec meter_tick(meter()) -> ok.
meter_tick(_Ref) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

simple_test() ->
    {ok, H} = ?MODULE:histogram_new(),
    [?MODULE:histogram_update(H, I) || I <- lists:seq(0, 1000000)],
    [{min,0},
     {max,1000000},
     {mean,500000},
     {count,1000001},
     {stddev,0},
     {p50,490606},
     {p95,954205},
     {p99,990504}] = ?MODULE:histogram_stats(H).

-endif.

