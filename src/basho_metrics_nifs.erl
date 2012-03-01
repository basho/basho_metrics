%% -------------------------------------------------------------------
%%
%% basho_metrics: fast performance metrics for Erlang.
%%
%% inspired and partially derived from Coda Hale's 'metrics' 
%% Copyright (c) 2010-2001 Coda Hale
%% https://github.com/codahale/metrics/blob/development/LICENSE.md
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
         meter_new/1,
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
    PrivDir = try
        {ok, AppName} = application:get_application(?MODULE),
        case code:priv_dir(AppName) of
            {error, Reason} ->
                throw(Reason);
            Result ->
                Result
        end
    catch _:_ ->
        EbinDir = filename:dirname(code:which(?MODULE)),
        AppPath = filename:dirname(EbinDir),
        filename:join(AppPath, "priv")
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

close_to(X, N, Delta) ->
    abs(X-N) < Delta.

histogram_test() ->
    {ok, H} = ?MODULE:histogram_new([{size, 1000}]),
    [?MODULE:histogram_update(H, I) || I <- lists:seq(1, 10000)],
    Stats = ?MODULE:histogram_stats(H),
    ?assertEqual(proplists:get_value(min, Stats), 1),
    ?assertEqual(proplists:get_value(max, Stats), 10000),
    ?assertEqual(proplists:get_value(count, Stats),  10000),
    ?assert(close_to(proplists:get_value(stddev, Stats), 2886, 10)),
    ?assert(close_to(proplists:get_value(mean, Stats), 5000, 100)),
    ?assert(close_to(proplists:get_value(p50, Stats), 5000, 100)),
    ?assert(close_to(proplists:get_value(p95, Stats), 9500, 100)),
    ?assert(close_to(proplists:get_value(p99, Stats), 9900, 100)).

-endif.

