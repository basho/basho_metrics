-module(basho_metrics_meter).
-behaviour(basho_metric).
-export([start_link/2, init/1, update/2, stats/1, tick/1]).

start_link(Name, Options) when is_atom(Name) ->
    basho_metric:start_link(?MODULE, Name, [{tick, 5}|Options]).

init([_Options]) ->
    basho_metrics_nifs:meter_new().

update(Value, Meter) ->
    basho_metrics_nifs:meter_update(Meter, Value),
    {ok, Meter}.

stats(Meter) ->
    {ok, basho_metrics_nifs:meter_stats(Meter), Meter}.

tick(Meter) ->
    basho_metrics_nifs:meter_tick(Meter),
    {ok, Meter}.
