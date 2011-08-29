-module(basho_metrics_histogram).
-behaviour(basho_metric).
-export([start_link/2, init/1, update/2, stats/1]).

start_link(Name, Options) when is_atom(Name) ->
    basho_metric:start_link(?MODULE, Name, Options).

init([_Options]) ->
    basho_metrics_nifs:histogram_new().

update(Value, Histogram) ->
    basho_metrics_nifs:histogram_update(Histogram, Value),
    {ok, Histogram}.

stats(Histogram) ->
    {ok, basho_metrics_nifs:histogram_stats(Histogram), Histogram}.
       
