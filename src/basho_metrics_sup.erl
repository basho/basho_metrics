-module(basho_metrics_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_metric/2, start_metric/3]).

%% Supervisor callbacks
-export([init/1]).

start_metric(Type, Name) ->
    start_metric(Type, Name, []).

start_metric(Type, Name, Options) ->
    supervisor:start_child(?MODULE, child_spec(Type, Name, Options)).

child_spec(histogram, Name, Options) ->
    {Name,
     {basho_metrics_histogram, start_link, [Name, Options]},
     permanent, 5000, worker, dynamic};
child_spec(meter, Name, Options) ->
    {Name,
     {basho_metrics_meter, start_link, [Name, Options]},
     permanent, 5000, worker, dynamic}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.
