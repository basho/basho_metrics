-module(basho_metric).
-behaviour(gen_server).
-export([start_link/3, update/2, stats/1]).
-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {
          mod :: module(),
          modstate :: term(),
          options :: list()
}).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,1},
     {stats,1},
     {update,2}].

start_link(Mod, Name, Options) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Mod, Options], []).

update(Name, Value) ->
    gen_server:cast(Name, {update, Value}).

stats(Name) ->
    gen_server:call(Name, stats).

init([Mod, Options]) ->
    case proplists:get_value(tick, Options) of
        Interval when is_integer(Interval) ->
            timer:send_interval(Interval*1000, self(), tick);
        _ ->
            ok
    end,
    {ok, ModState} = Mod:init([Options]),
    {ok, #state{mod=Mod, modstate=ModState, options=Options}}.

handle_call(stats, _From, State=#state{mod=Mod, modstate=ModState}) ->
    {ok, Reply, NewModState} = Mod:stats(ModState),
    {reply, Reply, State#state{modstate=NewModState}}.

handle_cast({update, Value}, State=#state{mod=Mod, modstate=ModState}) ->
    {ok, NewModState} = Mod:update(Value, ModState),
    {noreply, State#state{modstate=NewModState}}.

handle_info(tick, State=#state{mod=Mod, modstate=ModState}) ->
    {ok, NewModState} = Mod:tick(ModState),
    {noreply, State#state{modstate=NewModState}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

