-module(basho_metrics_nifs).

-export([histogram_new/0,
         histogram_update/2,
         histogram_stats/1,
         histogram_clear/1,
         meter_new/0,
         meter_update/2,
         meter_tick/1,
         meter_stats/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).

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

histogram_new() ->
    ?nif_stub.

histogram_update(_Ref, _Samples) ->
    ?nif_stub.

histogram_stats(_Ref) ->
    ?nif_stub.

histogram_clear(_Ref) ->
    ?nif_stub.

meter_new() ->
    ?nif_stub.

meter_update(_Ref, _Samples) ->
    ?nif_stub.

meter_stats(_Ref) ->
    ?nif_stub.

meter_tick(_Ref) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

simple_test() ->
    {ok, H} = ?MODULE:histogram_new();
    [?MODULE:update(H, lists:seq(0, 1000000))],
    ?debugFmt("~p~n", [?MODULE:stats(H)]).

-endif.
