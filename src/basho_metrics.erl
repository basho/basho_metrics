%% -------------------------------------------------------------------
%%
%% Riak: A lightweight, decentralized key-value store.
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
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
-module(basho_metrics).
-export([new_histogram/1, new_histogram/2]).
-export([new_counter/1, new_counter/2]).
-export([new_meter/1, new_meter/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


new_histogram(Name) ->
    new_histogram(Name, []).

new_histogram(Name, Options) ->
    basho_metrics_sup:start_metric(histogram, Name, Options).

new_counter(Name) ->
    new_counter(Name, []).

new_counter(Name, Options) ->
    basho_metrics_sup:start_metric(counter, Name, Options).

new_meter(Name) ->
    new_meter(Name, []).

new_meter(Name, Options) ->
    basho_metrics_sup:start_metric(meter, Name, Options).
