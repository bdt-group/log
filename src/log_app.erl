%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2021, Big Data Technology. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @doc
%%% @end
%%% Created : 29 Mar 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(log_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, config_change/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
                   {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    log_sup:start_link().

-spec stop(term()) -> any().
stop(_State) ->
    ok.

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok.
config_change(Changed, New, Removed) ->
    log:reload(Changed, New, Removed).

%%%===================================================================
%%% Internal functions
%%%===================================================================
