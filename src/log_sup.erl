%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(log_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 1},
    Specs = [#{id => log,
               start => {log, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [log]}],
    {ok, {SupFlags, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
