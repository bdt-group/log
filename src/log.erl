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
-module(log).

-behaviour(gen_server).

%% Public API
-export([start/0]).
-export([stop/0]).
-export([set_level/1]).
-export([add_log_meta/2]).
-export([erase_log_meta/1]).
%% Internal API
-export([reload/3]).
-export([start_link/0]).
-export([progress_filter/2]).
-export([defaults/0]).
%% Getters for environment variables
-export([get_env_bool/1]).
-export([get_env_atom/1]).
-export([get_env_pos_int/2]).
-export([get_env_non_neg_int/1]).
-export([get_env_non_empty_string/1]).
-export([get_env_list/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {}).
-type state() :: #state{}.
-type option() :: level | rotate_size | rotate_count | single_line |
                  max_line_size | filesync_repeat_interval | dir |
                  sync_mode_qlen | drop_mode_qlen | flush_qlen | console |
                  formatter | exclude_meta | print_gun_shutdown_errors.

-type meta_key() :: atom().
-type meta_value() :: atom() | binary() | string() | number() |
                      #{meta_value() := meta_value()} | [meta_value()].

-define(LogLevels, [error, warning, notice, info, debug]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

-spec reload(Changed :: [{atom(), term()}],
             New :: [{atom(), term()}],
             Removed :: [atom()]) -> ok.
reload(Changed, New, Removed) ->
    case lists:keyfind(level, 1, Changed) of
        {_, Level} -> set_level(Level);
        false ->
            case lists:keyfind(level, 1, New) of
                {_, Level} -> set_level(Level);
                false ->
                    case lists:member(level, Removed) of
                        true -> set_level(default(level));
                        false -> ok
                    end
            end
    end.

-spec set_level(logger:level()) -> ok.
set_level(Level) when Level == emergency orelse
                      Level == alert orelse
                      Level == critical orelse
                      Level == error orelse
                      Level == warning orelse
                      Level == notice orelse
                      Level == info orelse
                      Level == debug orelse
                      Level == none ->
    case current_level() of
        Level -> ok;
        PrevLevel ->
            ?LOG_NOTICE("Changing loglevel from '~s' to '~s'",
                        [PrevLevel, Level]),
            _ = logger:set_primary_config(level, Level),
            ok
    end.

-spec defaults() -> #{option() => term()}.
defaults() ->
    #{level => notice,
      rotate_size => 1024*1024*10,
      rotate_count => 5,
      logging_mode => debug_and_error,
      single_line => false,
      max_line_size => 1024*100,
      filesync_repeat_interval => no_repeat,
      sync_mode_qlen => 1000,
      drop_mode_qlen => 1000,
      flush_qlen => 5000,
      console => false,
      formatter => plain,
      print_gun_shutdown_errors => false,
      exclude_meta => [domain, report_cb, gl, error_logger, logger_formatter],
      dir => case file:get_cwd() of
                 {ok, Path} -> Path;
                 {error, _} -> "."
             end}.

-spec add_log_meta(meta_key(), meta_value()) -> ok.
add_log_meta(Key, Value) ->
    LogMeta = get_meta(),
    logger:set_process_metadata(LogMeta#{Key => Value}).

-spec erase_log_meta(meta_key() | [meta_key()]) -> ok.
erase_log_meta(Keys) when is_list(Keys) ->
    LogMeta = get_meta(),
    logger:set_process_metadata(maps:without(Keys, LogMeta));
erase_log_meta(Key) ->
    LogMeta = get_meta(),
    logger:set_process_metadata(maps:without([Key], LogMeta)).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, state()} | {stop, term()}.
init([]) ->
    process_flag(trap_exit, true),
    case load() of
        ok -> {ok, #state{}};
        {error, Reason} -> {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, state()) -> {noreply, state()}.
handle_call(Msg, {Pid, _}, State) ->
    ?LOG_WARNING("Unexpected call from ~p: ~p", [Pid, Msg]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?LOG_WARNING("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Msg, State) ->
    ?LOG_WARNING("Unexpected info: ~p", [Msg]),
    {noreply, State}.

-spec terminate(term(), state()) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term() | {down, term()}, state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load() ->
    Level = get_level_from_env(),
    Config = config(),
    %% We always log plain text in console backend
    try
        ok = logger:set_primary_config(level, Level),
        case logger:add_primary_filter(progress_report, {fun ?MODULE:progress_filter/2, stop}) of
            ok -> ok;
            {error, {already_exist, _}} -> ok
        end,
        case Level of
            none -> ok;
            _ ->
                case get_env_atom(logging_mode) of
                    single ->
                        Filters = [{single, {fun logger_filters:level/2, {log, gteq, Level}}}],
                        add_handler(Level, Config, Filters);
                    separate ->
                        [add_handler(FileLogLevel, Config) || FileLogLevel <- get_level_list(Level)];
                    debug_and_error ->
                        [add_handler(FileLogLevel, Config) || FileLogLevel <- [error, debug]]
                end
        end,
        case get_env_bool(print_gun_shutdown_errors) of
            true -> ok;
            false -> enable_gun_filters(Level)
        end,
        %% Get rid of the default handler
        case logger:remove_handler(default) of
            ok -> ok;
            {error, {not_found, _}} -> ok
        end,
        case get_env_bool(console) andalso add_handler(console, Config) of
            false -> ok;
            ok -> ok;
            {error, {already_exist, _}} -> ok
        end
    catch _:{Tag, Err} when Tag == badmatch; Tag == case_clause ->
            ?LOG_CRITICAL("Failed to set logging: ~p", [Err]),
            Err
    end.

add_handler(none, _Config) ->
    ok;
add_handler(Level0, Config) ->
    add_handler(Level0, Config, []).
add_handler(Level0, Config, Filters) ->
    Dir = get_env_non_empty_string(dir),
    Formatter = get_formatter_from_env(),
    FormatterMod = formatter_module(Formatter),
    FileFmtConfig = FormatterMod:formatter_config(),
    {Log, Level} =
        case Level0 of
            console -> {standard_io, all};
            debug -> {filename:join(Dir, "all.log"), Level0};
            _ -> {filename:join(Dir, [Level0, ".log"]), Level0}
        end,
    HandlerConfig = #{
        level => Level,
        config => Config#{file => Log},
        formatter => {FormatterMod, FileFmtConfig},
        filters => Filters
    },
    case logger:add_handler(get_handler_id(Level), logger_std_h, HandlerConfig) of
        ok -> ok;
        {error, {already_exist, _}} -> ok
    end.

get_level_list(Level) -> get_level_list(?LogLevels, Level, []).
get_level_list([Level|_], Level, Acc) -> lists:reverse([Level|Acc]);
get_level_list([L|Levels], Level, Acc) -> get_level_list(Levels, Level, [L|Acc]).

-spec progress_filter(logger:log_event(), _) -> logger:log_event() | stop.
progress_filter(#{level := info,
                  msg := {report, #{label := {_, progress}}}} = Event, _) ->
    case current_level() of
        debug ->
            logger_filters:progress(Event#{level => debug}, log);
        _ ->
            stop
    end;
progress_filter(Event, _) ->
    Event.

config() ->
    #{max_no_bytes => get_env_pos_int(rotate_size, infinity),
      max_no_files => get_env_non_neg_int(rotate_count),
      filesync_repeat_interval => get_env_pos_int(filesync_repeat_interval, no_repeat),
      sync_mode_qlen => get_env_non_neg_int(sync_mode_qlen),
      drop_mode_qlen => get_env_pos_int(drop_mode_qlen),
      flush_qlen => get_env_pos_int(flush_qlen)}.

-spec current_level() -> logger:level().
current_level() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

-spec default(option()) -> term().
default(Opt) ->
    maps:get(Opt, defaults()).

-spec formatter_module(plain | json) -> module().
formatter_module(plain) -> log_plain;
formatter_module(json) -> log_json.

%%%===================================================================
%%% Getters for environment variables
%%%===================================================================
-spec get_env_non_empty_string(option()) -> string().
get_env_non_empty_string(Opt) ->
    case application:get_env(?MODULE, Opt) of
        {ok, [_|_] = String} ->
            String;
        _ ->
            default(Opt)
    end.

-spec get_env_list(option()) -> list().
get_env_list(Opt) ->
    case application:get_env(?MODULE, Opt) of
        {ok, L} when is_list(L) ->
            L;
        _ ->
            default(Opt)
    end.

-spec get_env_non_neg_int(option()) -> non_neg_integer().
get_env_non_neg_int(Opt) ->
    case application:get_env(?MODULE, Opt) of
        {ok, Val} when is_integer(Val) andalso Val >= 0 ->
            Val;
        _ ->
            default(Opt)
    end.

-spec get_env_pos_int(option()) -> pos_integer().
get_env_pos_int(Opt) ->
    case application:get_env(?MODULE, Opt) of
        {ok, Val} when is_integer(Val) andalso Val > 0 ->
            Val;
        _ ->
            default(Opt)
    end.

-spec get_env_pos_int(option(), T) -> pos_integer() | T.
get_env_pos_int(Opt, Other) ->
    case application:get_env(?MODULE, Opt) of
        {ok, Val} when (is_integer(Val) andalso Val > 0)
                       orelse Val == Other ->
            Val;
        _ ->
            default(Opt)
    end.

-spec get_env_bool(option()) -> boolean().
get_env_bool(Opt) ->
    case application:get_env(?MODULE, Opt) of
        {ok, true} -> true;
        _ -> default(Opt)
    end.

-spec get_env_atom(option()) -> boolean().
get_env_atom(Opt) ->
    case application:get_env(?MODULE, Opt) of
        {ok, A} when is_atom(A) -> A;
        _ -> default(Opt)
    end.

-spec get_formatter_from_env() -> module().
get_formatter_from_env() ->
    case application:get_env(?MODULE, formatter) of
        {ok, plain} -> plain;
        {ok, json} -> json;
        _ -> default(formatter)
    end.

-spec get_level_from_env() -> logger:level().
get_level_from_env() ->
    case application:get_env(?MODULE, level) of
        {ok, L} when L == emergency orelse
                     L == alert orelse
                     L == critical orelse
                     L == error orelse
                     L == warning orelse
                     L == notice orelse
                     L == info orelse
                     L == debug orelse
                     L == none ->
            L;
        _ ->
            default(level)
    end.

get_meta() ->
    case logger:get_process_metadata() of
        undefined -> #{};
        M when is_map(M) -> M
    end.

enable_gun_filters(Level) ->
    case logger:add_handler_filter(get_handler_id(Level), gun_error_filter, {fun log_gun:filter_supervisor_reports/2, #{}}) of
        ok -> ok;
        {error, {already_exist, _}} -> ok
    end.

get_handler_id(Level) ->
    list_to_atom(atom_to_list(Level) ++ "_handler").