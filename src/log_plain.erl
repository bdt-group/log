%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%    Plain text formatter
%%% @end
%%% Created : 15 Oct 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(log_plain).

%% API
-export([formatter_config/0]).
%% Erlang logger formatter callbacks
-export([check_config/1]).
-export([format/2]).
-export_type([config/0]).

-type config() :: map(). %% logger_formatter:config()

%%%===================================================================
%%% API
%%%===================================================================
-spec formatter_config() -> config().
formatter_config() ->
    #{legacy_header => false,
      time_designator => $ ,
      template => formatter_template(),
      single_line => log:get_env_bool(single_line),
      max_size => log:get_env_pos_int(max_line_size, unlimited)}.

-spec check_config(config()) -> ok | {error, term()}.
check_config(Config) ->
    logger_formatter:check_config(Config).

-spec format(logger:log_event(), config()) -> unicode:chardata().
format(LogEvent, Config) ->
    logger_formatter:format(LogEvent, Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
formatter_template() ->
     [time, " [", level, "] ", pid, mfa(), " " | msg()].

mfa() ->
    {mfa, ["@", mfa, {line, [":", line], []}], []}.

msg() ->
    [{logger_formatter,
      [[logger_formatter, title], ":", io_lib:nl()], []},
     msg, io_lib:nl()].