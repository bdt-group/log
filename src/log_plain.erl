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
      time_designator => $T,
      time_offset => "",
      exclude_meta => log:get_env_list(exclude_meta),
      single_line => log:get_env_bool(single_line),
      max_size => log:get_env_pos_int(max_line_size, unlimited)}.

-spec check_config(config()) -> ok | {error, term()}.
check_config(Config) ->
    logger_formatter:check_config(formatter_config(Config)).

%% Effective template:
%%
%% timestamp [level] [pid mfa] [Meta] [file:line] Msg
%%
%% where application code business Meta:
%% #{key1 => #{key11 => Val11}, key2 => Val2}
%% is formatted by flatlog as:
%% key1_key11=Val11, key2=Val2

-spec format(logger:log_event(), config()) -> unicode:chardata().
format(#{meta := Meta} = LogEvent, #{exclude_meta := ExcludeKeys} = Config) ->
    Config1 = formatter_config(Config),
    Template1 = [time, " [", level, "] [", pid, mfa(), "] ["],
    Template2 = ["] ", source() | msg()],
    case user_meta(ExcludeKeys, Meta) of
        UserMeta when map_size(UserMeta) == 0 ->
            logger_formatter:format(LogEvent,
                                    Config1#{template => Template1 ++ Template2});
        UserMeta ->
            flatlog:format(LogEvent#{msg => {report, UserMeta}},
                           Config1#{template => Template1 ++ [msg],
                                    map_depth => 3})
                ++
                logger_formatter:format(LogEvent,
                                        Config1#{template => Template2})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
user_meta(ExcludeKeys, Meta) ->
    maps:without([time, file, line, mfa, pid, domain, report_cb, gl,
                  error_logger, logger_formatter | ExcludeKeys], Meta).

formatter_config(Config) ->
    maps:without([exclude_meta], Config).

mfa() ->
    {mfa, [" ", mfa], []}.

source() ->
    {file, ["[", file, {line, [":", line], []}, "] "], []}.

msg() ->
    [{logger_formatter,
      [[logger_formatter, title], ":", io_lib:nl()], []},
     msg, io_lib:nl()].
