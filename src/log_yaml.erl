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
%%% Created : 31 Mar 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(log_yaml).

%% API
-export([validator/0]).
%% Imported validators
-import(yval, [options/2, enum/1, pos_int/1, non_neg_int/0,
               and_then/2, directory/1, string/0, bool/0,
               pos_int/0, either/2, timeout/1, atom/0, list/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec validator() -> yval:validator().
validator() ->
    options(
      #{level => enum([emergency, alert, critical, error,
                       warning, notice, info, debug, none]),
        rotate_size => pos_int(infinity),
        rotate_count => non_neg_int(),
        file_filter => enum([single, separate]),
        single_line => bool(),
        max_line_size => pos_int(unlimited),
        filesync_repeat_interval => either(no_repeat, timeout(millisecond)),
        sync_mode_qlen => non_neg_int(),
        drop_mode_qlen => pos_int(),
        flush_qlen => pos_int(),
        console => bool(),
        formatter => enum([plain, json]),
        exclude_meta => list(atom()),
        print_gun_shutdown_errors => bool(),
        dir => and_then(directory(write), string())},
      [unique, {defaults, log:defaults()}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
