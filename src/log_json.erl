%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%    JSON formatter
%%% @end
%%% Created : 15 Oct 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(log_json).

%% API
-export([formatter_config/0]).
%% Erlang logger formatter callbacks
-export([check_config/1]).
-export([format/2]).
%% Exported types
-export_type([config/0]).

-type config() :: #{single_line := boolean(),
                    exclude_meta := [atom()],
                    max_size := pos_integer() | unlimited}.

%%%===================================================================
%%% API
%%%===================================================================
-spec formatter_config() -> config().
formatter_config() ->
    #{single_line => log:get_env_bool(single_line),
      exclude_meta => log:get_env_list(exclude_meta),
      max_size => log:get_env_pos_int(max_line_size, unlimited)}.

-spec check_config(_) -> ok.
check_config(_) ->
    ok.

-spec format(logger:log_event(), config()) -> iolist().
format(#{level := Level, meta := Meta, msg := Msg},
       #{exclude_meta := ExcludeKeys} = Config) ->
    JSON1 =
        maps:fold(
          fun(time, USec, Acc) ->
                  Acc#{timestamp => format_time(USec)};
             (Key, Val, Acc) when Key == file; Key == line ->
                  Acc#{Key => term_to_json(Val, Config)};
             (Key, Val, Acc) when Key == mfa; Key == pid;
                                  Key == domain; Key == report_cb;
                                  Key == gl; Key == error_logger;
                                  Key == logger_formatter ->
                  Map = maps:get(erlang, Acc, #{}),
                  Acc#{erlang => update_map(Key, Val, Map, Config)};
             (Key, Val, Acc) ->
                  Map = maps:get(meta, Acc, #{}),
                  Acc#{meta => update_map(Key, Val, Map, Config)}
          end, #{}, maps:without(ExcludeKeys, Meta)),
    JSON2 = JSON1#{severity => Level,
                   message => format_msg(Msg, Meta, Config)},
    [jiffy:encode(JSON2), $\n].

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_map(mfa, MFA, Map, _) ->
    Map#{mfa => format_mfa(MFA)};
update_map(Key, Val, Map, Config) ->
    Map#{Key => term_to_json(Val, Config)}.

format_msg({string, String}, _, Config) ->
    truncate(unicode:characters_to_binary(single_line(String, Config)), Config);
format_msg({report, Report}, #{report_cb := Fun} = Meta, Config) when is_function(Fun, 1) ->
    format_msg(Fun(Report), maps:remove(report_cb, Meta), Config);
format_msg({report, Report}, #{report_cb := Fun}, Config) when is_function(Fun, 2) ->
    Config1 = maps:without([max_size], Config),
    truncate(unicode:characters_to_binary(Fun(Report, Config1)), Config);
format_msg({Format, Args}, _, Config) ->
    MsgStr0 = io_lib:format(Format, Args),
    MsgStr = single_line(MsgStr0, Config),
    truncate(unicode:characters_to_binary(MsgStr), Config).

format_time(USec) ->
    list_to_binary(calendar:system_time_to_rfc3339(USec, [{unit, microsecond}])).

format_mfa({M, F, A}) ->
    list_to_binary([atom_to_list(M), $:, atom_to_list(F), $/, integer_to_list(A)]).

format_term(Term, Config) ->
    S = single_line(io_lib:format("~tp", [Term]), Config),
    try unicode:characters_to_binary(S) of
        B when is_binary(B) -> B;
        _ -> list_to_binary(S)
    catch _:_ ->
            list_to_binary(S)
    end.

term_to_json(N, _) when is_number(N) -> N;
term_to_json(A, _) when is_atom(A) -> A;
term_to_json(B, _) when is_binary(B) -> B;
term_to_json(P, _) when is_pid(P) ->
    list_to_binary(erlang:pid_to_list(P));
term_to_json(M, Config) when is_map(M) ->
    try maps:map(
          fun(Key, Val) when is_binary(Key) orelse is_atom(Key) ->
                  term_to_json(Val, Config)
          end, M)
    catch _:_ ->
            format_term(M, Config)
    end;
term_to_json(L, Config) when is_list(L) ->
    try unicode:characters_to_binary(L) of
        B when is_binary(B) -> B;
        _ -> list_to_binary(L)
    catch _:_ ->
            list_to_json(L, Config)
    end;
term_to_json(Term, Config) ->
    format_term(Term, Config).

list_to_json(L, Config) ->
    try [term_to_json(E, Config) || E <- L]
    catch _:_ -> format_term(L, Config)
    end.

%% Trim leading and trailing whitespaces, and replace newlines with ", "
%% Copied from logger_formatter.erl
single_line(S, #{single_line := true}) ->
    T = lists:reverse(
          trim(
            lists:reverse(
              trim(S, false)), true)),
    re:replace(T, ",?\r?\n\s*", ", ",
               [{return, list}, global, unicode]);
single_line(S, _) ->
    S.

%% Copied from logger_formatter.erl
trim([H|T], Rev) when H==$\s; H==$\r; H==$\n ->
    trim(T, Rev);
trim([H|T], false) when is_list(H) ->
    case trim(H, false) of
        [] ->
            trim(T, false);
        TrimmedH ->
            [TrimmedH|T]
    end;
trim([H|T], true) when is_list(H) ->
    case trim(lists:reverse(H), true) of
        [] ->
            trim(T, true);
        TrimmedH ->
            [lists:reverse(TrimmedH)|T]
    end;
trim(String, _) ->
    String.

-spec truncate(_, map()) -> binary().
truncate(S, #{max_size := Max}) when is_binary(S), is_integer(Max), size(S) > Max ->
    H = binary:part(S, {0, Max}),
    <<H/binary, "...">>;
truncate(S, _) ->
    S.
