-module(log_gun).

-export([filter_supervisor_reports/2]).

-spec filter_supervisor_reports(logger:log_event(), any()) -> logger:filter_return().
filter_supervisor_reports(#{msg := {report, Report}} = Event, _) when is_list(Report) ->
    case lists:sort(Report) of
        [{errorContext, shutdown_error}, {offender, _}, {reason, Reason}, {supervisor, _}] when
                Reason == noproc orelse Reason == {shutdown, normal} ->
            stop;
        _ ->
            Event
    end;
filter_supervisor_reports(#{msg := {report, Report}} = Event, _) when is_map(Report)->
    case Report of
        #{reason := Reason, errorContext := shutdown_error} when
                Reason == noproc
                orelse Reason == {shutdown, normal} ->
            stop;
        _ ->
            Event
    end;
filter_supervisor_reports(Event, _) ->
    Event.