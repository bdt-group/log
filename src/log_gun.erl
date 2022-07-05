-module(log_gun).

-export([filter_supervisor_reports/2]).

-spec filter_supervisor_reports(logger:log_event(), any()) -> logger:filter_return().
filter_supervisor_reports(#{msg := {report, Report}} = Event, _) ->
    case Report of
        #{reason := Reason, errorContext := shutdown_error} when
                Reason == norpoc
                orelse Reason == {shutdown, normal} ->
            ignore;
        _ ->
            Event
    end.