-module(notifyer_sup).
-export([loop/1]).

loop(Period) ->
    receive after Period ->
        notifyer:evaluate_send_progress(),
        loop(Period)
    end.
