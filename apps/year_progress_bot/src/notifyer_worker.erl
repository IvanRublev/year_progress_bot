-module(notifyer_worker).
-export([loop/1]).

loop(Period) ->
    receive after Period ->
        notifyer:evaluate_send_progress({25, 1000}),
        loop(Period)
    end.
