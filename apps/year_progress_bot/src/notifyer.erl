-module(notifyer).
-export([evaluate_send_progress/0]).

evaluate_send_progress() ->
    {H, M} = local_time:time("CET"),
    if 
        (H >= 11) and (M >= 30) -> telegram:send_message();
        true -> ok
    end.
