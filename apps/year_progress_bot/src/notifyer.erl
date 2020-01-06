-module(notifyer).
-export([evaluate_send_progress/1]).

evaluate_send_progress(BatchSpec) ->
    {H, M} = local_time:time("CET"),
    if 
        (H >= 11) and (M >= 30) -> send_progress();
        true -> ok
    end.

send_progress() ->
    case db:unnotified_chats(25) of
        [] -> ok;
        List -> 
            [telegram:send_message(Id) || Id <- List],
            send_progress()
    end.
