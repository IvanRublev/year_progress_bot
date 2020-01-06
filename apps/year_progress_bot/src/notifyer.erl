-module(notifyer).
-export([evaluate_send_progress/1]).

evaluate_send_progress(BatchSpec) ->
    {H, M} = local_time:time("CET"),
    if 
        (H >= 11) and (M >= 30) -> send_progress(BatchSpec);
        true -> ok
    end.

send_progress({BatchSize, BatchTime} = BatchSpec) ->
    Pause = BatchTime / BatchSize,
    case db:unnotified_chats(BatchSize) of
        [] -> ok;
        List -> 
            [send_message(Id, Pause) || Id <- List],
            send_progress(BatchSpec)
    end.

send_message(Id, Pause) ->
    telegram:send_message(Id),
    util:pause(Pause).
