-module(notifyer).
-export([evaluate_send_progress/1]).

evaluate_send_progress(BatchSpec) ->
    {H, M} = date:time("CET"),
    if 
        (H >= 11) and (M >= 30) -> send_progress(BatchSpec);
        true -> ok
    end.

send_progress({BatchSize, BatchTime} = BatchSpec) ->
    Pause = BatchTime / BatchSize,
    case db:unnotified_chats(BatchSize, date:now()) of
        [] -> ok;
        List -> 
            SuccIds = filter_success([{send_message(Id, Pause), Id} || Id <- List]),
            db:mark_chats_notified(SuccIds, date:now()),
            send_progress(BatchSpec)
    end.

send_message(Id, Pause) ->
    R = telegram:send_message(Id),
    util:pause(Pause),
    R.

filter_success(SentList) ->
    lists:filtermap(fun({Status, Id}) -> 
        case Status of 
            ok -> {true, Id};
            _ -> false 
        end 
    end, SentList).
