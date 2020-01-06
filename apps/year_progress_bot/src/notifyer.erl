-module(notifyer).
-export([evaluate_send_progress/1]).

evaluate_send_progress(BatchSpec) ->
    {H, M} = date:time(),
    if 
        (H >= 11) and (M >= 30) -> send_progress(BatchSpec, date:now());
        true -> ok
    end.

send_progress({BatchSize, BatchTime} = BatchSpec, CurrentDate) ->
    Pause = BatchTime / BatchSize,
    case db:unnotified_chats(BatchSize, CurrentDate) of
        [] -> ok;
        List -> 
            SuccIds = filter_success([{send_message(Id, CurrentDate, Pause), Id} || Id <- List]),
            db:mark_chats_notified(SuccIds, date:now()),
            send_progress(BatchSpec, CurrentDate)
    end.

send_message(Id, ProgressDate, Pause) ->
    R = telegram:send_message(Id, ProgressDate),
    util:pause(Pause),
    R.

filter_success(SentList) ->
    lists:filtermap(fun({Status, Id}) -> 
        case Status of 
            ok -> {true, Id};
            _ -> false 
        end 
    end, SentList).
