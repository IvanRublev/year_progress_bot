-module(notifyer).
-export([evaluate_send_progress/1]).
-compile([{parse_transform, lager_transform}]).

evaluate_send_progress(BatchSpec) ->
    {H, M} = date:time(),
    if 
        (H >= 11) and (M >= 30) -> send_progress(BatchSpec, date:now(), #{});
        true -> ok
    end.

send_progress({BatchSize, BatchTime} = BatchSpec, CurrentDate, Tries) ->
    Pause = BatchTime / BatchSize,
    Unnotified = db:unnotified_chats(BatchSize, CurrentDate) -- triple_failed_ids(Tries),
    case Unnotified of
        [] -> ok;
        List -> 
            {SuccIds, FailIds} = filter_success([{send_message(Id, CurrentDate, Pause), Id} || Id <- List]),
            lager:info("Notifications on ~p: succeeded ~p, failed ~p.", [CurrentDate, length(SuccIds), length(FailIds)]),
            db:mark_chats_notified(SuccIds, date:end_of_today()),
            send_progress(BatchSpec, CurrentDate, merge_fail_count(FailIds, Tries))
    end.

send_message(Id, ProgressDate, Pause) ->
    R = telegram:send_message(Id, ProgressDate),
    lager:debug("Send pb message to chat_id: ~p, result: ~p", [Id, R]),
    util:pause(Pause),
    R.

triple_failed_ids(IdCount) ->
    maps:keys(maps:filter(fun(_, Count) -> Count >= 3 end, IdCount)).

merge_fail_count(FailIds, IdCount) ->
    lists:foldl(fun(Id, UpdIdCount) ->
        maps:update_with(Id, fun(Count) -> Count+1 end, 1, UpdIdCount)
    end, IdCount, FailIds).

filter_success(SentList) ->
    {Succ, Fail} = lists:foldl(fun({Status, Id}, {Succ, Fail}) ->
        case Status of
            ok -> {[Id | Succ], Fail};
            _ -> {Succ, [Id | Fail]}
        end
    end, {[], []}, SentList),
    {lists:reverse(Succ), lists:reverse(Fail)}.