-module(notifyer_tests).
-include_lib("eunit/include/eunit.hrl").

%-------------------------
progress_send_time_test_() ->
    {foreach,
     fun() -> 
        meck:new(telegram, [{stub_all, ok}]),
        meck:new(db),
        meck:expect(db, unnotified_chats, [1, '_'], meck:seq([[1], []])),
        meck:expect(db, mark_chats_notified, fun(_, _) -> ok end)
     end,
     fun(_) -> 
        meck:unload(date),
        meck:unload(telegram),
        meck:unload(db)
     end,
     [fun should_send_progress_to_subscribed_chats_after_11_30_CET/1,
      fun should_Not_send_progress_to_subscribed_chats_before_11_30_CET/1]}.

should_send_progress_to_subscribed_chats_after_11_30_CET(_) ->
        meck:expect(date, time, fun(_) -> {11, 30} end),

        notifyer:evaluate_send_progress({1, 1}),

        ?_assert(meck:called(telegram, send_message, '_')).

should_Not_send_progress_to_subscribed_chats_before_11_30_CET(_) ->
        meck:expect(date, time, fun(_) -> {11, 29} end),

        notifyer:evaluate_send_progress({1, 1}),

        ?_assertMatch(false, meck:called(telegram, send_message, '_')).

%-------------------------
send_in_batches_test_() ->
    {foreach,
    fun() ->
        meck:new(telegram),
        meck:expect(telegram, send_message, fun(_) -> ok end),
        meck:new(date),
        meck:expect(date, time, fun(_) -> {11, 30} end),
        meck:expect(date, now, [], meck:seq([
            {{2020, 01, 02}, {11, 31, 19}},
            {{2020, 01, 02}, {11, 32, 1}}
        ])),
        meck:new(db),
        meck:expect(db, unnotified_chats, [25, '_'], meck:seq([[X || X <- lists:seq(1,25)], [26, 27], []])),
        meck:expect(db, mark_chats_notified, fun(_, _) -> ok end),
        meck:new(util),
        meck:expect(util, pause, fun(_) -> ok end)
    end,
    fun(_) ->
        meck:unload(util),
        meck:unload(db),
        meck:unload(date),
        meck:unload(telegram)
    end,
    [fun should_send_progress_in_batches_of_25_per_1000ms/1,
     fun should_request_db_for_25_chat_ids/1,
     fun should_pause_after_each_message_for_40ms/1,
     fun should_mark_chat_ids_as_notified_in_db/1,
     fun should_mark_chat_ids_as_notified_in_db_only_on_success_send/1]}.

should_send_progress_in_batches_of_25_per_1000ms(_) ->
    notifyer:evaluate_send_progress({25, 1000}),

    ?_assert(meck:called(telegram, send_message, '_')).

should_request_db_for_25_chat_ids(_) ->
    meck:expect(date, now, [], {{2020, 01, 02}, {11, 30, 10}}),

    notifyer:evaluate_send_progress({25, 1000}),

    ?_assertEqual(3, meck:num_calls(db, unnotified_chats, [25, {{2020, 01, 02}, {11, 30, 10}}])).

should_pause_after_each_message_for_40ms(_) ->
    notifyer:evaluate_send_progress({25, 1000}),

    ?_assertEqual(27, meck:num_calls(util, pause, [40.0])).

should_mark_chat_ids_as_notified_in_db(_) ->
    notifyer:evaluate_send_progress({25, 1000}),

    ?_assert(meck:called(db, mark_chats_notified, [lists:seq(1,25), {{2020, 01, 02}, {11, 31, 19}}])),
    ?_assert(meck:called(db, mark_chats_notified, [[26, 27], {{2020, 01, 02}, {11, 32, 1}}])).

should_mark_chat_ids_as_notified_in_db_only_on_success_send(_) ->
    meck:expect(telegram, send_message, ['_'], meck:loop([ok, {error, {error_code, 500}}])),

    notifyer:evaluate_send_progress({25, 1000}),

    ?_assert(meck:called(db, mark_chats_notified, [
        lists:filter(fun(E) -> E rem 2 == 0 end, lists:seq(1,25)), 
        {{2020, 01, 02}, {11, 31, 19}}
    ])),
    ?_assert(meck:called(db, mark_chats_notified, [
        [27],
        {{2020, 01, 02}, {11, 32, 1}}
    ])).
