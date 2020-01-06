-module(notifyer_tests).
-include_lib("eunit/include/eunit.hrl").

%-------------------------
progress_send_time_test_() ->
    {foreach,
     fun() -> 
        meck:new(telegram, [{stub_all, ok}]),
        meck:new(db),
        meck:expect(db, unnotified_chats, [1], meck:seq([[1], []]))
     end,
     fun(_) -> 
        meck:unload(local_time),
        meck:unload(telegram),
        meck:unload(db)
     end,
     [fun should_send_progress_to_subscribed_chats_after_11_30_CET/1,
      fun should_Not_send_progress_to_subscribed_chats_before_11_30_CET/1]}.

should_send_progress_to_subscribed_chats_after_11_30_CET(_) ->
        meck:new(local_time, [{stub_all, {11, 30}}]),

        notifyer:evaluate_send_progress({1, 1}),

        ?_assert(meck:called(telegram, send_message, '_')).

should_Not_send_progress_to_subscribed_chats_before_11_30_CET(_) ->
        meck:new(local_time, [{stub_all, {11, 29}}]),

        notifyer:evaluate_send_progress({1, 1}),

        ?_assertMatch(false, meck:called(telegram, send_message, '_')).

%-------------------------
send_in_batches_test_() ->
    {foreach,
    fun() ->
        meck:new(telegram, [{stub_all, ok}]),
        meck:new(local_time, [{stub_all, {11, 30}}]),
        meck:new(db),
        meck:expect(db, unnotified_chats, [25], meck:seq([[X || X <- lists:seq(1,25)], [26, 27], []])),
        meck:new(util),
        meck:expect(util, pause, fun(_) -> ok end)
    end,
    fun(_) ->
        meck:unload(util),
        meck:unload(db),
        meck:unload(local_time),
        meck:unload(telegram)
    end,
    [fun should_send_progress_in_batches_of_25_per_1000ms/1,
     fun should_request_db_for_25_chat_ids/1,
     fun should_pause_after_each_message_for_40ms/1]}.

should_send_progress_in_batches_of_25_per_1000ms(_) ->
    notifyer:evaluate_send_progress({25, 1000}),

    ?_assert(meck:called(telegram, send_message, '_')).

should_request_db_for_25_chat_ids(_) ->
    notifyer:evaluate_send_progress({25, 1000}),

    ?_assertEqual(3, meck:num_calls(db, unnotified_chats, [25])).

should_pause_after_each_message_for_40ms(_) ->
    notifyer:evaluate_send_progress({25, 1000}),

    ?_assertEqual(27, meck:num_calls(util, pause, [40.0])).
