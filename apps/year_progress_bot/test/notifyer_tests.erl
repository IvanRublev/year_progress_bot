-module(notifyer_tests).
-include_lib("eunit/include/eunit.hrl").

should_send_progress_to_subscribed_chats_after_11_30_CET_test() ->
    meck:new(telegram, [{stub_all, ok}]),
    meck:new(local_time, [{stub_all, {11, 30}}]),

    notifyer:evaluate_send_progress(),

    ?assert(meck:called(telegram, send_message, '_')),
    meck:unload(local_time),
    meck:unload(telegram).

should_Not_send_progress_to_subscribed_chats_before_11_30_CET_test() ->
    meck:new(telegram, [{stub_all, ok}]),
    meck:new(local_time, [{stub_all, {11, 29}}]),

    notifyer:evaluate_send_progress(),

    ?assertMatch(false, meck:called(telegram, send_message, '_')),
    meck:unload(local_time),
    meck:unload(telegram).
