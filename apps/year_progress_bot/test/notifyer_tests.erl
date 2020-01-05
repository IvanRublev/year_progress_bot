-module(notifyer_tests).
-include_lib("eunit/include/eunit.hrl").

should_send_progress_to_subscribed_chats_after_11_30_CET_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> 
        meck:unload(local_time),
        meck:unload(telegram)
     end,
     fun(_) -> 
        meck:new(telegram, [{stub_all, ok}]),
        meck:new(local_time, [{stub_all, {11, 30}}]),

        notifyer:evaluate_send_progress(),

        ?_assert(meck:called(telegram, send_message, '_'))
     end}.

should_Not_send_progress_to_subscribed_chats_before_11_30_CET_test_() ->
    {setup,
    fun() -> ok end,
    fun(_) ->
        meck:unload(local_time),
        meck:unload(telegram)        
    end,
    fun(_) ->
        meck:new(telegram, [{stub_all, ok}]),
        meck:new(local_time, [{stub_all, {11, 29}}]),

        notifyer:evaluate_send_progress(),

        ?_assertMatch(false, meck:called(telegram, send_message, '_'))
    end}.

