-module(notifyer_worker_tests).
-include_lib("eunit/include/eunit.hrl").

periodic_send_test_() ->
    {foreach,
    fun() -> 
        meck:new(notifyer, [{stub_all, ok}])
    end,
    fun(_) -> 
        meck:unload(notifyer)
    end,
    [fun should_evaluate_send_of_progress_periodically/1,
     fun should_limit_send_for_25_messages_per_second/1,
     fun should_return_child_spec/1]}.

should_evaluate_send_of_progress_periodically(_) ->
    Pid = spawn(notifyer_worker, loop, [1]),
    receive after 10 -> ok end,
    exit(Pid, exit),

    ?_assert(meck:num_calls(notifyer, evaluate_send_progress, '_') >= 3).

should_limit_send_for_25_messages_per_second(_) ->
    Pid = spawn(notifyer_worker, loop, [1]),
    receive after 10 -> ok end,
    exit(Pid, exit),

    ?_assert(meck:called(notifyer, evaluate_send_progress, [{25, 1000}])).

should_return_child_spec(_) ->
    Exp = #{id => notifyer_worker, 
            restart => permanent,
            start => {notifyer_worker, start_link, []}},
    ?_assertMatch(Exp, notifyer_worker:child_spec()).
