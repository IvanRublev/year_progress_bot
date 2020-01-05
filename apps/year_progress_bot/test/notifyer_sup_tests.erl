-module(notifyer_sup_tests).
-include_lib("eunit/include/eunit.hrl").

should_evaluate_send_of_progress_periodically_test() ->
    meck:new(notifyer),
    meck:expect(notifyer, evaluate_send_progress, fun() -> ok end),

    Pid = spawn(notifyer_sup, loop, [1]),
    receive after 6 -> ok end,
    exit(Pid, exit),

    ?assertEqual(meck:num_calls(notifyer, evaluate_send_progress, []), 3),
    meck:unload(notifyer).
