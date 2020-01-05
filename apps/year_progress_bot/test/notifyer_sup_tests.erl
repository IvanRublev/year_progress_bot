-module(notifyer_sup_tests).
-include_lib("eunit/include/eunit.hrl").

should_evaluate_send_of_progress_periodically_test() ->
    meck:new(notifyer, [{stub_all, ok}]),

    Pid = spawn(notifyer_sup, loop, [1]),
    receive after 10 -> ok end,
    exit(Pid, exit),

    ?assert(meck:num_calls(notifyer, evaluate_send_progress, '_') >= 3),
    meck:unload(notifyer).
