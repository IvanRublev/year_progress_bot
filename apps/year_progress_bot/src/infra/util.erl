-module(util).
-export([pause/1]).

pause(T) ->
    timer:sleep(round(T)).

%---------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

should_stop_process_for_given_milliseconds_test() ->
    {MicroSec, _} = timer:tc(util, pause, [100.0]),
    Err = 10000,
    ?assert(MicroSec >= (100000-Err)),
    ?assert(MicroSec =< (100000+Err)).

-endif.
