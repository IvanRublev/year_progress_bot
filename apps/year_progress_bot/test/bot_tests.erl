-module(bot_tests).
-include_lib("eunit/include/eunit.hrl").

reply_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     fun(_) ->
         ok
     end,
     []}.
