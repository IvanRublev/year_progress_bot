-module(date).
-export([time/1, now/0]).

time(_) ->
    {0, 0}.

now() ->
    {{0, 0, 0}, {0, 0, 0}}.
