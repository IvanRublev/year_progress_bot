-module(date).
-export([time/0, now/0]).

time() -> 
    {_, {H, M, _}} = ?MODULE:now(),
    {H, M}.

now() ->
    localtime:utc_to_local(calendar:universal_time(), "CET").

start_of_year_date() ->
    {0,0,0}.

today_date() ->
    {0,0,0}.