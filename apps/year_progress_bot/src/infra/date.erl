-module(date).
-export([time/0, now/0, start_of_year_date/0, end_of_year_date/0]).

time() -> 
    {_, {H, M, _}} = ?MODULE:now(),
    {H, M}.

now() ->
    localtime:utc_to_local(calendar:universal_time(), "CET").

start_of_year_date() ->
    {{Y, _, _}, _} = ?MODULE:now(),
    {Y, 1, 1}.

end_of_year_date() ->
    {{Y, _, _}, _} = ?MODULE:now(),
    {Y, 12, 31}.
