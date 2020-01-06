-module(date_tests).
-include_lib("eunit/include/eunit.hrl").

should_return_current_time_in_CET_test() ->
    {{_, _, _}, {H, M, _}} = localtime:utc_to_local(calendar:universal_time(), "CET"),
    ?assertEqual({H, M}, date:time()).

should_return_todays_date_time_in_CET_test() ->
    Today = localtime:utc_to_local(calendar:universal_time(), "CET"),
    ?assertEqual(Today, date:now()).
