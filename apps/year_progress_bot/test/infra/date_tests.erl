-module(date_tests).
-include_lib("eunit/include/eunit.hrl").

date_test_() ->
    {foreach,
     fun() ->
        meck:new(calendar, [unstick, passthrough])
     end,
     fun(_) ->
        meck:unload(calendar)
     end,
     [fun should_return_current_time_in_CET/1,
      fun should_return_todays_date_time_in_CET/1,
      fun should_return_1_jan_as_start_of_year/1,
      fun should_return_31_dec_as_end_of_year/1]}.

should_return_current_time_in_CET(_) ->
    {_, {H, M, _}} = localtime:utc_to_local(calendar:universal_time(), "CET"),
    ?_assertEqual({H, M}, date:time()).

should_return_todays_date_time_in_CET(_) ->
    Today = localtime:utc_to_local(calendar:universal_time(), "CET"),
    ?_assertEqual(Today, date:now()).

should_return_1_jan_as_start_of_year(_) ->
    meck:expect(calendar, universal_time, fun() -> {{2020,3,5},{15,22,35}} end),

    ?_assertEqual({2020,1,1}, date:start_of_year_date()).

should_return_31_dec_as_end_of_year(_) ->
    meck:expect(calendar, universal_time, fun() -> {{2020,3,5},{15,22,35}} end),

    ?_assertEqual({2020,12,31}, date:end_of_year_date()).
