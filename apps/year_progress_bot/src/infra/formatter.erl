-module(formatter).
-export([year_progress_bar/1]).

    % StartDays = calendar:date_to_gregorian_days(date:start_of_year_date()),
    % TodayDays = calendar:date_to_gregorian_days(date:today_date()),
    % DayOfYear = TodayDays-StartDays.

year_progress_bar(_Date) ->    
    Percent = 0,
    io_lib:format("~15c ~B%", [$░, Percent]).

%---------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

should_have_empty_bar_for_1_jan_test() ->
    P = year_progress_bar({{2020,1,1}, {20,30}}),

    ?assertEqual(binary_to_list(<<"░░░░░░░░░░░░░░░ 0%">>), string:left(P, 18)).

-endif.
