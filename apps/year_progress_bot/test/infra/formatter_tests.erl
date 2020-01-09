-module(formatter_tests).
-include_lib("eunit/include/eunit.hrl").

formatter_test_() ->
    {foreach,
     fun() ->
        meck:new(date, [passthrough]),
        meck:expect(date, start_of_year_date, fun() -> {2020, 1, 1} end),
        meck:expect(date, end_of_year_date, fun() -> {2020, 12, 31} end)
     end,
     fun(_) ->
        meck:unload(date)
     end,
     [fun should_have_empty_bar_for_1_jan/1,
      fun should_have_0_blocks_1_percent_bar_for_3_jan/1,
      fun should_have_1_block_7percent_bar_for_24_jan/1]}.

should_have_empty_bar_for_1_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,1}, {20,30}}),
    ?_assertEqual(binary_to_list(<<"░░░░░░░░░░░░░░░ 0%">>), string:left(P, 18)).

should_have_0_blocks_1_percent_bar_for_3_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,3}, {20,30}}),
    ?_assertEqual(binary_to_list(<<"░░░░░░░░░░░░░░░ 1%">>), string:left(P, 18)).

should_have_1_block_7percent_bar_for_24_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,25}, {0,0}}),
    ?_assertEqual(binary_to_list(<<"▓░░░░░░░░░░░░░░ 7%">>), string:left(P, 18)).
