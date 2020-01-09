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
      fun should_have_0_blocks_1_percent_bar_for_5_jan/1,
      fun should_have_1_block_6_percent_bar_for_25_jan/1,
      fun should_have_15_block_99_percent_bar_for_31_dec/1,
      fun should_have_15_block_100_percent_bar_for_1_jan_2021/1]}.

should_have_empty_bar_for_1_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,1}, {20,30}}),
    ?_assertEqual(binary_to_list(<<"░░░░░░░░░░░░░░░ 0%">>), string:left(P, 18)).

should_have_0_blocks_1_percent_bar_for_5_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,5}, {20,30}}),
    ?_assertEqual(binary_to_list(<<"░░░░░░░░░░░░░░░ 1%">>), string:left(P, 18)).

should_have_1_block_6_percent_bar_for_25_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,25}, {0,0}}),
    ?_assertEqual(binary_to_list(<<"▓░░░░░░░░░░░░░░ 6%">>), string:left(P, 18)).

should_have_15_block_97_percent_bar_for_19_dec(_) ->
    P = formatter:year_progress_bar({{2020,12,19}, {0,0}}),
    ?_assertEqual(binary_to_list(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 97%">>), string:left(P, 19)).

should_have_15_block_99_percent_bar_for_31_dec(_) ->
    P = formatter:year_progress_bar({{2020,12,31}, {23,59}}),
    ?_assertEqual(binary_to_list(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 99%">>), string:left(P, 19)).

should_have_15_block_100_percent_bar_for_1_jan_2021(_) ->
    P = formatter:year_progress_bar({{2021,1,1}, {0,0}}),
    ?_assertEqual(binary_to_list(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 100%">>), string:left(P, 20)).

