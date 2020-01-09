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
     [fun should_have_0_blocks_0_percent_bar_for_2_jan/1,
      fun should_have_0_blocks_1_percent_bar_for_5_jan/1,
      fun should_have_1_block_6_percent_bar_for_25_jan/1,
      fun should_have_15_block_96_percent_bar_for_20_dec/1,
      fun should_have_15_block_99_percent_bar_for_31_dec/1,
      fun should_have_full_bar_of_2020_and_zero_bar_of_2021_for_1_jan_2021/1,
      fun should_have_year_number_in_message/1]}.

should_have_0_blocks_0_percent_bar_for_2_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,2}, {0,0}}),
    ?_assertEqual(binary_to_list(<<"░░░░░░░░░░░░░░░ 0%">>), string:left(P, 18)).

should_have_0_blocks_1_percent_bar_for_5_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,5}, {20,30}}),
    ?_assertEqual(binary_to_list(<<"░░░░░░░░░░░░░░░ 1%">>), string:left(P, 18)).

should_have_1_block_6_percent_bar_for_25_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,25}, {0,0}}),
    ?_assertEqual(binary_to_list(<<"▓░░░░░░░░░░░░░░ 6%">>), string:left(P, 18)).

should_have_15_block_96_percent_bar_for_20_dec(_) ->
    P = formatter:year_progress_bar({{2020,12,20}, {0,0}}),
    ?_assertEqual(binary_to_list(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 96%">>), string:left(P, 19)).

should_have_15_block_99_percent_bar_for_31_dec(_) ->
    P = formatter:year_progress_bar({{2020,12,31}, {23,59}}),
    ?_assertEqual(binary_to_list(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 99%">>), string:left(P, 19)).

should_have_full_bar_of_2020_and_zero_bar_of_2021_for_1_jan_2021(_) ->
    meck:expect(date, start_of_year_date, fun() -> {2021, 1, 1} end),
    meck:expect(date, end_of_year_date, fun() -> {2021, 12, 31} end),

    P = formatter:year_progress_bar({{2021,1,1}, {0,0}}),
    E = binary_to_list(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 100%">>) ++ io_lib:format("~n", []) ++ binary_to_list(<<"2 0 2 0">>) ++
        io_lib:format("~n~n", []) ++
        binary_to_list(<<"░░░░░░░░░░░░░░░ 0%">>) ++ io_lib:format("~n", []) ++ binary_to_list(<<"2 0 2 1">>),
    ?_assertEqual(E, P).

should_have_year_number_in_message(_) ->
    P = formatter:year_progress_bar({{2020,12,31}, {23,59}}),
    ?_assertEqual(io_lib:format("~n", []) ++ binary_to_list(<<"2 0 2 0">>), string:right(P, 8)).
