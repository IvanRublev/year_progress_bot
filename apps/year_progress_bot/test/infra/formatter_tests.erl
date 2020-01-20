-module(formatter_tests).
-include_lib("eunit/include/eunit.hrl").

year_bar_test_() ->
    {foreach,
     fun() ->
        meck:new(date, [passthrough]),
        meck:expect(date, start_of_year_date, fun() -> {2020, 1, 1} end),
        meck:expect(date, end_of_year_date, fun() -> {2020, 12, 31} end)
     end,
     fun(_) ->
        meck:unload(date)
     end,
     [fun should_add_spaces_after_bar_to_push_year_on_next_line_on_macos/1,
      fun should_have_0_blocks_0_percent_bar_for_2_jan/1,
      fun should_have_0_blocks_1_percent_bar_for_5_jan/1,
      fun should_have_1_block_6_percent_bar_for_25_jan/1,
      fun should_have_15_block_96_percent_bar_for_20_dec/1,
      fun should_have_15_block_99_percent_bar_for_31_dec/1,
      fun should_have_year_number_in_message/1,
      fun should_have_full_bar_of_2020_and_zero_bar_of_2021_for_1_jan_2021/1]}.

should_add_spaces_after_bar_to_push_year_on_next_line_on_macos(_) ->
    P = formatter:year_progress_bar({{2020,1,2}, {0,0}}),
    ?_assertEqual(<<"░░░░░░░░░░░░░░░ 0%      \n"/utf8>>, binary:part(P, 0, 55)).

should_have_0_blocks_0_percent_bar_for_2_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,2}, {0,0}}),
    ?_assertEqual(<<"░░░░░░░░░░░░░░░ 0%"/utf8>>, binary:part(P, 0, 48)).

should_have_0_blocks_1_percent_bar_for_5_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,5}, {20,30}}),
    ?_assertEqual(<<"░░░░░░░░░░░░░░░ 1%"/utf8>>, binary:part(P, 0, 48)).

should_have_1_block_6_percent_bar_for_25_jan(_) ->
    P = formatter:year_progress_bar({{2020,1,25}, {0,0}}),
    ?_assertEqual(<<"▓░░░░░░░░░░░░░░ 6%"/utf8>>, binary:part(P, 0, 48)).

should_have_15_block_96_percent_bar_for_20_dec(_) ->
    P = formatter:year_progress_bar({{2020,12,20}, {0,0}}),
    ?_assertEqual(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 96%"/utf8>>, binary:part(P, 0, 49)).

should_have_15_block_99_percent_bar_for_31_dec(_) ->
    P = formatter:year_progress_bar({{2020,12,31}, {23,59}}),
    ?_assertEqual(<<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 99%"/utf8>>, binary:part(P, 0, 49)).

should_have_year_number_in_message(_) ->
    P = formatter:year_progress_bar({{2020,12,31}, {23,59}}),
    ?_assertEqual(<<$\n, "2 0 2 0"/utf8>>, binary:part(P, byte_size(P), -8)).

should_have_full_bar_of_2020_and_zero_bar_of_2021_for_1_jan_2021(_) ->
    meck:expect(date, start_of_year_date, fun() -> {2021, 1, 1} end),
    meck:expect(date, end_of_year_date, fun() -> {2021, 12, 31} end),

    P = formatter:year_progress_bar({{2021,1,1}, {0,0}}),
    E = <<"▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 100%    \n2 0 2 0\n\n░░░░░░░░░░░░░░░ 0%      \n2 0 2 1"/utf8>>,
    ?_assertEqual(E, P).

request_response_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [fun should_give_short_info_about_long_gun_response/1,
      fun should_give_short_info_about_short_gun_response/1,
      fun should_give_short_info_about_long_request_body/1,
      fun should_give_short_info_about_short_request_body/1]}.

should_give_short_info_about_long_gun_response(_) ->
    ?_assertMatch(
        [200, "this is a beginning part of a long response body, we want to keep the log output clean, because of that we return not the full body as you may expects, but only first 255 bytes of the response, that one can call a pragmatic approach. 1-----------------345"],
        formatter:gun_response_printable(#{status_code => 200, body => <<"this is a beginning part of a long response body, we want to keep the log output clean, because of that we return not the full body as you may expects, but only first 255 bytes of the response, that one can call a pragmatic approach. 1-----------------345-----this is 285 bytes binary.">>})
    ).

should_give_short_info_about_short_gun_response(_) ->
    ?_assertMatch(
        [400, ""],
        formatter:gun_response_printable(#{status_code => 400, body => <<"">>})
    ).

should_give_short_info_about_long_request_body(_) ->
    ?_assertMatch(
        ["this is a beginning part of a long response body, we want to keep the log output clean, because of that we return not the full body as you may expects, but only first 255 bytes of the response, that o ... -----------------345-----this is 285 bytes binary."],
        formatter:gun_request_body_printable(<<"this is a beginning part of a long response body, we want to keep the log output clean, because of that we return not the full body as you may expects, but only first 255 bytes of the response, that one can call a pragmatic approach. 1-----------------345-----this is 285 bytes binary.">>)
    ).

should_give_short_info_about_short_request_body(_) ->
    ?_assertMatch(
        [""],
        formatter:gun_request_body_printable(<<"">>)
    ).
