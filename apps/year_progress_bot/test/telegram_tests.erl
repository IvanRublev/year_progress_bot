-module(telegram_tests).
-include_lib("eunit/include/eunit.hrl").

send_test_() ->
    {foreach,
     fun() ->
         meck:new(formatter),
         meck:expect(formatter, year_progress_bar, fun(_) -> "%" end),
         meck:new(jiffy),
         meck:expect(jiffy, encode, fun(_) -> "{json}" end)
     end,
     fun(_) ->
         meck:unload(jiffy),
         meck:unload(formatter)
     end,
     [fun should_get_progress_bar_from_date/1,
     fun should_make_json_payload_for_progress_bar_message/1]}.

should_get_progress_bar_from_date(_) ->
    telegram:send_message(0, {{2020,10,11}, {11,50}}),
    
    ?_assert(meck:called(formatter, year_progress_bar, [{{2020,10,11}, {11,50}}])).

should_make_json_payload_for_progress_bar_message(_) ->
    telegram:send_message(15, {{2020,10,11}, {11,50}}),

    ?_assert(meck:called(jiffy, encode, [{[{chat_id, 15}, {text, "%"}]}])).