-module(telegram_tests).
-include_lib("eunit/include/eunit.hrl").

send_test_() ->
    {foreach,
     fun() ->
         meck:new(formatter),
         meck:expect(formatter, year_progress_bar, fun(_) -> "%" end),
         meck:new(jiffy),
         meck:expect(jiffy, encode, fun(_) -> <<"{json}">> end),
         meck:new(shotgun),
         meck:expect(shotgun, open, fun(_,_,_) -> {ok, 123} end),
         meck:expect(shotgun, post, fun(_,_,_,_) -> {ok, #{status_code => 200}} end),
         meck:expect(shotgun, close, fun(_) -> ok end),
         application:set_env([
            {year_progress_bot, [
                {tel_token, "TOKEN"},
                {tel_host, "HOST"}
            ]}
         ], [{persistent, true}])
     end,
     fun(_) ->
         meck:unload(shotgun),
         meck:unload(jiffy),
         meck:unload(formatter)
     end,
     [fun should_get_progress_bar_from_date/1,
     fun should_make_json_payload_for_progress_bar_message/1,
     fun should_POST_message_payload_to_telegram_server/1]}.

should_get_progress_bar_from_date(_) ->
    telegram:send_message(0, {{2020,10,11}, {11,50}}),
    
    ?_assert(meck:called(formatter, year_progress_bar, [{{2020,10,11}, {11,50}}])).

should_make_json_payload_for_progress_bar_message(_) ->
    telegram:send_message(15, {{2020,10,11}, {11,50}}),

    ?_assert(meck:called(jiffy, encode, [{[{chat_id, 15}, {text, "%"}]}])).

should_POST_message_payload_to_telegram_server(_) ->
    telegram:send_message(15, {{2020,10,11}, {11,50}}),

    [?_assert(meck:called(shotgun, open, ["HOST", 443, https])),
     ?_assert(meck:called(shotgun, post, ['_', "/botTOKEN/sendMessage", #{<<"content-type">> => <<"application/json">>}, <<"{json}">>])),
     ?_assert(meck:called(shotgun, close, [123]))].

should_return_ok_on_status_2xx(_) ->
    meck:expect(shotgun, post, fun(_,_,_,_) -> {ok, #{status_code => 210}} end),

    ?_assertMatch(ok, telegram:send_message(15, {{2020,10,11}, {11,50}})).
