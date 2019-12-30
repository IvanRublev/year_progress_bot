-module(endpoint_tests).
-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {foreach,
    fun() ->
        meck:new(db, [{stub_all, ok}]),
        meck:new(cowboy_req),
        Body = <<"{
            \"message\":{
                \"chat\":{
                    \"last_name\":\"Test Lastname\",
                    \"id\":123456
                },
                \"text\":\"/start\"
            }
        }">>,
        meck:expect(cowboy_req, read_urlencoded_body, fun(_) -> {ok, [{Body, true}], {}} end),
        meck:expect(cowboy_req, reply, fun(_, _, _, _) -> {} end),
        meck:new(date, [passthrough]),
        meck:expect(date, now, [], {{2020, 01, 02}, {12, 08, 22}}),
        meck:expect(date, end_of_today, [], {{2020, 01, 02}, {23, 59, 59}})
    end,
    fun(_) ->
        meck:unload(date),
        meck:unload(cowboy_req),
        meck:unload(db)
    end,
    [fun should_record_chat_id_on_start/1]}.

should_record_chat_id_on_start(_) ->
    endpoint:init({}, #{}),

    ?_assert(meck:called(db, add_notified_chat, [123456, {{2020, 01, 02}, {23, 59, 59}}])).
