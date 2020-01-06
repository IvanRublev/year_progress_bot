-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    {foreach,
     fun() ->
         meck:new(sumo),
         meck:expect(sumo, create_schema, fun() -> ok end),
         meck:expect(sumo, find_by, fun(_, _, _, _) -> [] end)
     end,
     fun(_) ->
         meck:unload(sumo)
     end,
     [fun should_create_schema/1,
      fun should_request_unnotified_chats_as_notified_at_before_set_date/1]}.

should_create_schema(_) ->
    db:create_schema(),

    ?_assert(meck:called(sumo, create_schema, [])).

should_request_unnotified_chats_as_notified_at_before_set_date(_) ->
    db:unnotified_chats(10, {{2020,1,7}, {11,45}}),
    
    ?_assert(meck:called(sumo, find_by, [chats, [{notified_at, '<', {{2020,1,7}, {11,45}}}], 10, 0])).
