-module(db_test).
-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    {foreach,
     fun() ->
         meck:new(sumo, [passthrough]),
         meck:expect(sumo, create_schema, fun() -> ok end)
     end,
     fun(_) ->
         meck:unload(sumo)
     end,
     [fun should_create_schema/1]}.

should_create_schema(_) ->
    db:create_schema(),
    ?_assert(meck:called(sumo, create_schema, [])).
