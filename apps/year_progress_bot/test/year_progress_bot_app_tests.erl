-module(year_progress_bot_app_tests).
-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {foreach,
     fun() ->
         meck:new(db),
         meck:expect(db, create_schema, fun() -> ok end),
         meck:new(year_progress_bot_sup),
         meck:expect(year_progress_bot_sup, start_link, fun() -> ok end)
     end,
     fun(_) ->
         meck:unload(year_progress_bot_sup),
         meck:unload(db)
     end,
     [fun should_create_db_schemas_on_start/1,
      fun should_start_bot_supervisor/1]}.


should_create_db_schemas_on_start(_) ->
    year_progress_bot_app:start({}, {}),
    ?_assert(meck:called(db, create_schema, [])).

should_start_bot_supervisor(_) ->
    year_progress_bot_app:start({}, {}),
    ?_assert(meck:called(year_progress_bot_sup, start_link, [])).
