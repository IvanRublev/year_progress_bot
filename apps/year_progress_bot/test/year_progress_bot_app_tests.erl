-module(year_progress_bot_app_tests).
-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {foreach,
     fun() ->
         meck:new(db),
         meck:expect(db, create_schema, fun() -> ok end),
         meck:new(year_progress_bot_sup),
         meck:expect(year_progress_bot_sup, start_link, fun() -> ok end),
         meck:new(cowboy),
         meck:expect(cowboy, start_clear, fun(_,_,_) -> {ok, {}} end),
         meck:new(cowboy_router),
         meck:expect(cowboy_router, compile, fun(_) -> dspch end),
         application:set_env([
            {year_progress_bot, [
                {port, 12345}
            ]}
         ], [{persistent, true}])
     end,
     fun(_) ->
         meck:unload(cowboy_router),
         meck:unload(cowboy),
         meck:unload(year_progress_bot_sup),
         meck:unload(db)
     end,
     [fun should_create_db_schemas_on_start/1,
      fun should_start_bot_supervisor/1,
      fun should_compile_routes_to_endpoint/1,
      fun should_start_endpoint/1]}.

should_create_db_schemas_on_start(_) ->
    year_progress_bot_app:start({}, {}),
    ?_assert(meck:called(db, create_schema, [])).

should_start_bot_supervisor(_) ->
    year_progress_bot_app:start({}, {}),
    ?_assert(meck:called(year_progress_bot_sup, start_link, [])).

should_compile_routes_to_endpoint(_) ->
    year_progress_bot_app:start({}, {}),
    ?_assert(meck:called(cowboy_router, compile, [[
		{'_', [
			{"/start", endpoint, [start]},
            {"/help", endpoint, [help]},
            {"/progress", endpoint, [progress]}
		]}
	]])).

should_start_endpoint(_) ->
    year_progress_bot_app:start({}, {}),
    ?_assert(meck:called(cowboy, start_clear, [http, 12345, #{env => #{dispatch => dspch}}])).
