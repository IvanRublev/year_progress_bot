%%%-------------------------------------------------------------------
%% @doc year_progress_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(year_progress_bot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = db:create_schema(),
    launch_endpoint(),
    year_progress_bot_sup:start_link().

launch_endpoint() ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/start", endpoint, [start]},
            {"/help", endpoint, [help]},
            {"/progress", endpoint, [progress]}
		]}
	]),
    {ok, Port} = application:get_env(year_progress_bot, port),
    cowboy:start_clear(http, Port, #{env => #{dispatch => Dispatch}}).

stop(_State) ->
    ok.

%% internal functions
