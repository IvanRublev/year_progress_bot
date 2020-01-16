%%%-------------------------------------------------------------------
%% @doc year_progress_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(year_progress_bot_app).

-behaviour(application).

-export([start/2, stop/1, launch_endpoint/0]).

start(_StartType, _StartArgs) ->
    ok = db:create_schema(),
    launch_endpoint(),
    year_progress_bot_sup:start_link().

stop(_State) ->
    stop_endpoint(),
    ok.

%% internal functions
launch_endpoint() ->
    {ok, Path} = application:get_env(year_progress_bot, webhook_path),
    Dispatch = cowboy_router:compile([{'_', [{Path, endpoint, []}]}]),
    {ok, Port} = application:get_env(year_progress_bot, port),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{env => #{dispatch => Dispatch}}).

stop_endpoint() ->
    cowboy:stop_listener(http).
