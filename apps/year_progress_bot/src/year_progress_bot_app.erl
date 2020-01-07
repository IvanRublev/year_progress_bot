%%%-------------------------------------------------------------------
%% @doc year_progress_bot public API
%% @end
%%%-------------------------------------------------------------------

-module(year_progress_bot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = db:create_schema(),
    year_progress_bot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
