-module(health).
-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, "ok", Req0),
    {ok, Req, Opts}.
