-module(endpoint).
-export([init/2]).

init(Req0, [start] = Opts) ->
    Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[{<<"text">>, <<"Bot would send the year progress bar daily.">>}]}), Req0),
	{ok, Req, Opts};

init(_Req0, _Opts) ->
    ok.
