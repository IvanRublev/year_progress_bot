-module(endpoint).
-export([init/2]).

init(Req0, [start] = Opts) ->
    {ok, [{Body, true}], _} = cowboy_req:read_urlencoded_body(Req0),
    BodyJson = jiffy:decode(Body, [return_maps]),
    ChatId = chat_id(BodyJson),
    Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[
        {<<"text">>, <<"Bot would send the year progress bar daily.">>},
        {<<"chat_id">>, ChatId}
    ]}), Req0),
	{ok, Req, Opts};
init(_Req0, _Opts) ->
    ok.

chat_id(Message) ->
    #{<<"message">> := #{<<"chat">> := #{<<"id">> := Id}}} = Message,
    Id.
