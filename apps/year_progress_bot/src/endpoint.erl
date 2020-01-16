-module(endpoint).
-export([init/2]).

init(Req0, Opts) ->
    {ok, [{Body, true}], _} = cowboy_req:read_urlencoded_body(Req0),
    BodyJson = jiffy:decode(Body, [return_maps]),
    {ChatId, Text} = parse_update(BodyJson),
    case Text of
        <<"/start">> -> reply_on_start(ChatId, Req0, Opts);
        <<"/progress">> -> reply_on_progress(ChatId, Req0, Opts);
        <<"/help">> -> reply_on_help(ChatId, Req0, Opts);
        _ -> reply_not_implemented_error(Req0, Opts)
    end.

parse_update(Message) ->
    #{<<"message">> := #{
        <<"chat">> := #{<<"id">> := ChatId}, 
        <<"text">> := Text
    }} = Message,
    {ChatId, Text}.

reply_on_start(ChatId, Req0, Opts) ->
    Msg = "Bot will send the year progress bar daily.\nLike the following.\n" ++ formatter:year_progress_bar(date:now()),
    Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[
        {<<"text">>, Msg},
        {<<"chat_id">>, ChatId}
    ]}), Req0),
	{ok, Req, Opts}.

reply_on_progress(ChatId, Req0, Opts) ->
    Msg = formatter:year_progress_bar(date:now()),
    Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[
        {<<"text">>, Msg},
        {<<"chat_id">>, ChatId}
    ]}), Req0),
	{ok, Req, Opts}.

reply_on_help(ChatId, Req0, Opts) ->
    Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[
        {<<"text">>, "Bot sends the year progress bar. The following commands are supported:\n/start - start the bot\n/progress - show today's progress of the year\n/help - this message"},
        {<<"chat_id">>, ChatId}
    ]}), Req0),
	{ok, Req, Opts}.

reply_not_implemented_error(Req0, Opts) ->
    Req = cowboy_req:reply(501, #{<<"content-type">> => <<"plain/text">>}, "Not implemented", Req0),
	{ok, Req, Opts}.
