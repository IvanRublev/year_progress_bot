-module(endpoint).
-export([init/2]).
-compile([{parse_transform, lager_transform}]).

init(Req0, Opts) ->
    lager:debug("Endpoint requested ->"),
    case cowboy_req:read_urlencoded_body(Req0) of 
        {ok, [{Body, true}], _} ->
            lager:debug("-> Body: ~s...", formatter:gun_request_body_printable(Body)),
            BodyJson = jiffy:decode(Body, [return_maps]),
            case parse_update(BodyJson) of
                {ChatId, Text} ->
                    case Text of
                        <<"/start">> ->
                            db:add_notified_chat(ChatId, date:end_of_today()),
                            reply_on_start(ChatId, Req0, Opts);
                        <<"/progress">> -> reply_on_progress(ChatId, Req0, Opts);
                        <<"/help">> -> reply_on_help(ChatId, Req0, Opts);
                        _ -> reply_not_implemented_error(Req0, Opts)
                    end;
                _ ->
                    reply_bad_request_error(Req0, Opts)
            end;
        _ ->
            reply_bad_request_error(Req0, Opts)
    end.

parse_update(Message) ->
    case Message of
        #{<<"message">> := #{
            <<"chat">> := #{<<"id">> := ChatId}, 
            <<"text">> := Text
        }} -> 
            {ChatId, Text};
        _ -> 
            error
    end.

cowboy_reply_fun(Status, Headers, Body, Req0, Opts) ->
    lager:debug("<- Endpoint reply with status: ~p body: ~s...", [Status, Body]),
    Req = cowboy_req:reply(Status, Headers, Body, Req0),
    {ok, Req, Opts}.

reply_on_start(ChatId, Req0, Opts) ->
    Top = <<"Bot will send the year progress bar daily.\nLike the following.\n"/utf8>>,
    Bar = formatter:year_progress_bar(date:now()),
    Closing = <<"\n\nYou can add this bot to a channel and it will post progress bar there."/utf8>>,
    Msg = <<Top/binary, Bar/binary, Closing/binary>>,
    cowboy_reply_fun(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[
        {<<"text">>, Msg},
        {<<"chat_id">>, ChatId}
    ]}), Req0, Opts).

reply_on_progress(ChatId, Req0, Opts) ->
    Msg = formatter:year_progress_bar(date:now()),
    cowboy_reply_fun(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[
        {<<"text">>, Msg},
        {<<"chat_id">>, ChatId}
    ]}), Req0, Opts).

reply_on_help(ChatId, Req0, Opts) ->
    cowboy_reply_fun(200, #{
		<<"content-type">> => <<"application/json">>
	}, jiffy:encode({[
        {<<"text">>, <<"Bot sends the year progress bar. The following commands are supported:\n/start - start the bot\n/progress - show today's progress of the year\n/help - this message">>},
        {<<"chat_id">>, ChatId}
    ]}), Req0, Opts).

reply_not_implemented_error(Req0, Opts) ->
    cowboy_reply_fun(501, #{<<"content-type">> => <<"text/html">>}, "Not implemented", Req0, Opts).

reply_bad_request_error(Req0, Opts) ->
    cowboy_reply_fun(400, #{<<"content-type">> => <<"text/html">>}, "Bad request", Req0, Opts).
