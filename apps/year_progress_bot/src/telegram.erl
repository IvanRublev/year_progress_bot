-module(telegram).
-export([send_message/2, register_webhook/0]).


send_message(ChatId, ProgressDate) ->
    case application:get_env(year_progress_bot, telegram_integrate) of
        {ok, true} -> send_message_fun(ChatId, ProgressDate);
        _ -> ok
    end.

send_message_fun(ChatId, ProgressDate) ->
    {ok, Host} = application:get_env(year_progress_bot, tel_host),
    {ok, Conn} = shotgun:open(Host, 443, https),
    {ok, Token} = application:get_env(year_progress_bot, tel_token),
    Path = "/bot" ++ Token ++ "/sendMessage",
    Headers = #{<<"content-type">> => <<"application/json">>},
    Msg = formatter:year_progress_bar(ProgressDate),
    Payload = jiffy:encode({[{chat_id, ChatId}, {text, Msg}]}),
    
    {ok, Response} = shotgun:post(Conn, Path, Headers, Payload),
    
    ok = shotgun:close(Conn),
    
    #{status_code := Status, body := Body} = Response,
    if 
        (Status >= 200) and (Status =< 299) -> 
            RespDoc = jiffy:decode(Body, [return_maps]),
            case maps:get(<<"ok">>, RespDoc) of
                true -> ok;
                false -> {error, internal, Body}
            end;
        true -> {error, Status, Body}
    end.

register_webhook() ->
    case application:get_env(year_progress_bot, telegram_integrate) of
        {ok, true} -> register_webhook_fun();
        _ -> ok
    end.

register_webhook_fun() ->
    {ok, Host} = application:get_env(year_progress_bot, tel_host),
    {ok, Conn} = shotgun:open(Host, 443, https),
    {ok, Token} = application:get_env(year_progress_bot, tel_token),
    
    {ok, SelfHost} = application:get_env(year_progress_bot, host),
    {ok, HookPath} = application:get_env(year_progress_bot, webhook_path),
    HookUrl = "https://" ++ SelfHost ++ HookPath,
    Path = "/bot" ++ Token ++ "/setWebhook?" ++
        "url=" ++ http_uri:encode(HookUrl) ++ 
        "&allowed_updates=" ++ http_uri:encode("[\"message\",\"channel_post\"]"),
    {ok, _} = shotgun:get(Conn, Path),
    ok = shotgun:close(Conn).
