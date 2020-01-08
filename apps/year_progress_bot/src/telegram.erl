-module(telegram).
-export([send_message/2]).

send_message(ChatId, ProgressDate) ->
    {ok, Host} = application:get_env(year_progress_bot, tel_host),
    {ok, Conn} = shotgun:open(Host, 443, https),
    {ok, Token} = application:get_env(year_progress_bot, tel_token),
    Path = "/bot" ++ Token ++ "/sendMessage",
    Headers = #{<<"content-type">> => <<"application/json">>},
    Msg = formatter:year_progress_bar(ProgressDate),
    Payload = jiffy:encode({[{chat_id, ChatId}, {text, Msg}]}),
    {ok, Response} = shotgun:post(Conn, Path, Headers, Payload),
    ok = shotgun:close(Conn),
    #{status_code := Status} = Response,
    if 
        (Status >= 200) and (Status =< 299) -> ok;
        true -> {error, Status}
    end.