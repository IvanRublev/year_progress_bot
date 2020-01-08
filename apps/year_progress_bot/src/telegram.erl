-module(telegram).
-export([send_message/2]).

send_message(ChatId, ProgressDate) ->
    Msg = formatter:year_progress_bar(ProgressDate),
    Payload = jiffy:encode({[{chat_id, ChatId}, {text, Msg}]}),
    ok.