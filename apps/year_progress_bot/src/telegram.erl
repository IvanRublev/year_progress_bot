-module(telegram).
-export([send_message/2]).

send_message(_ChatId, ProgressDate) ->
    Msg = formatter:year_progress_bar(ProgressDate),
    ok.