-module(db).
-export([unnotified_chats/2, mark_chats_notified/2]).

unnotified_chats(_Count, _Date) ->
    [].

mark_chats_notified(_List, _Date) ->
    ok.