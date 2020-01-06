-module(db).
-export([unnotified_chats/1, mark_chats_notified/2]).

unnotified_chats(_Count) ->
    [].

mark_chats_notified(_List, _Date) ->
    ok.