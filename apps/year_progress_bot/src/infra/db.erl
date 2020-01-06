-module(db).
-export([create_schema/0, unnotified_chats/2, mark_chats_notified/2]).

create_schema() ->
    sumo:create_schema().

unnotified_chats(Count, Date) ->
    sumo:find_by(chats, [{notified_at, '<', Date}], Count, 0),
    [].

mark_chats_notified(_List, _Date) ->
    ok.