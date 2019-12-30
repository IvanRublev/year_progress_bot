-module(db).
-export([create_schema/0, unnotified_chats/2, mark_chats_notified/2, add_notified_chat/2]).

create_schema() ->
    sumo:create_schema().

unnotified_chats(Count, Date) ->
    Chats = sumo:find_by(chats, [{notified_at, '<', Date}], Count, 0),
    lists:map(fun(Ch) -> maps:get(id, Ch) end, Chats).

mark_chats_notified(List, Date) ->
    [persist(Id, Date) || Id <- List].

add_notified_chat(Id, Date) ->
    persist(Id, Date, false).

persist(Id, Date, Ensure) ->
    Chat = chats:new(Id, Date),
    Changeset = sumo_changeset:cast(chats, Chat, #{}, [id, notified_at]),
    Valid = sumo_changeset:validate_required(Changeset, [id, notified_at]),
    Res = sumo:persist(Valid),
    if Ensure ->
        {ok, _} = Res;
        true -> ok
    end.

persist(Id, Date) ->
    persist(Id, Date, true).
