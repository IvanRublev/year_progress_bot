-module(chats).
-behaviour(sumo_doc).
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([new/2]).

new(Id, Date) ->
    #{id => Id, notified_at => Date}.

% sumo_doc behavior
sumo_wakeup(Data) ->
  Data.

sumo_sleep(Chat) ->
  Chat.

sumo_schema() ->
  sumo:new_schema(chats, [
    sumo:new_field(id, string, [{length, 17}, not_null, id]),
    sumo:new_field(notified_at, datetime, [not_null])
  ]).
