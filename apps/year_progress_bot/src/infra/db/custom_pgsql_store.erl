-module(custom_pgsql_store).
-include_lib("epgsql/include/epgsql.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([sumo_store_pgsql]).

-export([persist_new/3]).

persist_new(Changeset, DocName, State) ->
  #{conn := Conn} = State,
  
  NPFieldNames = maps:keys(Changeset),
  NPColumnNames = lists:map(fun escape/1, NPFieldNames),
  NPColumnsNamesCSV = string:join(NPColumnNames, ", "),
  SlotsFun = fun(N) -> [" $", integer_to_list(N), " "]  end,
  InsertSlots = lists:map(SlotsFun, lists:seq(1, length(NPFieldNames))),
  InsertSlotsCSV = string:join(InsertSlots, ", "),
  Query = [
    "INSERT INTO ",
    escape(DocName),
    " ( ", NPColumnsNamesCSV, " ) ",
    " VALUES ",
    " ( ", InsertSlotsCSV, " ) "
  ],

  Values = lists:map(fun(Key) -> maps:get(Key, Changeset, undefined) end, NPFieldNames),
  ToNullFun = fun
    (undefined) -> null;
    (Value) -> Value
  end,
  ProcessedValues = lists:map(ToNullFun, Values),

  Res = epgsql:equery(Conn, stringify(Query), ProcessedValues),
  parse_result(Res, State).

%%% Internal

%% @private
escape(Name) when is_atom(Name) ->
  ["\"", atom_to_list(Name), "\""];
escape(String) ->
  ["\"", String, "\""].

%% @private
stringify(Sql) -> binary_to_list(iolist_to_binary(Sql)).

%% @private
parse_result(Res, State) ->
    case Res of
        {ok, Count, _Columns, _Rows} ->
            {ok, {raw, Count}, State};
        {ok, Count} ->
            {ok, {raw, Count}, State};
        {error, {error,error,_,unique_violation,_,_}} ->
            {ok, {raw, 0}, State};
        {error, Error} ->
            {ok, Error, State}
    end.
