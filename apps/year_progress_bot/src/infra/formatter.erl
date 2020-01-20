-module(formatter).
-export([year_progress_bar/1, gun_response_printable/1, gun_request_body_printable/1]).

year_progress_bar(Date) ->
    StartDays = calendar:date_to_gregorian_days(date:start_of_year_date()),
    {DT, _} = Date,
    TodayDays = calendar:date_to_gregorian_days(DT),
    EndDays = calendar:date_to_gregorian_days(date:end_of_year_date())+1,
    PassedDays = TodayDays-StartDays,
    TotalDays = EndDays-StartDays,
    Percent = trunc(PassedDays/TotalDays*100),
    BarLen = 15,
    FillBar = round(PassedDays*BarLen/TotalDays),
    EmptyBar = BarLen-FillBar,
    {Y, _, _} = DT,
    List = case DT of 
        {_, 1, 1} -> bar(BarLen, 0, 100, Y-1) ++ io_lib:format("~n~n", []) ++ bar(0, BarLen, 0, Y);
        _ -> bar(FillBar, EmptyBar, Percent, Y)
    end,
    unicode:characters_to_binary(List).

bar(FillBar, EmptyBar, Percent, Year) ->
    YearSp = lists:join(32, io_lib:format("~B", [Year])),
    io_lib:format("~*tc~*tc ~B%~n", [FillBar, $▓, EmptyBar, $░, Percent]) ++ YearSp.

gun_response_printable(Resp) ->
    [maps:get(status_code, Resp, undef),
     list_body_first(maps:get(body, Resp, <<"">>), 255)].

list_body_first(Body, Len) ->
    binary:bin_to_list(Body, {0, min(Len, byte_size(Body))}).

list_body_last(Body, Len) ->
    binary:bin_to_list(Body, {byte_size(Body), -min(Len, byte_size(Body))}).

gun_request_body_printable(Body) ->
    First = list_body_first(Body, 200),
    Last = list_body_last(Body, 50),
    [if 
        length(First)+length(Last) > 0 -> First ++ " ... " ++ Last;
        true -> ""
    end].
