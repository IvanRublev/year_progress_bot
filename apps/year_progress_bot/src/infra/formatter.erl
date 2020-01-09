-module(formatter).
-export([year_progress_bar/1]).

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
    case DT of 
        {_, 1, 1} -> bar(BarLen, 0, 100, Y-1);
        _ -> bar(FillBar, EmptyBar, Percent, Y)
    end.
    % io:format(user, "pc ~p ~p ~p", [PassedDays, TotalDays, Percent]),

bar(FillBar, EmptyBar, Percent, Year) ->
    YearSp = lists:join(32, io_lib:format("~B", [Year])),
    io_lib:format("~*c~*c ~B%~n", [FillBar, $▓, EmptyBar, $░, Percent]) ++ YearSp.
