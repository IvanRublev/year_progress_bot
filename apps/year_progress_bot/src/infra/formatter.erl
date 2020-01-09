-module(formatter).
-export([year_progress_bar/1]).

year_progress_bar(Date) ->
    StartDays = calendar:date_to_gregorian_days(date:start_of_year_date()),
    {DT, _} = Date,
    TodayDays = calendar:date_to_gregorian_days(DT),
    EndDays = calendar:date_to_gregorian_days(date:end_of_year_date()),
    PassedDays = TodayDays-StartDays,
    TotalDays = EndDays-StartDays,
    Percent = round(PassedDays/TotalDays*100),
    BarLen = 15,
    FillBar = round(PassedDays*BarLen/TotalDays),
    EmptyBar = BarLen-FillBar,
    io_lib:format("~*c~*c ~B%", [FillBar, $▓, EmptyBar, $░, Percent]).
    % io:format(user, "pc ~p ~p ~p", [PassedDays, TotalDays, Percent]),
    % FillBar = round(PassedDays*BarLen/TotalDays),
    % EmptyBar = BarL