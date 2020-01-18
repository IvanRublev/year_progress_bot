-module(notifyer_worker).
-export([child_spec/0,start_link/0,loop/1,init/1]).
-compile([{parse_transform, lager_transform}]).

child_spec() ->
    #{id => notifyer_worker, 
      restart => permanent,
      start => {notifyer_worker, start_link, []}}.

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    case application:get_env(year_progress_bot, notifyer_loop_period) of
        {ok, Period} -> 
            proc_lib:init_ack(Parent, {ok, self()}),
            lager:info("Start notifyer loop with period of ~p ms", [Period]),
            loop(Period);

        _ -> 
            exit(no_loop_period_setting)
    end.


loop(Period) ->
    receive after Period ->
        notifyer:evaluate_send_progress({25, 1000}),
        loop(Period)
    end.
