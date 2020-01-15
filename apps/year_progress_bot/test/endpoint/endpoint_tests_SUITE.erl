-module(endpoint_tests_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    application:set_env([{year_progress_bot, [{port, 8080}]}], [{persistent, true}]),
    year_progress_bot_app:launch_endpoint(),
    [{host_url, "http://localhost:8080"} | Config].

end_per_suite(_Config) ->
    application:stop(cowboy).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> 
    [should_reply_with_warning_about_periodic_notification].

should_reply_with_warning_about_periodic_notification(Config) ->
    Host = ?config(host_url, Config),
    Res = ?perform_get(Host ++ "/start"),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"text">>, <<"Bot would send the year progress bar daily.">>, Res),
    ok.
