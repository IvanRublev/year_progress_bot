-module(endpoint_tests_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    application:set_env([
        {year_progress_bot, [
            {port, 8080},
            {webhook_path, "/some_uuid_path"}
        ]}
    ], [{persistent, true}]),
    year_progress_bot_app:launch_endpoint(),
    [{host_url, "http://localhost:8080/some_uuid_path"} | Config].

end_per_suite(_Config) ->
    application:stop(cowboy).

init_per_testcase(_TestCase, Config) ->
    meck:new(calendar, [unstick, passthrough]),
    meck:expect(calendar, universal_time, fun() -> {{2020,2,25},{10,12,30}} end),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(calendar).

all() -> 
    [should_reply_with_warning_about_periodic_notification_on_start,
     should_reply_with_chat_id_received_on_start,
     should_reply_with_progress_bar_on_progress].

should_reply_with_warning_about_periodic_notification_on_start(Config) ->
    Res = ?perform_post(
        ?config(host_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/start">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"text">>, <<"Bot would send the year progress bar daily.">>, Res),
    ok.

should_reply_with_chat_id_received_on_start(Config) ->
    Res = ?perform_post(
        ?config(host_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1213141, <<"/start">>)
    ),
    ?assert_json_value(<<"chat_id">>, 1213141, Res).

should_reply_with_progress_bar_on_progress(Config) ->
    Res = ?perform_post(
        ?config(host_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/progress">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"text">>, binary_to_list(<<"▓▓░░░░░░░░░░░░░ 15%\n2 0 2 0">>), Res).

json_message(ChatId, Text) -> 
    <<<<"{
        \"message\":{
            \"chat\":{
                \"last_name\":\"Test Lastname\",
                \"id\":">>/binary, (list_to_binary(io_lib:format("~w", [ChatId])))/binary, <<",
                \"first_name\":\"Test\",
                \"username\":\"Test\"
            },
            \"text\":\"">>/binary, Text/binary, <<"\"
        }
    }">>/binary>>.
