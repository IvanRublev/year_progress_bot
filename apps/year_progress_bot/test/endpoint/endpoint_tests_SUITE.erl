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
            {tel_token, "tel_token"},
            {tel_host, "tel_host"},
            {host, "self.host"},
            {port, 8080},
            {webhook_path, "/some_uuid_path"}
        ]}
    ], [{persistent, true}]),

    meck:new(telegram),
    meck:expect(telegram, register_webhook, fun() -> ok end),
    year_progress_bot_app:launch_endpoint(),
    meck:unload(telegram),

    lists:merge([
        {bot_endpoint_url, "http://localhost:8080/some_uuid_path"},
        {health_endpoint_url, "http://localhost:8080/health"}
    ], Config).

end_per_suite(_Config) ->
    application:stop(cowboy).

init_per_testcase(_TestCase, Config) ->
    meck:new(calendar, [unstick, passthrough]),
    meck:expect(calendar, universal_time, fun() -> {{2020,2,25},{10,12,30}} end),
    meck:new(db, [{stub_all, ok}]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(db),
    meck:unload(calendar).

all() -> 
    [should_reply_with_warning_about_periodic_notification_on_start,
     should_reply_with_chat_id_received_on_start,
     should_reply_with_progress_bar_on_progress,
     should_reply_with_supported_commands_on_help,
     should_reply_with_501_not_implemented_on_unknown_command,
     should_reply_with_400_bad_request_on_empty_body,
     should_reply_with_400_bad_request_on_malformed_json_body,
     should_reply_ok_on_health_path].

should_reply_with_warning_about_periodic_notification_on_start(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/start">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"text">>, <<"Bot will send the year progress bar daily.\nLike the following.\n▓▓░░░░░░░░░░░░░ 15%\n2 0 2 0\n\nYou can add this bot to a channel and it will post progress bar there."/utf8>>, Res),
    ok.

should_reply_with_chat_id_received_on_start(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1213141, <<"/start">>)
    ),
    ?assert_json_value(<<"chat_id">>, 1213141, Res).

should_reply_with_progress_bar_on_progress(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/progress">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"text">>, <<"▓▓░░░░░░░░░░░░░ 15%\n2 0 2 0"/utf8>>, Res).

should_reply_with_supported_commands_on_help(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/help">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"text">>, <<"Bot sends the year progress bar. The following commands are supported:\n/start - start the bot\n/progress - show today's progress of the year\n/help - this message">>, Res).

should_reply_with_501_not_implemented_on_unknown_command(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/unknown">>)
    ),
    ?assert_status(501, Res),
    ?assert_header_value("content-type", "text/html", Res),
    ?assert_body("Not implemented", Res).

should_reply_with_400_bad_request_on_empty_body(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        <<"">>
    ),
    ?assert_status(400, Res),
    ?assert_header_value("content-type", "text/html", Res),
    ?assert_body("Bad request", Res).

should_reply_with_400_bad_request_on_malformed_json_body(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        <<"{\"some\": true}">>
    ),
    ?assert_status(400, Res),
    ?assert_header_value("content-type", "text/html", Res),
    ?assert_body("Bad request", Res).

should_reply_ok_on_health_path(Config) ->
    Res = ?perform_get(?config(health_endpoint_url, Config)),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "text/html", Res),
    ?assert_body("ok", Res).

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
