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
     should_reply_to_chat_id_received_on_start,
     should_reply_with_warning_about_periodic_notification_on_start_in_channel,
     should_reply_to_chat_id_received_on_start_in_channel,
    %  should_reply_with_warning_about_periodic_notification_on_start_in_channel_addressed,
     should_reply_with_progress_bar_on_progress,
     should_reply_with_progress_bar_on_progress_in_channel,
     should_reply_with_supported_commands_on_help,
     should_reply_with_supported_commands_on_help_in_channel,
     should_reply_with_200_dont_know_emoji_on_unknown_command,
     should_reply_with_200_ignore_on_unknown_command_in_channel,
     should_reply_with_200_dont_know_emoji_on_unhandled_message,
     should_reply_with_200_ignore_on_unhandled_message_in_channel,
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
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"Bot will send you the year progress bar daily.\nLike the following.\n▓▓░░░░░░░░░░░░░ 15%     \n2 0 2 0\n\nYou can add this bot to a channel as well, and it will post progress bar there."/utf8>>, Res).

should_reply_to_chat_id_received_on_start(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1213141, <<"/start">>)
    ),
    ?assert_json_value(<<"chat_id">>, 1213141, Res).

should_reply_with_warning_about_periodic_notification_on_start_in_channel(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        channel_message(-1214111, <<"/start">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"Bot will send you the year progress bar daily.\nLike the following.\n▓▓░░░░░░░░░░░░░ 15%     \n2 0 2 0\n\nYou can add this bot to a channel as well, and it will post progress bar there."/utf8>>, Res).

should_reply_to_chat_id_received_on_start_in_channel(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        channel_message(-1214111, <<"/start">>)
    ),
    ?assert_json_value(<<"chat_id">>, -1214111, Res).

% should_reply_with_warning_about_periodic_notification_on_start_in_channel_addressed(Config) ->
%     Res = ?perform_post(
%         ?config(bot_endpoint_url, Config),
%         [{<<"content-type">>, <<"application/json">>}],
%         channel_message(-1214111, <<"/start@yrpb_bot">>)
%     ),
%     ?assert_status(200, Res),
%     ?assert_header_value("content-type", "application/json", Res),
%     ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
%     ?assert_json_value(<<"text">>, <<"Bot will send you the year progress bar daily.\nLike the following.\n▓▓░░░░░░░░░░░░░ 15%     \n2 0 2 0\n\nYou can add this bot to a channel as well, and it will post progress bar there."/utf8>>, Res).

should_reply_with_progress_bar_on_progress(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/progress">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"▓▓░░░░░░░░░░░░░ 15%     \n2 0 2 0"/utf8>>, Res).

should_reply_with_progress_bar_on_progress_in_channel(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        channel_message(-1111111, <<"/progress">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"▓▓░░░░░░░░░░░░░ 15%     \n2 0 2 0"/utf8>>, Res).

should_reply_with_supported_commands_on_help(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/help">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"Bot sends the year progress bar. The following commands are supported:\n/start - start the bot\n/progress - show today's progress of the year\n/help - this message">>, Res).

should_reply_with_supported_commands_on_help_in_channel(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        channel_message(-1111111, <<"/help">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"Bot sends the year progress bar. The following commands are supported:\n/start - start the bot\n/progress - show today's progress of the year\n/help - this message">>, Res).

should_reply_with_200_dont_know_emoji_on_unknown_command(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        json_message(1111111, <<"/unknown">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"🤷‍♂️"/utf8>>, Res).

should_reply_with_200_ignore_on_unknown_command_in_channel(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        channel_message(-1111111, <<"/unknown">>)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_body("", Res).

should_reply_with_200_dont_know_emoji_on_unhandled_message(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        audio_message(1111111)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_json_value(<<"method">>, <<"sendMessage">>, Res),
    ?assert_json_value(<<"text">>, <<"🤷‍♂️"/utf8>>, Res).

should_reply_with_200_ignore_on_unhandled_message_in_channel(Config) ->
    Res = ?perform_post(
        ?config(bot_endpoint_url, Config),
        [{<<"content-type">>, <<"application/json">>}],
        channel_audio_message(-1111111)
    ),
    ?assert_status(200, Res),
    ?assert_header_value("content-type", "application/json", Res),
    ?assert_body("", Res).

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

audio_message(ChatId) ->
    <<<<"{
        \"message\":{
            \"chat\":{
                \"last_name\":\"Test Lastname\",
                \"id\":">>/binary, (list_to_binary(io_lib:format("~w", [ChatId])))/binary, <<",
                \"first_name\":\"Test\",
                \"username\":\"Test\"
            },
            \"message_id\":1365,
            \"from\":{
                \"last_name\":\"Test Lastname\",
                \"id\":1111111,
                \"first_name\":\"Test Firstname\",
                \"username\":\"Testusername\"
            },
            \"voice\": {
                \"file_id\": \"AwADBAADbXXXXXXXXXXXGBdhD2l6_XX\",
                \"duration\": 5,
                \"mime_type\": \"audio/ogg\",
                \"file_size\": 23000
            }
        }
    }">>/binary>>.

channel_message(ChatId, Command) ->
    <<<<"{
        \"channel_post\": {
            \"message_id\":3,
            \"chat\":{
                \"id\":">>/binary, (list_to_binary(io_lib:format("~w", [ChatId])))/binary, <<",
                \"title\":\"\ud83e\udd37\u200d\u2642\ufe0f\",
                \"type\":\"channel\"
            },
            \"date\":1579557751,
            \"text\":\"">>/binary, (Command)/binary ,<<"\",
            \"entities\":[{
                \"offset\":0,
                \"length\":15,
                \"type\":\"bot_command\"
            }]
        }
    }">>/binary>>.

channel_audio_message(ChatId) ->
    <<<<"{
        \"channel_post\":{
            \"chat\":{
                \"id\":">>/binary, (list_to_binary(io_lib:format("~w", [ChatId])))/binary, <<",
                \"title\":\"Test\",
                \"type\":\"channel\"
            },
            \"message_id\":1365,
            \"voice\": {
                \"file_id\": \"AwADBAADbXXXXXXXXXXXGBdhD2l6_XX\",
                \"duration\": 5,
                \"mime_type\": \"audio/ogg\",
                \"file_size\": 23000
            }
        }
    }">>/binary>>.
