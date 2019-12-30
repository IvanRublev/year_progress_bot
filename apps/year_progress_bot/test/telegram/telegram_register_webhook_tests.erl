-module(telegram_register_webhook_tests).
-include_lib("eunit/include/eunit.hrl").

register_test_() ->
    {foreach,
     fun() ->
         meck:new(shotgun),
         meck:expect(shotgun, open, fun(_,_,_) -> {ok, 123} end),
         meck:expect(shotgun, get, fun(_,_) -> {ok, #{status_code => 200, body => <<"">>}} end),
         meck:expect(shotgun, close, fun(_) -> ok end),
         application:set_env([
            {year_progress_bot, [
                {tel_token, "TOKEN"},
                {tel_host, "HOST"},
                {host, "SELF.HOST"},
                {port, 8080},
                {webhook_path, "/af1a-c775"},
                {tel_integrate, true}
            ]}
         ], [{persistent, true}])
     end,
     fun(_) ->
         meck:unload(shotgun)
     end,
     [fun should_call_setHook_on_telegram_backend/1]}.

should_call_setHook_on_telegram_backend(_) ->
    telegram:register_webhook(),

    [?_assert(meck:called(shotgun, open, ["HOST", 443, https])),
    ?_assert(meck:called(shotgun, get, [123, "/botTOKEN/setWebhook?url=https%3A%2F%2FSELF.HOST%2Faf1a-c775&allowed_updates=%5B%22message%22%2C%22channel_post%22%5D"])),
    ?_assert(meck:called(shotgun, close, [123]))].
