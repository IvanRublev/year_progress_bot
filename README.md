year_progress_bot
=====

[![Build Status](https://travis-ci.org/IvanRublev/year_progress_bot.svg?branch=master)](https://travis-ci.org/IvanRublev/year_progress_bot) ![Method TDD](https://img.shields.io/badge/method-TDD-blue) [![Uptime Robot ratio (7 days)](https://img.shields.io/uptimerobot/ratio/7/m784181817-0615d6ad21ff2a6bb33e2cac)](https://uptimerobot.com)

A Telegram bot that messages a progress bar for the current year daily.

Messages from the bot look like the following.
```
▓░░░░░░░░░░░░░░ 6%
2 0 2 0
```

Add bot to your Telegram app with https://t.me/yrpb_bot link.


Build
-----

You can install an Erlang environment to run the project on macOS using the asdf version manager:

  * Install https://asdf-vm.com/ runtime management tool with `brew install asdf`
  * Add it to the shell with
    ```
    echo -e '\n. $(brew --prefix asdf)/asdf.sh' >> ~/.bash_profile
    echo -e '\n. $(brew --prefix asdf)/etc/bash_completion.d/asdf.bash' >> ~/.bash_profile
    ```
  * Restart the terminal
  * Install asdf plugins:
    ```
    asdf plugin-add erlang
    asdf plugin-add rebar
    ```
  * Install Erlang/Rebar3 runtimes in project's directory with `cd year_progress_bot && asdf install` 

  * Install direnv and set it globally if wasn't done before
    + `asdf plugin-add direnv && asdf install direnv 2.20.0 && asdf global direnv 2.20.0`
    + Follow the [instructions to hook direnv](https://github.com/direnv/direnv/blob/master/docs/hook.md) into your SHELL

To build the application
  
  * Fetch dependencies with `rebar3 get-deps`

  * Build the application with `rebar3 compile`


Run locally
-----------

Make sure that a PostgresSQL instance is running on the local machine.

Create a `.env` file containing following configuration variables:

    TEL_TOKEN=a_ttoken
    TEL_HOST=api.telegram.org.host
    TEL_INTEGRATE=false
    TEL_BOT_NAME=yrpb_bot
    HOST=localhost
    WEBHOOK_PATH=/wh_path
    NOTIFIER_LOOP_PERIOD=60000
    COOKIE=a_cookie
    PORT=8080
    PGSQL_HOST=127.0.0.1
    PGSQL_PORT=25432
    PGSQL_USERNAME=postgres
    PGSQL_PASSWORD=postgres
    PGSQL_DATABASE=yrpb

Then run server with `rebar3 shell`


Deployment on Heroku
--------------------

Create `.env-prod` file contatining production values for keys shown above except `PGSQL_*`. And set `TEL_INTEGRATE=true`. Database configuration would be added automatically.

Install Heroku CLI and create an app.

Deploy with `cd deploy && ./deploy_heroku.sh`.
