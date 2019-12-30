year_progress_bot
=====

A Telegram bot that messages a progress bar for the current year daily.

Messages from the bot look like the following.
```
▓░░░░░░░░░░░░░░ 6%
2 0 2 0
```

Build
-----

    $ rebar3 compile


Run locally
-----------

Make sure that a PostgresSQL instance is running on local machine.

Create a `.env` file containing following configuration variables:

    TEL_TOKEN=a_ttoken
    TEL_HOST=api.telegram.org.host
    TEL_INTEGRATE=false
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
