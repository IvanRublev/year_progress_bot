#!/bin/bash
trap 'echo Deploy failed. An error occured while executing command at `basename $0`:${LINENO}.' ERR
set -e
type heroku >/dev/null 2>&1 || { echo >&2 "heroku cli is required."; exit 1; }

app='yrpb-bot'

echo "
=== Add PostreSQL database to the app
"
echo -n "Should we add the postgres addon to the app (y/n)? "
read answer
if [ "$answer" != "${answer#[Yy]}" ]; then
    if [[ $(heroku addons -a $app | grep postgresql) ]]; then
        echo "Already have database added."
    else
        heroku addons:create heroku-postgresql:hobby-dev -a $app --version=12
        heroku pg:diagnose -a $app
    fi
    echo "Connection DATABASE_URL:"
    heroku config:get DATABASE_URL -a $app
fi

branch='master'
if [[ "$(git branch | grep \* | cut -d ' ' -f2)" != "${branch}" ]]; then
    echo "We should be on branch ${branch} to continue. Config file commit is possible."
    exit 2
fi

echo "
=== Set environment variables for ${app} app with content of .env file and others
"
echo -n "This would cause currently running app to restart. Continue (y/n)? "
read answer
if [ "$answer" != "${answer#[Nn]}" ]; then
    exit 3
fi
heroku config:set -a $app $(cat ../.env | xargs)

echo "
Add database connection setting
"
DATABASE_URL=$(heroku config:get DATABASE_URL -a $app)
if [[ $DATABASE_URL =~ postgres:\/\/([^:]+):([^@]+)@([^:]+):([^\/]+)\/(.+) ]]; then
    heroku config:set -a $app PGSQL_HOST=${BASH_REMATCH[3]} PGSQL_PORT=${BASH_REMATCH[4]} PGSQL_USERNAME=${BASH_REMATCH[1]} PGSQL_PASSWORD=${BASH_REMATCH[2]} PGSQL_DATABASE=${BASH_REMATCH[5]}
else
    echo "Failed to parse DB connection string: $DATABASE_URL"
    exit 35
fi


echo "
=== Configure erlang buildpack
"
echo "
Add preffered OTP version
"
export $(cat ../.tool-versions | sed -e "s/ /=/g" | xargs)
preffered_otp='../.preferred_otp_version'
echo "${erlang}" > $preffered_otp

echo "
Add web dyno start command
"
release_app=$(basename $(cd .. && pwd))
procfile='../Procfile'
echo "web: _build/prod/rel/${release_app}/bin/${release_app} foreground" > $procfile

git add $preffered_otp $procfile
git commit --amend --no-edit || true

heroku buildpacks:set "https://github.com/ChrisWhealy/cf-buildpack-erlang" -a $app ||
heroku buildpacks:add --index 1 "https://github.com/peergradeio/heroku-buildpack-envvars.git" || true
# see bin/envvars

echo "
=== Deploy ${app} app
"
heroku apps:info -a $app
echo -n "This would push the local branch *${branch} to the remote gigalixir. Continue (y/n)? "
read answer
if [ "$answer" != "${answer#[Nn]}" ]; then
    exit 4 
fi

git push -f heroku $branch
