#!/bin/bash
trap 'echo Build failed. An error occured while executing command at `basename $0`:${LINENO}.' ERR
set -e

type envsubst >/dev/null 2>&1 || { echo >&2 "
envsubst is requeired for this script. 
On macOS it can be installed with the following Homebrew command:
    
    brew install gettext && brew link --force gettext

"; exit 1; }

export $(cat ../.tool-versions | sed -e "s/ /=/g" | xargs)
envsubst < ./.travis.yml.src '$erlang' > ../.travis.yml

echo "
travis.yml
==========
"
cat ../.travis.yml

echo -n "
Amend updated travis.yml to last commit (y/n)? "
read answer
if [ "$answer" != "${answer#[Yy]}" ]; then
    git add ../.travis.yml
    git commit --amend --no-edit || true
fi
