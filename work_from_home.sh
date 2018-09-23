#!/bin/bash

set -e

on_off=$1


last_name="Seux"
if [[ "$on_off" == "on" ]]; then
  last_name="$last_name [work-at-home]"
  emoji=":house:"
  echo -n "Activating work-from-home..."
else
  echo -n "Disabling work-from-home...."
  emoji=":rocket:"
fi

source ~/.watson_env

curl -s -f -XPOST -H 'Content-Type: application/json' 'https://slack.com/api/users.profile.set' -H "Authorization: Bearer $SLACK_TOKEN" -d "{\"profile\": {\"first_name\": \"Grégoire\", \"last_name\": \"$last_name\", \"status_emoji\": \"$emoji\"}}" > /tmp/work-from-home.result

if [[ "$(cat /tmp/work-from-home.result | jq .ok)" == "true" ]]; then
  echo "OK"
else
  echo "FAILED"
  cat /tmp/work-from-home.result
fi
