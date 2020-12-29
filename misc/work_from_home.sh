#!/bin/bash

set -e

on_off=$1


last_name="Seux"
status_expiration=0 # does not expire
if [[ "$on_off" == "on" ]]; then
  last_name="$last_name [work-at-home]"
  emoji=":house:"
  echo -n "Activating work-from-home..."
elif [[ "$on_off" == "zoom" ]]; then
  emoji=":zoom:"
  status_expiration=$(date -d "now + 3 minutes" "+%s")
else
  echo -n "Disabling work-from-home...."
  emoji=":rocket:"
fi

source ~/.watson_env

curl -s -f -XPOST -H 'Content-Type: application/json' 'https://slack.com/api/users.profile.set' -H "Authorization: Bearer $SLACK_TOKEN" -d "{\"profile\": {\"first_name\": \"Grégoire\", \"last_name\": \"$last_name\", \"status_emoji\": \"$emoji\", \"status_expiration\": $status_expiration}}" > /tmp/work-from-home.result

if [[ "$(cat /tmp/work-from-home.result | jq .ok)" == "true" ]]; then
  echo "OK"
else
  echo "FAILED"
  cat /tmp/work-from-home.result
fi
