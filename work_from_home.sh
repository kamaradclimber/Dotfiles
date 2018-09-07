#!/bin/bash

set -e

on_off=$1


last_name="Seux"
if [[ "$on_off" == "on" ]]; then
  last_name="$last_name [work-from-home]"
  echo "Activating work-from-home"
else
  echo "Disabling work-from-home"
fi

source ~/.watson_env

curl -s -f -XPOST -H 'Content-Type: application/json' 'https://slack.com/api/users.profile.set' -H "Authorization: Bearer $SLACK_TOKEN" -d "{\"profile\": {\"first_name\": \"Grégoire\", \"last_name\": \"$last_name\"}}" | jq .ok
