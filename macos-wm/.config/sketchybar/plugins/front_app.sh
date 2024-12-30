#!/bin/sh

# Some events send additional information specific to the event in the $INFO
# variable. E.g. the front_app_switched event sends the name of the newly
# focused application in the $INFO variable:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

if [ "$SENDER" = "front_app_switched" ]; then
  # default behavior (without aerospace):
  # sketchybar --set $NAME label="$INFO"
  #
  # we prefer to list all apps instead:
  list=$(aerospace list-windows --workspace focused --json | jq '.[]."app-name"' -r | grep -v "$INFO"| tr '\n' '|')
  sketchybar --set $NAME label="$list *$INFO"
fi
