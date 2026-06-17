#!/bin/sh

# Some events send additional information specific to the event in the $INFO
# variable. E.g. the front_app_switched event sends the name of the newly
# focused application in the $INFO variable:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

if [ "$SENDER" = "front_app_switched" ] || [ "$SENDER" = "forced_update" ]; then
  if [ "$SENDER" = "front_app_switched" ]; then
    focused="$INFO"
  else
    focused=$(aerospace list-windows --workspace focused --json | jq -r '.[] | select(."window-title"!="") | ."app-name"' | head -1)
  fi
  list=$(aerospace list-windows --workspace focused --json | jq -r '.[] | select(."window-title"!="") | ."app-name"' | grep -v "$focused" | tr '\n' '|')
  sketchybar --set $NAME label="$list *$focused"
fi
