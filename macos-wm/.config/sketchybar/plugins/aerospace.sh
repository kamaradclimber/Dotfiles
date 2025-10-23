#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

windows=$(aerospace list-windows --workspace "$1" --json | jq -r '.[] | select(."window-title"!="") | ."app-name"')

if [[ "$windows" == "" ]]; then
  sketchybar --set $NAME drawing=off
else
  sketchybar --set $NAME drawing=on
fi

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME drawing=on
    sketchybar --set $NAME background.drawing=on
else
    sketchybar --set $NAME background.drawing=off
fi
