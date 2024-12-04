#!/bin/sh

# The $SELECTED variable is available for space components and indicates if
# the space invoking this script (with name: $NAME) is currently selected:
# https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item

echo "name:’$NAME’, sid: ’$SID’" >> /tmp/toto
sketchybar --set $NAME background.drawing=$SELECTED


# This technique is not great because it does not display spaces that have a window if we’ve never been there since the window was added
# reason is that this script is invoked only when moving to/from a given space
count="$(yabai -m query --windows  | jq ".[] | select(.space == $SID) | length" | head -n1)"
if [[ "$count" -eq 0 ]]; then
  sketchybar --set $NAME drawing=off
else
  sketchybar --set $NAME drawing=on
fi
