#!/bin/sh

# The $NAME variable is passed from sketchybar and holds the name of
# the item invoking this script:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

local_time=$(date '+%b %d %H:%M:%S')
new_york_time=$(TZ=America/New_York date '+%H:%M')
utc_time=$(TZ=UTC date '+%H:%M')

sketchybar --set $NAME label="$local_time | 🗽$new_york_time | 🌍$utc_time"

