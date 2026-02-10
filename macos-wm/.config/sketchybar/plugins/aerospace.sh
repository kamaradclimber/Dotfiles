#!/usr/bin/env bash

# Optimized version - only queries the focused workspace
# This reduces 10 aerospace list-windows calls to just 1 per workspace change

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    # Only check windows for the focused workspace
    windows=$(aerospace list-windows --workspace "$1" --json | jq -r '.[] | select(."window-title"!="") | ."app-name"')

    if [[ "$windows" == "" ]]; then
        sketchybar --set $NAME drawing=off
    else
        sketchybar --set $NAME drawing=on
    fi

    sketchybar --set $NAME background.drawing=on
else
    # For non-focused workspaces, keep them visible without checking
    sketchybar --set $NAME drawing=on background.drawing=off
fi
