#!/bin/sh

# Launching https://github.com/Jean-Tinland/simple-bar/blob/master/lib/scripts/zoom-mute-status.applescript

mic_status="$(timeout -k 10 5 osascript $HOME/.config/sketchybar/plugins/zoom-mute-status.applescript)"


ICON="Zoom"

case $mic_status in
  "on")
    LABEL="🎙️"
    ;;
  "off")
    LABEL=""
    ;;
esac

sketchybar --set $NAME icon="$ICON" label="$LABEL"
