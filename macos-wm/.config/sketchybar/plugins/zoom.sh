#!/bin/sh

# Launching https://github.com/Jean-Tinland/simple-bar/blob/master/lib/scripts/zoom-mute-status.applescript

mic_status="$(timeout -k 10 5 osascript $HOME/.config/sketchybar/plugins/zoom-mute-status.applescript)"


LABEL="" # "Zoom"
ICON=""
COLOR="0x000000"

case $mic_status in
  "on")
    ICON="🎙️"
    ;;
  "off")
    ICON=""
    COLOR="0xfd6d00"
    ;;
  "not_running")
    ICON="🙅"
    LABEL="Zoom not running"
    COLOR="0xfd6d00"
    ;;
esac

sketchybar --set $NAME icon="$ICON" label="$LABEL" colo=$COLOR
