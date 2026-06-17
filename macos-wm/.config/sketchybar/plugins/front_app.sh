#!/opt/homebrew/bin/bash

app_icon() {
  case "$1" in
    "Google Chrome"|"Chromium") printf '\uf268' ;;
    "Firefox") printf '\uf269' ;;
    "Safari") printf '\uf267' ;;
    "Arc") printf '\uf484' ;;
    "Slack") printf '\uf198' ;;
    "Zoom") printf '\uf03d' ;;
    "Visual Studio Code"|"Code") printf '\ue70c' ;;
    "Terminal"|"iTerm2"|"kitty"|"Kitty"|"Alacritty"|"WezTerm") printf '\uf120' ;;
    "Finder") printf '\uf07b' ;;
    "Spotify") printf '\uf1bc' ;;
    "Discord") printf '\ufaae' ;;
    "Telegram") printf '\uf2c6' ;;
    "Mail") printf '\uf0e0' ;;
    "Calendar") printf '\uf073' ;;
    "Notes") printf '\uf249' ;;
    "Messages") printf '\uf075' ;;
    "1Password"|"1Password 7") printf '\uf084' ;;
    "IntelliJ IDEA"|"GoLand"|"PyCharm"|"WebStorm"|"CLion"|"DataGrip"|"RubyMine") printf '\ue7b5' ;;
    "Xcode") printf '\uf8ff' ;;
    "System Preferences"|"System Settings") printf '\uf013' ;;
    "Activity Monitor") printf '\uf200' ;;
    "Notion") printf '\uf009' ;;
    "Obsidian") printf '\uf0a2' ;;
    *)               printf '%s' "$1" ;;
  esac
}

if [ "$SENDER" = "front_app_switched" ] || [ "$SENDER" = "forced_update" ]; then
  focused_window=$(aerospace list-windows --focused --json)
  focused_app=$(echo "$focused_window" | jq -r '.[0]."app-name"')
  focused_id=$(echo "$focused_window" | jq -r '.[0]."window-id"')

  focused_icon=$(app_icon "$focused_app")
  focused_title=$(echo "$focused_window" | jq -r '.[0]."window-title"' | cut -c1-30)

  others=""
  while IFS= read -r app; do
    [ -z "$app" ] && continue
    icon=$(app_icon "$app")
    others="${others:+$others }$icon"
  done <<WINLIST
$(aerospace list-windows --workspace focused --json | jq -r --argjson fid "$focused_id" '.[] | select(."window-id" != $fid) | select(."window-title"!="") | ."app-name"')
WINLIST

  sketchybar --set front_app_others label="$others" \
             --set front_app_focused label="$focused_icon $focused_title"
fi
