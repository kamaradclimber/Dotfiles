#!/usr/bin/sh

echo "Detecting if running a zoom meeting"

current_window=$(xdotool getwindowfocus getwindowname)

echo "Current window is: $current_window"

if [[ "$current_window" == "Zoom" ]]; then
  ssh g_seux@churchill.criteois.lan '.dotfiles/work_from_home.sh zoom'
else
  echo "Not in a zoom meeting"
fi
