#!/usr/bin/sh

echo "Detecting if running a zoom meeting"

current_window=$(xdotool getactivewindow getwindowname)

current_window_class=$(xprop -id $(xdotool getactivewindow) | grep -i class | cut -d = -f2  | awk '{print $1}' | sed -re 's/"(.*)".*/\1/')

echo "Current window is: $current_window, with class $current_window_class"

function set_status {
  status=$1
  ssh g_seux@churchill.criteois.lan ".dotfiles/work_from_home.sh $status"
}

if [[ "$current_window" == "Zoom Meeting" ]]; then
  set_status "zoom"
elif [[ "$current_window_class" == "kitty" ]]; then
  set_status "terminal"
else
  echo "Not in a known type of window, will not do anything"
fi
