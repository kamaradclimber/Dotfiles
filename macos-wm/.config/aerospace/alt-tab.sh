#!/usr/bin/env sh

# simple implementation of alt-tab
# sadly it does not display the list windows

# list=$(aerospace list-windows --workspace focused | awk '{print $1}')
# current_window_id=$(aerospace list-windows --focused | awk '{print $1}')
# # echo "Current window id: $current_window_id"
# # echo "List of windows: $list"
# 
# next=$(echo -e "$list\n$list" | grep -A 1 $current_window_id | head -n2 | tail -n1)
# # echo "Next window id: $next"
# aerospace focus --window-id $next

aerospace focus --window-id $(cat /tmp/next_window_id)
