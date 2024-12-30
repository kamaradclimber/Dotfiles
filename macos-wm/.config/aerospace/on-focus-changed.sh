#!/usr/bin/env bash

list=$(aerospace list-windows --workspace focused | awk '{print $1}')
current_window_id=$(aerospace list-windows --focused | awk '{print $1}')
next=$(echo -e "$list\n$list" | grep -A 1 $current_window_id | head -n2 | tail -n1)
echo "list: $list" >> /tmp/toto.log
echo "current_window_id: $current_window_id" >> /tmp/toto.log
echo "next: $next" >> /tmp/toto.log
echo $next > /tmp/next_window_id
