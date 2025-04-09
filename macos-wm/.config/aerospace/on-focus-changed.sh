#!/usr/bin/env bash

# this script should no longer be necessary because I’ve replaced the alt-tab behavior with "context" app
# but I’m keeping it for reference
# the reason to replace it was that this script is often too slow to execute (especially when I have a bazel command running)
exit 0

# immediately empty the file to avoid the case where I switch to another workspace and immediately press alt-tab.
# without this, it would switch me to the next window of the previous workspace leading to confusion
# this happens because 1) the script is not called immediately and 2) it takes time to execute
# the following command resolve the 2nd case
echo "empty" > /tmp/next_window_id

list=$(aerospace list-windows --workspace focused | awk '{print $1}')
current_window_id=$(aerospace list-windows --focused | awk '{print $1}')
next=$(echo -e "$list\n$list" | grep -A 1 $current_window_id | head -n2 | tail -n1)
echo "list: $list" >> /tmp/toto.log
echo "current_window_id: $current_window_id" >> /tmp/toto.log
echo "next: $next" >> /tmp/toto.log
echo $next > /tmp/next_window_id
