#!/usr/bin/env bash

# Only the focused workspace handler does work — all others skip.
# This means one aerospace call per workspace_change event regardless of space count.
[ "$1" != "$FOCUSED_WORKSPACE" ] && exit 0

non_empty=$(aerospace list-windows --all --format '%{workspace}' | sort -u)

spaces_file="/tmp/sketchybar_spaces"
[ ! -f "$spaces_file" ] && exit 0

args=()
while IFS= read -r sid; do
  [ -z "$sid" ] && continue
  if echo "$non_empty" | grep -qx "$sid"; then
    args+=(--set "space.$sid" drawing=on)
  else
    args+=(--set "space.$sid" drawing=off)
  fi
  if [ "$sid" = "$FOCUSED_WORKSPACE" ]; then
    args+=(background.drawing=on)
  else
    args+=(background.drawing=off)
  fi
done < "$spaces_file"

sketchybar "${args[@]}"
