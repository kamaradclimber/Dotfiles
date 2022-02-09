#!/usr/bin/bash

# FIXME: https://gitlab.freedesktop.org/pulseaudio/pulseaudio/-/merge_requests/497/diffs
# once the above is released (pulseaudio 0.16.0, it should be much easier to parse output of pactl)

pactl  list sources | grep Name: | awk '{print $2}' | while read name; do
  echo "Toggling $name"
  pactl set-source-mute $name toggle;
done
