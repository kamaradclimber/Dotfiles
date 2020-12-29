#!/bin/bash

# to convert all jpg to png:
# find . -name "*.jpg" -exec mogrify -format png {} \;
file=$(ls $HOME/wallpapers/*png| sort -R| head -n1)

revert() {
  xset dpms 0 0 3600
  amixer sset Master unmute
  ~/.dotfiles/work_from_home.sh off
}
trap revert SIGHUP SIGINT SIGTERM
xset +dpms dpms 120 180 600
amixer sset Master mute
i3lock -i $file -n
revert
