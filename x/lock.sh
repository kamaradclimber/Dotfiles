#!/bin/bash

# to convert all jpg to png:
find $HOME/wallpapers/ -name "*.jpg" | while read f; do
  dst=$(echo "$f" | sed -re 's/jpg$/png/')
  if [[ ! -f "$dst" ]]; then
    echo "Converting $f to png"
    mogrify -format png "$f"
  fi
done
file=$(ls $HOME/wallpapers/*png| sort -R| head -n1)

revert() {
  xset dpms 0 0 3600
  pamixer --unmute
  # ~/.dotfiles/work_from_home.sh off
}
trap revert SIGHUP SIGINT SIGTERM
xset +dpms dpms 120 180 600
pamixer --mute
i3lock -i $file -n
revert
