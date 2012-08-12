#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

xset +fp /usr/share/fonts/local
xset fp rehash


feh --no-fehbg --bg-scale /home/grego/.config/wallpapers/*
xrdb -merge ~/.Xresources

#no beep ever!
xset -b

#~/.dotfiles/dzen.sh &
~/.dotfiles/set_dualscreen_on.sh &


xsetroot -cursor_name left_ptr
exec xmonad

