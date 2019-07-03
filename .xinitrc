#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#
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


xrdb -merge ~/.Xresources

#no beep ever!
xset -b

case "$(uname -n)" in
  "churchill")
    setxkbmap fr -variant bepo
    ;;
  "gagarine")
    setxkbmap fr -variant bepo
    ;;
esac


if [ -f $HOME/.Xmodmap ]; then
   xmodmap ~/.Xmodmap
 fi


if [ -f /usr/bin/numlockx ]; then
   numlockx &
fi

if [[ -d $HOME/.config/wallpapers ]]; then
  feh --no-fehbg --bg-scale /home/grego/.config/wallpapers/*
else
  echo "no wallpaper dir in $HOME/.config/wallpapers. Won't set any wallpaper"
fi

which synclient && synclient TapButton1=1
which synclient && synclient PalmDetect=1



xsetroot -cursor_name left_ptr
exec awesome
