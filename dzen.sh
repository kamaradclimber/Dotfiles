#!/bin/bash

# Intervals in seconds
INTERVAL=10
TICK=0

# dzen2 theme
SEP='^p(5;-2)^ro(2)^p()'

# {{{ Date
DATE_PERIOD=3

fdate() {
  DATE=`TZ='Europe/Paris' date +'%H:%M'`
  PDATE="^fg(white)$DATE^fg()"
}
# }}}

# {{{ Package upgrades
PKG_PERIOD=360

fpkg() {
  PKG=`which pacman > /dev/null && pacman -Qu | wc -l`

  PPKG="PKG ^fg(green)$PKG^fg()"
}
# }}}

# {{{ Mails
MAIL_PERIOD=2

fmail() {
  if [[ -d ~/Maildir/Gmail ]]; then
    GMAILS=`find ~/Maildir/Gmail -type f -wholename '*/INBOX/new/*' | wc -l`
  fi
  if [[ -d ~/Maildir/familleseux ]]; then
  FAMAILS=`find ~/Maildir/familleseux -type f -wholename '*/INBOX/new/*' |wc -l`
  fi
  if [[ -d ~/Maildir/Criteo ]]; then
  CMAILS=`find ~/Maildir/Criteo -type f -wholename '*/INBOX/new/*' |wc -l`
  ACMAILS=`find ~/Maildir/Criteo -type f -wholename '*/*/new/*' |wc -l`
  ACMAILS=$((ACMAILS - CMAILS))
  fi
  if [[ -d ~/Maildir ]]; then
    PMAIL="$SEP MAIL ^fg(green)$GMAILS $FAMAILS $CMAILS $ACMAILS^fg()"
  fi
}
# }}}

READER_PERIOD=2
freader() {
  READER=`find ~/Maildir/ -type f -wholename '*/rss/INBOX/new/*' | wc -l`
  PREADER="$SEP RSS ^fg(green)$READER^fg()"
}


# {{{ Hard disk
HDISK_PERIOD=6

fhdisk() {
  DF=`df -hT`
  HDISK_ROOT=`echo "$DF" | awk '/\/$/ {print $5}'`

  PHDISK="/ ^fg(green)$HDISK_ROOT^fg()"
}
# }}}

REQUESTS_PERIOD=12
requests() {
  if [ -f ~/todo ]
  then
    td=`grep  -c ^- ~/todo`
    exToDo=`grep ^- ~/todo | shuf -n 1 | cut -c -50 |sed 's/^- //'`
    REQUESTS="$SEP requests :^fg(green)$td^fg() $exToDo"
  fi
}

WIFI_PERIOD=2
wifi() {
  if [ -f /sbin/iwconfig ]
  then
    w=`/sbin/iwgetid -a wlan0 | grep -v '00:00:00:00:00:00' > /dev/null && /sbin/iwgetid -r wlan0`
    if [[ $w = "off/any" || $w = " " ]]; then
      WIFI=""
    else
      WIFI="$SEP $w"
    fi
  fi
}

BATTERY_PERIOD=1
battery() {
  if [ `which acpi` ]
  then
    pc=`acpi | grep Battery | cut -d ',' -f 2 | sed 's/ \|%//g'`
    color=green
    test $pc -lt 21 && color=red
    (acpi | grep -q Discharging) && charge=" ^fg(red)↓^fg()" || charge=" ^fg(green)↑^fg()"
    BATTERY="$SEP B :^fg($color)$pc%^fg()$charge"
    acpi -a | grep 'on-line' && test $pc -gt 99 && unset BATTERY
  fi
}

while true; do
  if [ $(expr $TICK % $DATE_PERIOD) -eq 0 ]; then
    fdate
  fi

  if [ $(expr $TICK % $PKG_PERIOD) -eq 0 ]; then
    fpkg
  fi

  if [ $(expr $TICK % $MAIL_PERIOD) -eq 0 ]; then
    fmail
  fi
  if [ $(expr $TICK % $READER_PERIOD) -eq 0 ]; then
    freader
  fi

  if [ $(expr $TICK % $HDISK_PERIOD) -eq 0 ]; then
    fhdisk
  fi

  if [ $(expr $TICK % $REQUESTS_PERIOD) -eq 0 ]; then
    requests
  fi

  if [ $(expr $TICK % $WIFI_PERIOD) -eq 0 ]; then
    wifi
  fi

  if [ $(expr $TICK % $BATTERY_PERIOD) -eq 0 ]; then
    battery
  fi


  echo "  $SEP ${PHDISK} ${PMAIL} ${PREADER} $SEP ${PPKG} $SEP ${PDATE} ${REQUESTS} ${BATTERY} ${WIFI}"

  TICK=$((TICK+1))


  sleep $INTERVAL
done
