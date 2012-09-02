#!/bin/bash

# Intervals in seconds
INTERVAL=10

# dzen2 theme
SEP='^p(5;-2)^ro(2)^p()'

# {{{ Date
DATE_PERIOD=3
DATE_COUNTER=$DATE_PERIOD;

fdate() {
    DATE=`date +'%H:%M'`
    PDATE="^fg(white)$DATE^fg()"
}
# }}}

# {{{ Package upgrades
PKG_PERIOD=360
PKG_COUNTER=$PKG_PERIOD

fpkg() {
    PKG=`pacman -Qu | wc -l`

    PPKG="PKG ^fg(green)$PKG^fg()"
}
# }}}

# {{{ Mails
MAIL_PERIOD=2
MAIL_COUNTER=0

fmail() {
    MAILS=`find ~/Mail/ -type f -wholename '*/INBOX/new/*' | wc -l`
    PMAIL="MAIL ^fg(green)$MAILS^fg()"
}
# }}}

READER_PERIOD=2
READER_COUNTER=0
freader() {
    READER=`find ~/Mail/ -type f -wholename '*/rss/new/*' | wc -l`
    PREADER="RSS ^fg(green)$READER^fg()"
}


# {{{ Hard disk
HDISK_PERIOD=6
HDISK_COUNTER=$HDISK_PERIOD

fhdisk() {
    DF=`df -hT`
    HDISK_ROOT=`echo "$DF" | awk '/\/$/ {print $5}'`

    PHDISK="/ ^fg(green)$HDISK_ROOT^fg()"
}
# }}}

TODOS_PERIOD=12
TODOS_COUNTER=11
todos() {
    if [ -f ~/todo ]
    then
        td=`grep  -c ^- ~/todo`
        TODOS="todos :^fg(green)$td^fg()"
    fi
}


while true; do
   if [ $DATE_COUNTER -ge $DATE_PERIOD ]; then
     fdate
     DATE_COUNTER=0
   fi

   if [ $PKG_COUNTER -ge $PKG_PERIOD ]; then
     fpkg
     PKG_COUNTER=0
   fi

   if [ $MAIL_COUNTER -ge $MAIL_PERIOD ]; then
     fmail
     MAIL_COUNTER=0
   fi
   if [ $READER_COUNTER -ge $READER_PERIOD ]; then
     freader
     READER_COUNTER=0
   fi

   if [ $HDISK_COUNTER -ge $HDISK_PERIOD ]; then
     fhdisk
     HDISK_COUNTER=0
   fi

   if [ $TODOS_COUNTER -ge $TODOS_PERIOD ]; then
     todos
     TODOS_COUNTER=0
   fi

   
   echo "  $SEP ${PHDISK} $SEP ${PMAIL} $SEP ${PREADER} $SEP ${PPKG} $SEP ${PDATE} $SEP ${TODOS}"
   
   DATE_COUNTER=$((DATE_COUNTER+1))
   CPU_COUNTER=$((CPU_COUNTER+1))
   PKG_COUNTER=$((PKG_COUNTER+1))
   MAIL_COUNTER=$((MAIL_COUNTER+1))
   READER_COUNTER=$((READER_COUNTER+1))
   HDISK_COUNTER=$((HDISK_COUNTER+1))
   TEMP_COUNTER=$((TEMP_COUNTER+1))
   TODOS_COUNTER=$((TODOS_COUNTER+1))
   
   sleep $INTERVAL
done 
