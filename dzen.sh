#!/bin/bash

# Intervals in seconds
INTERVAL=1

# dzen2 theme
BG='#000033'
#FG='#3333ff'
FG='#BA4DE2'
X=160
Y=0
FN='-*-Inconsolata-*-*-*-*-16-*-*-*-*-*-iso8859'
SEP='^p(5;-2)^ro(2)^p()'

# Gauges theme
GW=50
GFG='#669'
GH=8
GBG='#333'



# {{{ Date
DATE_PERIOD=30
DATE_COUNTER=$DATE_PERIOD;

fdate() {
    DATE=`date +'%H:%M'`
    PDATE="^fg(white)$DATE^fg()"
}
# }}}





# {{{ Package upgrades
PKG_PERIOD=3600
PKG_COUNTER=$PKG_PERIOD

fpkg() {
    PKG=`pacman -Qu | wc -l`

    PPKG="PKG ^fg(green)$PKG^fg()"
}
# }}}

# {{{ Mails
MAIL_PERIOD=20
MAIL_COUNTER=0

fmail() {
    MAILS=`find ~/Mail/ -type f -wholename '*/INBOX/new/*' | wc -l`
    #MAIL2=`find ~/Mail/ -type f -regex '*/.*/cur/.*2,[^S]*$' | wc -l`

    PMAIL="MAIL ^fg(green)$MAILS^fg()"
}
# }}}

READER_PERIOD=20
READER_COUNTER=0
freader() {
    READER=`find ~/Mail/ -type f -wholename '*/rss/new/*' | wc -l`
    PREADER="RSS ^fg(green)$READER^fg()"
}


# {{{ Hard disk
HDISK_PERIOD=60
HDISK_COUNTER=$HDISK_PERIOD

fhdisk() {
    DF=`df -hT`
    HDISK_ROOT=`echo "$DF" | awk '/\/$/ {print $5}'`

    PHDISK="/ ^fg(green)$HDISK_ROOT^fg()"
}
# }}}



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

   
   echo "  $SEP ${PHDISK} $SEP ${PMAIL} $SEP ${PREADER} $SEP ${PPKG} $SEP ${PDATE} "
   
   DATE_COUNTER=$((DATE_COUNTER+1))
   CPU_COUNTER=$((CPU_COUNTER+1))
   PKG_COUNTER=$((PKG_COUNTER+1))
   MAIL_COUNTER=$((MAIL_COUNTER+1))
   READER_COUNTER=$((READER_COUNTER+1))
   HDISK_COUNTER=$((HDISK_COUNTER+1))
   TEMP_COUNTER=$((TEMP_COUNTER+1))
   
   sleep $INTERVAL
done | dzen2 -ta l -e r -y $Y -x $X -fg $FG -bg $BG -fn $FN 
